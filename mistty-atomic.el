;;; mistty-atomic.el --- Atomic update support for mistty -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary:
;;
;; This file implements synchronized/atomic terminal updates based on
;; https://gist.github.com/christianparpart/d8a62cc1ab659194337d73e399004036
;;
;; It intercepts CSI ? 2026 h/l sequences and buffers terminal output
;; during atomic updates to prevent screen tearing.

;;; Code:

(require 'mistty-util)
(require 'mistty-accum)

(defcustom mistty-atomic-timeout-s 1.0
  "Maximum seconds to buffer during atomic update before forcing flush."
  :type 'number
  :group 'mistty)

;; State table: (expected-char next-state-if-match next-state-if-no-match)
(defconst mistty--atomic-state-table
  [("\e" 1 0)                   ;  0: looking for \e
   (?\[ 2 0)                    ;  1: saw \e
   (?? 3 0)                     ;  2: saw \e[
   (?2 4 0)                     ;  3: saw \e[?
   (?0 5 0)                     ;  4: saw \e[?2
   (?2 6 0)                     ;  5: saw \e[?20
   (?6 7 0)                     ;  6: saw \e[?202
   (?h 8 0)                     ;  7: saw \e[?2026
   ("\e" 9 8)                   ;  8: in atomic mode, looking for \e
   (?\[ 10 8)                   ;  9: saw \e during atomic
   (?? 11 8)                    ; 10: saw \e[
   (?2 12 8)                    ; 11: saw \e[?
   (?0 13 8)                    ; 12: saw \e[?2
   (?2 14 8)                    ; 13: saw \e[?20
   (?6 15 8)                    ; 14: saw \e[?202
   (?l 0 8)]                    ; 15: saw \e[?2026
  "State table for atomic update parser.")

(defun mistty--substring-fast (string start end)
  "Efficient substring that avoids copy when possible."
  (if (and (= start 0) (= end (length string)))
      string
    (substring string start end)))

(defun mistty--make-atomic-preprocessor (proc)
  "Create an atomic update preprocessor for process PROC.
Return (PREPROCESSOR . FORCE-EXIT), where PREPROCESSOR is a function
suitable as an accumulator preprocessor and FORCE-EXIT is a function of
no arguments that, when called, will make the preprocessor exit an
atomic region (if it's inside one) and dump any accumulated bytes ASAP."
  (let ((parse-state 0)
        (chunks nil)
        (timer nil)
        (force-exit nil))
    (cl-labels
        ((force-exit-atomic-mode ()
           (when (and (process-live-p proc) (>= parse-state 8))
             (setq force-exit t)
             (funcall (process-filter proc) proc "")))
         (push-down-atomic-chunks (next)
           (when timer
             (cancel-timer timer)
             (setq timer nil))
           (dolist (chunk (nreverse chunks))
             (funcall next chunk))
           (setq chunks nil))
         
         (process-data (next data)
           (when force-exit
             (when (>= parse-state 8)
               (push-down-atomic-chunks next)
               (setq parse-state 0))
             (setq force-exit nil))
           (let* ((last-flush-pos 0)
                  (pos 0)
                  (len (length data))
                  ;; Any sequence before this point will be found
                  ;; by regular string search and so would be
                  ;; pointless to scan for partial sequences.
                  (possible-end-pos (- len 7)))
             (cl-labels
                 ((flush (state pos)
                    (when (< last-flush-pos pos)
                      (let ((chunk (mistty--substring-fast data last-flush-pos pos)))
                        (if (< state 8)
                            ;; Not in atomic mode - send downstream
                            (funcall next chunk)
                          ;; In atomic mode - accumulate
                          (push chunk chunks)))
                      (setq last-flush-pos pos))))
               (while (< pos len)
                 (let ((old-state parse-state))
                   (or
                    ;; Fast search for whole escapes
                    (and (< pos possible-end-pos)
                         (or (= parse-state 0) (= parse-state 8))
                         (if-let* ((found-pos
                                    (string-search
                                     (if (= parse-state 0) "\e[?2026h" "\e[?2026l")
                                     data pos)))
                             ;; Found complete sequence - fast forward
                             (setq pos (+ found-pos 8)
                                   old-state (if (= parse-state 0) 7 15)
                                   parse-state (if (= parse-state 0) 8 0))
                           ;; No complete sequence - jump to near end
                           (setq pos (max pos possible-end-pos))))
                    ;; State machine for partial sequences near end
                    (when (< pos len)
                      (pcase-let*
                          ((`(,expected ,next-match ,next-no-match)
                             (aref mistty--atomic-state-table parse-state)))
                        (or
                         (and (stringp expected)
                              (let ((found-pos (string-search expected data pos)))
                                (if found-pos
                                    (setq pos (+ found-pos (length expected))
                                          old-state parse-state
                                          parse-state next-match)
                                  (setq pos len parse-state next-no-match))))
                         (and (= (aref data pos) expected)
                              (setq parse-state next-match pos (1+ pos)))
                         ;; No match: reset state and re-try this position
                         (setq parse-state next-no-match)))))
                     
                   ;; React to state changes
                   (cond
                     ((and (= old-state 7) (= parse-state 8)) ; Enter atomic
                      (mistty-log "ATOMIC REGION ENTER")
                      (flush 0 (max 0 (- pos 8)))
                      (cl-assert (null timer))
                      (cl-assert (null chunks))
                      (when timer
                        (cancel-timer timer)
                        (setq timer nil))
                      (setq timer (run-at-time mistty-atomic-timeout-s
                                               nil #'force-exit-atomic-mode)))
                      
                     ((and (= old-state 15) (= parse-state 0)) ; Leave atomic
                      ;; No need for an explicit (flush ...) here:
                      ;; it's fine to send the atomic-region suffix
                      ;; and subsequent non-atomic data to the next
                      ;; pipeline stage as one string; this way, we
                      ;; can avoid splitting the string.
                      (mistty-log "ATOMIC REGION NORMAL EXIT")
                      (push-down-atomic-chunks next)))))
                 
               ;; Final flush at end
               (flush parse-state len)))))
      (cons #'process-data #'force-exit-atomic-mode))))

(provide 'mistty-atomic)

;;; mistty-atomic.el ends here
