;;; mistty-accum.el --- Pre-processing process filter -*- lexical-binding: t -*-

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
;; This file defines an accumulator, which receives data coming from
;; the process, processes-it and eventually send it to the term
;; process filter.

(require 'seq)
(require 'subr-x)
(require 'pcase)
(require 'oclosure)
(require 'cl-lib)
(require 'rx)

(require 'mistty-util)
(require 'mistty-log)

;;; Code:

(defconst mistty--max-delay-processing-pending-output 0.1
  "Limits how long to spend processing pending output.

When MisTTY calls `accept-process-output', Emacs will read data from the
process as long as there is some. If the process keeps sending data, the
whole Emacs process would freeze for that long. This limit must be kept
low or Emacs might become unresponsive when the process outputs data
continuously.")

(oclosure-define (mistty--accumulator
                  (:predicate mistty--accum-p))
  "Process Output Accumulator.

Use this function as process filter to accumulate and process data
before sending it to its destination filter.

Processors can be added to the accumulator to react to specific terminal
sequences being found in the flow before it gets to the real process
filter. See `mistty--accum-add-processor'

Post-processors can be added to the accumulator to react to changes made
by the real process filter. See
`mistty--accum-add-post-processor'.

Usage example:
  (let ((accum (mistty--make-accumulator #'real-process-filter)))
    (process-set-filter proc accum)
    (mistty--accum-add-processor accum ...)
    ...
"
  ;; The following slots are meant to be accessed only by
  ;; mistty-make-accumulator and its helper functions.

  ;; The real process filter; a function with arguments (proc data)
  (destination :mutable t)
  ;; Alist of (cons regexp (lambda (ctx str)))
  (processors :mutable t)
  ;; Set to non-nil after changing processors.
  (processors-dirty :mutable t)
  ;; Set of no-arg functions to call after calling process-filter.
  (post-processors :mutable t))

(defun mistty--accum-redirect (accum new-destination)
  "Change the process filter the accumulator sends output to."
  (setf (mistty--accumulator--destination accum) new-destination))

(defsubst mistty--accum-clear-processors (accum)
  "Remove all (post-)processors registered for ACCUM."
  (setf (mistty--accumulator--processors accum) nil)
  (setf (mistty--accumulator--processors-dirty accum) t)
  (setf (mistty--accumulator--post-processors accum) nil))

(defsubst mistty--accum-add-post-processor (accum post-processor)
  "Add POST-PROCESSOR to ACCUM.

POST-PROCESSOR must be a function that takes no argument. It is called
after the real process filter, once there are no remaining pending
processed data to send."
  (push post-processor (mistty--accumulator--post-processors accum)))

(defsubst mistty--accum-add-processor-1 (accum processor)
  "Register PROCESSOR, a `cl-accum-processor' in ACCUM.

This function is meant to be called by the macro
`mistty--accum-add-processor' and `mistty--accum-add-processor-lambda',
which see."
  (push processor
        (mistty--accumulator--processors accum))
  (setf (mistty--accumulator--processors-dirty accum) t))

(cl-defstruct (mistty--accum-processor
               (:constructor mistty--accum-make-processor)
               (:conc-name mistty--accum-processor-))
  "Process specific regexps in an output filter.

Register processors with `mistty--accum-add-processor'."
  ;; Regexps whose matches should be sent.
  regexp

  ;; Set of regexp that match incomplete sequences, without any final
  ;; $ or \\'. Incomplete sequences are held back until more data is
  ;; available.
  hold-back-regexps

  ;; Function accepting (ctx str) for all match of regexps.
  func)

(cl-defstruct (mistty--accum-ctx
               (:constructor mistty--accum-make-ctx)
               (:conc-name mistty--accum-ctx-))
  "Allow processors to communicate with the accumulator"
  ;; Flush accumulator (no-arg function).
  ;; Call it through mistty--accum-ctx-flush.
  flush-f
  ;; Send processed string to destination (single-arg function)
  ;; Call it through mistty--accum-ctx-push-down.
  push-down-f)

(defsubst mistty--accum-ctx-flush (ctx)
  "Flush accumulator from a processor.

Flushing from a processor sends all data processed so far to the
destination process filter. There's likely to be more data left
afterwards.

Post-processors are not run after every flush, but rather when all data
has been processed.

CTX is the context passed to the current processor.

If the process buffer is killed while handling the flush, the processor
is interrupted."
  (funcall (mistty--accum-ctx-flush-f ctx)))

(defsubst mistty--accum-ctx-push-down (ctx str)
  "Send STR to destination from a processor.

CTX is the context passed to the current processor."
  (funcall (mistty--accum-ctx-push-down-f ctx) str))

(defun mistty--make-accumulator (dest)
  "Make an accumulator that sends process output to DEST.

An accumulator is a function with the signature (PROC DATA) that is
meant to be used as process filter. It intercepts, buffers and
transforms process data before sending it to DEST.

DEST is the destination process filter function, with the same
signature (PROC DATA).

The return value of this type is also an oclosure of type
mistty--accum whose slots can be accessed."
  (let ((unprocessed (mistty--make-fifo))
        (processed (mistty--make-fifo))
        (incomplete nil)
        (overall-processor-regexp nil)
        (overall-hold-back-regexp nil)
        (processing-pending-output nil)
        (needs-postprocessing nil))
    (cl-labels
        ;; Collect all pending strings from FIFO into one
        ;; single string.
        ;;
        ;; The fifo is cleared.
        ;;
        ;; Return a single, possibly empty, string.
        ((fifo-to-string (fifo)
           (let ((lst (mistty--fifo-to-list fifo)))
             (pcase (length lst)
               (0 "")
               (1 (car lst))
               (_ (mapconcat #'identity lst)))))

         ;; Send all processed data to DEST.
         (flush (dest proc)
           (let ((data (fifo-to-string processed)))
             (unless (string-empty-p data)
               (funcall dest proc data)
               (setq needs-postprocessing t))))

         ;; Call post-processors after everything has been processed.
         (post-process (proc post-processors)
           (when needs-postprocessing
             (setq needs-postprocessing nil)
             (dolist (p (reverse post-processors))
               (mistty--with-live-buffer (process-buffer proc)
                 (funcall p)))))

         ;; Check whether the current instance should flush the data.
         ;;
         ;; The accumulator calls accept-process-output, which, since
         ;; the accumulator is the process filter, makes Emacs calls
         ;; accumulator recursively.
         ;;
         ;; Recursive calls should not flush, only the toplevel call
         ;; should.
         (toplevel-accumulator-p (proc)
           (if processing-pending-output
               (prog1 nil ; don't flush
                 (when (>= (time-to-seconds (time-subtract
                                             (current-time)
                                             processing-pending-output))
                           mistty--max-delay-processing-pending-output)
                   (throw 'mistty-stop-accumlating nil)))
             (prog1 t ; flush
               (unwind-protect
                   (progn
                     ;; accept-process-output calls the accumulator
                     ;; recursively as there's pending data.
                     (setq processing-pending-output (current-time))
                     (catch 'mistty-stop-accumlating
                       (accept-process-output proc 0 nil 'just-this-one)))
                 (setq processing-pending-output nil)))))

         ;; Build a new value for overall-processor-regexp.
         ;;
         ;; This is a big concatenation of all regexps in
         ;; PROCESSORS or nil if the alist is empty.
         ;;
         ;; WARNING: regexps must not define groups. TODO: enforce
         ;; this.
         (build-overall-processor-regexp (processors)
           (when processors
             (let ((index 0))
               (mapconcat
                (lambda (p)
                  (cl-incf index)
                  (format "\\(?%d:%s\\)"
                          index
                          (mistty--accum-processor-regexp p)))
                processors
                "\\|"))))

         ;; Build a regexp that detects incomplete terminal sequences
         ;; that hold be held back.
         (build-overall-hold-back-regexp (processors)
           (let ((regexps nil))
             (dolist (p processors)
               (dolist (r (mistty--accum-processor-hold-back-regexps p))
                 (cl-pushnew r regexps :test #'equal)))
             (when regexps
               (concat
                "\\(?:"
                (mapconcat #'identity regexps "\\|")
                "\\)\\'"))))

         ;; Build a new value for processor-vector.
         ;;
         ;; The vector contain the processor function whose indexes
         ;; correspond to groups in overall-processor-regexp.
         (build-processor-vector (processors)
           (vconcat (mapcar #'cdr processors)))

         ;; Add STR as processed string
         (push-down (str)
           (mistty--fifo-enqueue processed str))

         ;; Process any data in unprocessed and move it to processed.
         (process-data (dest proc processors)
           (while (not (mistty--fifo-empty-p unprocessed))
             (let ((data (concat incomplete (fifo-to-string unprocessed))))
               (setq incomplete nil)
               (if (and overall-hold-back-regexp
                        (string-match overall-hold-back-regexp data))
                   (setq incomplete (match-string 0 data)
                         data (substring data 0 (match-beginning 0)))
                 (pcase-setq `(,data . ,incomplete)
                             (mistty--split-incomplete-chars data)))
               (while (not (string-empty-p data))
                 (if (and overall-processor-regexp
                          (string-match overall-processor-regexp data))
                     (let* ((before (substring data 0 (match-beginning 0)))
                            (matching (substring
                                       data (match-beginning 0) (match-end 0)))
                            (processor (cl-loop for i from 1
                                                for p in processors
                                                thereis (when (match-beginning i)
                                                          p))))
                       (setq data (substring data (match-end 0))) ; next loop

                       (unless (string-empty-p before)
                         (mistty--fifo-enqueue processed before))
                       (mistty--with-live-buffer (process-buffer proc)
                         (catch 'mistty-abort-processor
                           (funcall
                            (mistty--accum-processor-func processor)
                            (mistty--accum-make-ctx
                             :flush-f (lambda ()
                                        (flush dest proc)
                                        (unless (buffer-live-p (process-buffer proc))
                                          (throw 'mistty-abort-processor nil)))
                             :push-down-f #'push-down)
                            matching))))
                   (mistty--fifo-enqueue processed data)
                   (setq data "")))))))

      ;; Build the accumulator as an open closure.
      (oclosure-lambda (mistty--accumulator (destination dest)
                                            (processors-dirty t))
          (proc data)
        (mistty--fifo-enqueue unprocessed data)
        (when (toplevel-accumulator-p proc)
          (when processors-dirty
            (setq overall-processor-regexp
                  (build-overall-processor-regexp processors))
            (setq overall-hold-back-regexp
                  (build-overall-hold-back-regexp processors))
            (setq processors-dirty nil))
          (process-data destination proc processors)
          (flush destination proc)
          (post-process proc post-processors))))))

(defun mistty--split-incomplete-chars (str)
  "Extract incomplete multibyte chars at the end of STR.

This function detects multibyte chars that couldn't be decoded at
the end of STR and splits it into a cons of complete string and
remaining bytes.

term.el is meant to do that, but it fails, because `char-charset'
alone doesn't behave the way term.el assumes (anymore?). This is
hopefully a temporary workaround."
  (let* ((len (length str))
         (end (substring str (max 0 (- len 8))))
         (decoded-end (decode-coding-string end locale-coding-system t))
         (undecoded-count 0)
         (i (1- (length decoded-end))))
    (while (and (>= i 0) (mistty--eight-bit-char-p decoded-end i))
      (cl-incf undecoded-count)
      (cl-decf i))
    (if (zerop undecoded-count)
        (cons str nil)
      (cons
       (substring str 0 (- len undecoded-count))
       (substring str (- len undecoded-count))))))

(defun mistty--eight-bit-char-p (str index)
  "Check whether char in STR at INDEX has been decoded."
  ;; logic taken from Emacs 29 describe-char
  (let ((c (aref str index)))
    (eq 'eight-bit
        (if (and (not enable-multibyte-characters) (>= c 128))
            'eight-bit
          (or (get-text-property index 'charset str)
              (char-charset c))))))

(provide 'mistty-accum)

;;; mistty-accum.el ends here
