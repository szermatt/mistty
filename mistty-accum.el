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
(require 'ring)

(require 'mistty-util)
(require 'mistty-log)

;;; Code:

(defconst mistty--max-accumulate-delay 0.1
  "Limits how long to spend processing immediately pending output.

When MisTTY calls `accept-process-output', Emacs will read data from the
process as long as there is some already available. If the process keeps
sending data, the whole Emacs process would freeze for that long. This
limit must be kept low or Emacs might become unresponsive when the
process outputs data continuously.")

(defconst mistty--max-accumulate-bytes (* 32 1024)
  "Limit how much immediately pending output to accumulate.

This limit is applied in addition to
`mistty--max-accumulate-delay' in case data comes in
quickly.")

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

  ;; mistty--accum-reset
  reset-f
  ;; mistty--accum-add-processor-1
  add-processor-f
  ;; mistty--accum-add-post-processor
  add-post-processor-f
  ;; mistty--accum-add-around-process-filter
  add-around-f)

(defsubst mistty--accum-reset (accum)
  "Reset ACCUM to its post-creation state.

All processors, post-processors, around destination registered to the
accumulator are cleared by this call."
  (funcall (mistty--accumulator--reset-f accum)))

(defsubst mistty--accum-add-post-processor (accum post-processor)
  "Add POST-PROCESSOR to ACCUM.

POST-PROCESSOR must be a function that takes no argument. It is called
after the real process filter, once there are no remaining pending
processed data to send."
  (funcall (mistty--accumulator--add-post-processor-f accum)
           post-processor))

(defsubst mistty--accum-add-around-process-filter (accum func)
  "Have FUNC wrap the process filter function called by ACCUM.

FUNC must be a function with the signature (NEXTFUNC), with
NEXTFUNC the function to call to run the process filter or the next
wrapper.

Note that FUNC is *not* called with any specific active buffer, just
like any process filter. The function should make sure to set the buffer
it needs and react to it having been killed. Further, the function
should avoid calling the function it wraps with any buffer active, as
that buffer might be killed during processing and break process
filtering."
  (funcall (mistty--accumulator--add-around-f accum) func))

(defsubst mistty--accum-add-processor-1 (accum processor)
  "Register PROCESSOR, a `cl-accum-processor' in ACCUM.

This function is meant to be called by the macro
`mistty--accum-add-processor' and `mistty--accum-add-processor-lambda',
which see."
  (funcall (mistty--accumulator--add-processor-f accum)
           processor))

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
  push-down-f
  ;; Return a small buffer of data processed before. Call it through
  ;; mistty--accum-ctx-look-back.
  look-back-f)

(defsubst mistty--accum-ctx-look-back (ctx)
  "Return a small buffer of data just processed from CTX.

This returns up to 8 bytes of processed data as it has been or will be
sent to the process filter.

When a processor is just called, that includes only data processed
before it. If the processor calls `mistty--accum-ctx-push-down', that
includes the data just pushed down as well."
  (funcall (mistty--accum-ctx-look-back-f ctx)))

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

DEST is the process filter function, with the same signature (PROC DATA)
that'll be eventually called.

The return value of this type is also an oclosure of type
mistty--accum whose slots can be accessed."
  (let ((post-processors nil)
        (around-process-filter nil)
        (processors nil)
        (processors-dirty nil)
        (unprocessed (mistty--make-fifo))
        (unprocessed-bytes 0)
        (processed (mistty--make-fifo))
        (look-back-ring (make-ring 8))
        (incomplete nil)
        (overall-processor-regexp nil)
        (overall-hold-back-regexp nil)
        (processing-pending-output nil)
        (needs-postprocessing nil))
    (cl-labels
        ;; Implement mistty--accum-reset.
        ((reset ()
           (setq post-processors nil)
           (setq processors nil)
           (setq around-process-filter nil)
           (setq processors-dirty t))

         ;; Add POST-PROCESSOR, a no-argument function to call after
         ;; processing data.
         (add-post-processor (post-processor)
           (push post-processor post-processors))

         ;; Add PROCESSOR, a mistty--accum-processor instance.
         (add-processor (processor)
           (push processor processors)
           (setq processors-dirty t))

         ;; Add FUNC, a wrapper function around the process filter
         ;; with signature (NEXT-FUNC).
         (add-around (func)
           (push func around-process-filter))

        ;; Collect all pending strings from FIFO into one
        ;; single string.
        ;;
        ;; The fifo is cleared.
        ;;
        ;; Return a single, possibly empty, string.
        (fifo-to-string (fifo)
           (let ((lst (mistty--fifo-to-list fifo)))
             (pcase (length lst)
               (0 "")
               (1 (car lst))
               (_ (mapconcat #'identity lst)))))

         ;; Send all processed data to the process filter.
         (flush (proc)
           (let ((data (fifo-to-string processed)))
             (run-wrapped around-process-filter
                          (lambda ()
                            (unless (string-empty-p data)
                              (funcall dest proc data)
                              (setq needs-postprocessing t))))))

         ;; Run WRAPPERS around FUNC.
         (run-wrapped (wrappers func)
           (dolist (wrapper (reverse wrappers))
             (setq func (let ((f func))
                          (lambda ()
                            (funcall wrapper f)))))
           (funcall func))

         ;; Call post-processors after everything has been processed.
         (post-process (proc)
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
                 (let ((duration (time-to-seconds (time-subtract
                                                   (current-time)
                                                   processing-pending-output))))
                   (when (or (>= duration
                                 mistty--max-accumulate-delay)
                             (>= unprocessed-bytes
                                 mistty--max-accumulate-bytes))
                     (throw 'mistty-stop-accumlating nil))))
             (prog1 t ; flush
               (unwind-protect
                   (progn
                     ;; accept-process-output calls the accumulator
                     ;; recursively as there's pending data.
                     (setq processing-pending-output (current-time))
                     (catch 'mistty-stop-accumlating
                       (accept-process-output proc 0 nil 'just-this-one)))
                 (setq processing-pending-output nil)))))

         ;; Update overall process regexps, if necessary.
         ;;
         ;; Call this before using either overall-processor-regexp or
         ;; overall-hold-back-regexp, in case processors has been
         ;; modified.
         (update-processor-regexps ()
           (when processors-dirty
             (setq overall-processor-regexp (build-overall-processor-regexp))
             (setq overall-hold-back-regexp (build-overall-hold-back-regexp))
             (setq processors-dirty nil)))

         ;; Build a new value for overall-processor-regexp.
         ;;
         ;; This is a big concatenation of all regexps in
         ;; PROCESSORS or nil if the alist is empty.
         ;;
         ;; WARNING: regexps must not define groups. TODO: enforce
         ;; this.
         (build-overall-processor-regexp ()
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
         (build-overall-hold-back-regexp ()
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
           (let ((len (length str)))
             (cl-loop for i
                      from (max 0 (- len (ring-size look-back-ring)))
                      below len
                    do (ring-insert look-back-ring (aref str i))))

           (mistty--fifo-enqueue processed str))

         ;; Return up to 8 bytes of raw data in a string from older to
         ;; newer.
         (look-back ()
           (let* ((len (ring-length look-back-ring))
                  (str (make-string len ?\0)))
             (cl-loop for i from 0 below len
                      do (setf (aref str i)
                               (ring-ref look-back-ring (- len i 1))))
             str))

         ;; Process any data in unprocessed and move it to processed.
         (process-data (proc)
           (while (not (mistty--fifo-empty-p unprocessed))
             (let ((data (concat incomplete (fifo-to-string unprocessed))))
               (setq unprocessed-bytes 0)
               (setq incomplete nil)
               (while (not (string-empty-p data))
                 (update-processor-regexps)
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
                         (push-down before))
                       (mistty--with-live-buffer (process-buffer proc)
                         (catch 'mistty-abort-processor
                           (funcall
                            (mistty--accum-processor-func processor)
                            (mistty--accum-make-ctx
                             :flush-f (lambda ()
                                        (flush proc)
                                        (unless (buffer-live-p (process-buffer proc))
                                          (throw 'mistty-abort-processor nil)))
                             :push-down-f #'push-down
                             :look-back-f #'look-back)
                            matching))))

                   (if (and overall-hold-back-regexp
                            (string-match overall-hold-back-regexp data))
                       (setq incomplete (match-string 0 data)
                             data (substring data 0 (match-beginning 0)))
                     (pcase-setq `(,data . ,incomplete)
                                 (mistty--split-incomplete-chars data)))
                   (push-down data)
                   (setq data "")))))))

      ;; Build the accumulator as an open closure.
      (oclosure-lambda (mistty--accumulator (reset-f #'reset)
                                            (add-post-processor-f #'add-post-processor)
                                            (add-processor-f #'add-processor)
                                            (add-around-f #'add-around))
          (proc data)
        (mistty--fifo-enqueue unprocessed data)
        (cl-incf unprocessed-bytes (length data))

        (when (toplevel-accumulator-p proc)
          (process-data proc)
          (flush proc)
          (post-process proc))))))

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
