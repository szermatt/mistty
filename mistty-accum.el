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
  "Register PROCESSOR, a `cl-accum-processor' in ACCUM."
  (push processor
        (mistty--accumulator--processors accum))
  (setf (mistty--accumulator--processors-dirty accum) t))

(cl-defmacro mistty--accum-add-processor-lambda (accum (ctx rx-regexp) &rest body)
  "Define a processor for processing RX-REGEXP in ACCUM.

BODY defines a lambda that takes as arguments CTX and any pcase-style
let capture defined in RX-REGEXP. Except for the let capture, RX-REGEXP
is written in the same language as `mistty--accum-add-processor'.

For example, the following defines a processor for \"\\e[0-9]?J\" that
makes the number available to the lamba as under the symbol num.

(mistty--accum-add-processor-lambda accum
    (ctx \\='(seq ESC (let num Pn) ?J))
  ...)"
  (declare (indent 2))
  (unless (and (listp rx-regexp) (eq 'quote (car rx-regexp)))
    (error "Regexp must be in quoted rx-notation, not: %S" (car rx-regexp)))

  `(mistty--accum-add-processor
    ,accum
    (quote ,(mistty--accum-strip-let (cl-copy-list (cadr rx-regexp))))
    (pcase-lambda (,ctx (rx ,(mistty--accum-expand-shortcuts (cadr rx-regexp))))
      ,@body)))

(defmacro mistty--accum-add-processor (accum rx-regexp processor &optional no-hold-back)
  "Register PROCESSOR in ACCUM for processing RX-REGEXP.

RX-REGEXP is a regexp in a restricted subset of the RX notation,
which supports:
  (seq ...)
  (or ...)
  (? ...)
  (* ...)
  (+ ...)
  (char ...)
  (not (char ...))
  char
  string

In addition, the following notation shortcuts are supported, freely
adapted from https://www.xfree86.org/current/ctlseqs.html#Definitions

 ESC \\e
 BEL \\a
 TAB \\t
 CR \\r
 LF \\n
 SP               (Space)
 CSI ESC [        (Control Sequence Introducer )
 OSC ESC ]        (Operating System Command)
 DSC ESC P        (Device Control String)
 Ps               (Single optional numeric parameter)
 Pm               (Multiple optional numeric parameters ;-separated)
 ST  BEL | ESC \\ (String Terminator)

RX-REGEXP must be a quoted list.

PROCESSOR must be a function with signature (CTX STR). With CTX a
`mistty--accum-ctx' instance and STR the terminal sequence that matched
the regexp. The processor is executed with the process buffer as current
buffer.

If PROCESSOR does nothing, the terminal sequence matching REGEXP is
simply swallowed. To forward or modify it, PROCESSOR must call
`mistty--accum-ctx-push-down'.

If PROCESSOR needs to check the state of the process buffer, it must
first make sure that that state has been fully updated to take into
account everything that was sent before the matching terminal sequence
by calling `mistty--accum-ctx-flush'.

If NO-HOLD-BACK is non-nil, don't generate hold-back regexps, which
means that the pattern won't match if it's split between several chunks.
Using is always a bug."
  (unless (and (listp rx-regexp) (eq 'quote (car rx-regexp)))
    (error "Regexp must be in quoted rx-notation, not: %S" rx-regexp))
  (let* ((rx-regexp (mistty--accum-expand-shortcuts (cadr rx-regexp)))
         (regexp (rx-to-string rx-regexp 'no-group))
         (hold-back (unless no-hold-back (mistty--accum-build-hold-back rx-regexp))))
    ;; canary: check the regexps at macro expansion time, so any error
    ;; is thrown early with extra information.
    (condition-case err
        (string-match regexp "")
      (invalid-regexp
       (signal 'invalid-regexp
               (format "%s [%S]" err regexp))))
    (let ((hold-back-str (mapconcat #'identity hold-back "\\|")))
      (condition-case err
          (string-match hold-back-str "")
        (invalid-regexp
         (signal 'invalid-regexp
                 (format "%s [%S]" err hold-back-str)))))

    `(mistty--accum-add-processor-1
      ,accum
      (mistty--accum-make-processor
       :regexp ,regexp
       :hold-back-regexps (list ,@hold-back)
       :func ,processor))))

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
               (when (and overall-hold-back-regexp
                          (string-match overall-hold-back-regexp data))
                 (setq incomplete (match-string 0 data))
                 (setq data (substring data 0 (match-beginning 0))))
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

(defun mistty--accum-strip-let (tree)
  "In-place removal of (let var rx-tree) from TREE."
  (pcase tree
    (`(let ,_ ,arg) arg)
    (`(,_ . ,args)
     (mistty--accum-transform-list args #'mistty--accum-strip-let)
     tree)
    (_ tree)))

(defun mistty--accum-expand-shortcuts (tree)
  "Expand in-place special shortcuts into base RX-notation.

TREE might include notations inspired from
https://www.xfree86.org/current/ctlseqs.html#Definitions

The notation is documented in `mistty--accum-add-processor'.

The transformed tree is returned."
  (pcase tree
    (`(,_ . ,args)
     (mistty--accum-transform-list args #'mistty--accum-expand-shortcuts)
     tree)
    ('ESC ?\e)
    ('BEL ?\a)
    ('CSI '(seq ?\e ?\[))
    ('OSC '(seq ?\e ?\]))
    ('DCS '(seq ?\e ?P))
    ('Ps '(* (char "0-9")))
    ('Pm '(* (char "0-9;")))
    ('Pt '(* (not (char "\x00-\x07\x0e-\x1f\x7f"))))
    ('ST '(or ?\a (seq ?\e ?\\)))
    ('SP ?\ )
    ('TAB ?\t)
    ('CR ?\r)
    ('LF ?\n)
    (_ tree)))

(defun mistty--accum-transform-list (lst func)
  "Apply FUNC on all car of LST."
  (let ((cur lst))
    (while cur
      (setcar cur (funcall func (car cur)))
      (setq cur (cdr cur)))))

(defun mistty--accum-build-hold-back (tree)
  "Build a set of hold-back regexps for TREE.

TREE must be written in a limited subset of the RX notation, documented
in `mistty--accum-add-processor'.

TREE must have been transformed with `mistty--accum-expand-shortcuts' first."
  (let ((collect (list "")))
    (cl-labels
        ((collect (tree)
           (pcase tree
             (`(seq . ,sub-trees)
              ;; Collect partial sequences into sub.
              (dolist (sub sub-trees)
                (collect sub)))

             (`(? ,sub)
              ;; The case without sub is already in collect.
              (collect sub))

             (`(or . ,subs)
              (let ((start collect)
                    (subcollects nil))

                (dolist (sub subs)
                  ;; match an incomplete sub-rx tree
                  (setq collect (list (car start)))
                  (collect sub)
                  (push (trim-list collect) subcollects))

                (setq collect start)
                (dolist (subcollect subcollects)
                  (addall subcollect))

                ;; match a complete a or a complete b
                (addone
                 (concat (car start)
                         (rx-to-string tree)))))

             ((or `(* ,sub) `(+ ,sub))
              ;; The case without sub is already in collect, add the
              ;; case with one or more sub, followed optionally by one
              ;; partial sub.
              (addone
               (concat (car collect)
                       (rx-to-string (list (car tree) sub) 'no-group)))

              (collect sub)
              ;; Remove the last where sub is complete; this is already
              ;; covered by (+ sub)
              (pop collect))

             ((and (pred characterp) c)
              ;; The case without the character is already in collect.
              ;; Add the case with the character.
              (addone
               (concat (car collect) (rx-to-string c))))

             ((and (pred stringp) str)
              (cl-loop for c across str
                       do (collect c)))

             (`(char ,set)
              ;; The case without the character set is already in
              ;; collect. Add the case with the character set.
              (addone
               (concat (car collect) "[" set "]")))

             (`(not (char ,set))
              ;; The case without the character set is already in
              ;; collect. Add the case with the character set.
              (addone
               (concat (car collect) "[^" set "]")))

             (_ (error "Unsupported RX notation: %s" tree))))
         (addall (lst)
           (dolist (elt lst)
             (addone elt)))
         (addone (elt)
           (cl-pushnew elt collect :test #'string=))
         (trim-list (lst)
           (cdr (butlast lst))))
      (collect tree)
      (pop collect)
      (cdr (nreverse collect)))))

(provide 'mistty-accum)

;;; mistty-accum.el ends here
