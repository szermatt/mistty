;;; mistty-accum-macros.el --- Macros for mistty-accum.el -*- lexical-binding: t -*-

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
;; This file defines macros built on top of mistty-accum.el so they
;; can be included with eval-when-compile and the macro code can
;; be left out of the compiled output.
;;
;; Usage:
;; (require 'mistty-accum)
;; (eval-when-compile (require 'mistty-accum-macros))

;;; Code:

(require 'rx)
(require 'cl-lib)
(require 'mistty-accum)

(cl-defmacro mistty--accum-add-processor-lambda (accum (ctx rx-regexp) &rest body)
  "Define a processor for processing RX-REGEXP in ACCUM.

BODY defines a lambda that takes as arguments CTX and any pcase-style
let capture defined in RX-REGEXP. Except for the let capture, RX-REGEXP
is written in the same language as `mistty--accum-add-processor'.

For example, the following defines a processor for \"\\e[0-9]?J\" that
makes the number available to the lamba as under the symbol num.

\(mistty--accum-add-processor-lambda accum
    (ctx \\='(seq ESC (let num Pn) ?J))
  ...)"
  (declare (indent 2))
  (let ((rx-regexp (mistty--accum-unquote-rx-regexp rx-regexp)))
    `(mistty--accum-add-processor
      ,accum
      (quote ,(mistty--accum-strip-let rx-regexp))
      (pcase-lambda (,ctx (rx ,(mistty--accum-expand-shortcuts rx-regexp)))
        ,@body))))

(defmacro mistty--accum-add-processor (accum rx-regexp processor)
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
by calling `mistty--accum-ctx-flush'."
  (let* ((rx-regexp (mistty--accum-expand-shortcuts
                     (mistty--accum-unquote-rx-regexp rx-regexp)))
         (regexp (rx-to-string rx-regexp 'no-group))
         (hold-back (mistty--accum-build-hold-back rx-regexp)))
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

(defun mistty--accum-unquote-rx-regexp (arg)
  "Check the type of ARG and return it unquoted.

The returned type is something that can be passed to
`mistty--accum-expand-shorts' then `mistty--accum-build-hold-back'."
  (pcase arg
    (`(quote ,elt) elt)
    ((or (pred stringp) (pred characterp)) arg)
    (_ (error "Macro expected quoted RX notation regexp, not %s"
              arg))))

(defun mistty--accum-strip-let (tree)
  "In-place removal of (let var rx-tree) from TREE."
  (pcase tree
    (`(let ,_ ,arg)
     (mistty--accum-strip-let arg))
    (`(,op . ,args)
     (cons
      op (mapcar #'mistty--accum-strip-let args)))
    (_ tree)))

(defun mistty--accum-expand-shortcuts (tree)
  "Expand special shortcuts into base RX-notation.

TREE might include notations inspired from
https://www.xfree86.org/current/ctlseqs.html#Definitions

The notation is documented in `mistty--accum-add-processor'.

A transformed version of TREE is returned."
  (pcase tree
    (`(,op . ,args)
     (cons
      op (mapcar #'mistty--accum-expand-shortcuts args)))
    ('ESC ?\e)
    ('BEL ?\a)
    ('CSI '(seq ?\e ?\[))
    ('OSC '(seq ?\e ?\]))
    ('DCS '(seq ?\e ?P))
    ('ST '(or ?\a (seq ?\e ?\\)))
    ('SP ?\ )
    ('TAB ?\t)
    ('CR ?\r)
    ('LF ?\n)
    ('Ps '(* (char "0-9")))
    ('Pm '(* (char "0-9;")))

    ;; Note on Pt: ECMA 48 8.3.89 only allows 0x08-0x0d 0x20-7e. That
    ;; would disallow all non-US-ASCII characters, often used in file
    ;; names, which would then need to be encoded. This would be
    ;; inconvenient and error-prone, so we disallow the US-ASCII
    ;; characters disallowed by ECMA 48 and allow all non-US-ASCII
    ;; chars (usually multibyte UTF-8).
    ('Pt '(* (not (char "\x00-\x07\x0e-\x1f\x7f"))))
    (_ tree)))

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

(provide 'mistty-accum-macros)

;;; mistty-accum-macros.el ends here
