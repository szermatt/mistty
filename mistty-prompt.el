;;; mistty-prompt.el --- Prompt type MisTTY -*- lexical-binding: t -*-

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
;; This file define a prompt type, to be used in terminal
;; implementations.

(require 'cl-lib)
(require 'subr-x)
(require 'text-property-search)

;;; Code:

;; A detected prompt.
;;
;; This datastructure is shared between the work and term buffer and
;; uses scrollines as units.
;;
;; Use (mistty--register-prompt) to create and register new prompts.
(cl-defstruct (mistty--prompt
               (:constructor mistty--make-prompt))
  input-id

  ;; prompt source:
  ;;  - regexp
  ;;  - bracketed paste
  source

  ;; Non-nil once the prompt has been accepted by MisTTY
  realized

  ;; Start scrolline. Shouldn't be nil.
  start

  ;; End scrolline, or nil if prompt is open-ended.
  ;;
  ;; This is the first scrolline on which the prompt is *not* present, so
  ;; a single-line prompt starting at 10 would end at 11.
  end

  ;; Text of the prompt, used when source=regexp.
  text

  ;; Position of the start of the user input, if known.
  ;; This is a (cons scrolline column).
  user-input-start

  ;; The command executed, as reported by the shell (OSC 133)
  cmdline)

(defun mistty--prompt-contains (prompt scrolline)
  "Return non-nil if SCROLLINE is inside of PROMPT."
  (and (>= scrolline (mistty--prompt-start prompt))
       (or (null (mistty--prompt-end prompt))
           (< scrolline (mistty--prompt-end prompt)))))

(defun mistty--prompt-overlaps (prompt other)
  "Return non-nil if PROMPT range overlaps with OTHER's."
  (or (mistty--prompt-contains
       prompt (mistty--prompt-start other))
      (mistty--prompt-contains
       other (mistty--prompt-start prompt))))

(cl-defstruct (mistty--prompt-cell
               (:constructor mistty--make-prompt-cell
                             (&aux (counter 0))))
  current
  archive
  counter)

(defsubst mistty--get-prompt (cell)
  "Get current prompt from CELL."
  (mistty--prompt-cell-current cell))

(defsubst mistty--set-prompt (cell prompt)
  "Set current prompt of CELL to PROMPT.

PROMPT may be a `mistty--prompt' instance or nil.

The old prompt, if any, is pushed into the archive."
  (when prompt
    (setf (mistty--prompt-input-id prompt)
          (cl-incf (mistty--prompt-cell-counter cell))))
  (when-let* ((old (mistty--prompt-cell-current cell)))
    (push old (mistty--prompt-cell-archive cell)))
  (setf (mistty--prompt-cell-current cell) prompt))

(provide 'mistty-prompt)

;;; mistty-prompt.el ends here
