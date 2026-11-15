;;; mistty-term.el --- Extensions for term.el for MisTTY -*- lexical-binding: t -*-

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
;; This file collects helpers for mistty.el that deal with the
;; terminal buffer.

(require 'pcase)
(require 'subr-x)
(require 'cl-lib)
(require 'text-property-search)

(require 'mistty-custom)
(require 'mistty-scrolline)
(require 'mistty-util)
(require 'mistty-log)
(require 'mistty-accum)
(eval-when-compile
  (require 'mistty-accum-macros))
(require 'mistty-kbd)
(require 'mistty-osc7)
(require 'mistty-term-base)

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

(defvar-local mistty--point-marker nil
  "Marker (re)used by `mistty--sync-buffer' on Emacs 31 and later.")

(if (eval-when-compile (>= emacs-major-version 31))
    ;; Emacs 31 and later, using replace-region-contents
    (defun mistty--sync-buffers
        (source-buffer source-pos dest-buffer dest-pos)
      "Copy SOURCE-BUFFER from SOURCE-POS to eob to DEST-BUFFER.

The text and text properties of the region from DEST-POS to eob is
overwritten. This function attempt to keep
markers point and overlays as it rewrites the text."
      (mistty--with-live-buffer source-buffer
        (let ((properties (mistty--save-properties source-pos))
              (source-def (vector source-buffer source-pos (point-max))))
          (with-current-buffer dest-buffer
            (let ((point-marker (or mistty--point-marker
                                    (setq mistty--point-marker (make-marker)))))
              (set-marker point-marker (point))
              (set-marker-insertion-type point-marker nil)
              (replace-region-contents dest-pos (point-max) source-def 0.2)
              (goto-char point-marker)
              (move-marker point-marker nil))
            (mistty--restore-properties properties dest-pos)))))

  ;; Emacs 29 and 30, using replace-buffer-contents and narrowing.
  (defun mistty--sync-buffers
      (source-buffer source-pos dest-buffer dest-pos)
    "Copy SOURCE-BUFFER from SOURCE-POS to eob to DEST-BUFFER.

The text and text properties of the region from DEST-POS to eob is
overwritten. This function attempt to keep
markers point and overlays as it rewrites the text."
    (mistty--with-live-buffer source-buffer
      (save-restriction
        (narrow-to-region source-pos (point-max))
        (let ((properties (mistty--save-properties (point-min))))
          (with-current-buffer dest-buffer
            (let ((old-point (and (< (point) dest-pos) (point))))
              (save-restriction
                (narrow-to-region dest-pos (point-max))
                (replace-buffer-contents source-buffer 0.2)
                (mistty--restore-properties properties (point-min)))

              ;; If the point was outside the sync region, restore it,
              ;; as it has been moved by narrow-to-region . Otherwise,
              ;; trust replace-buffer-contents to do something
              ;; reasonable with it.
              (when old-point
                (goto-char old-point)))))))))

(defun mistty--save-properties (start)
  "Extracts the properties from START in the current buffer.

Returns a list of (BEG END PROPERTIES), ordered, with positions
relative to START."
  (let ((pos start) intervals)
    (while (< pos (point-max))
      (let ((props (text-properties-at pos))
            (last-pos pos))
        (setq pos (next-property-change pos nil (point-max)))
        (push `(,(- last-pos start) ,(- pos start) ,props)
              intervals)))

    intervals))

(defun mistty--restore-properties (intervals start)
  "Apply saved properties INTERVALS to the buffer at START.

This is the reverse operation of `mistty--save-properties'."
  (pcase-dolist (`(,beg ,end ,props) intervals)
    (set-text-properties (+ beg start) (+ end start) props)))

(provide 'mistty-term)

;;; mistty-term.el ends here
