;;; mistty-util.el --- random utils used by mistty -*- lexical-binding: t -*-

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
;; This file collects some random low-level utilities used by mistty
;; that usually have nothing to do with MisTTY itself.

;;; Code:

(defmacro mistty--with-live-buffer (buf &rest body)
  "Execute BODY with BUF enabled, if BUF is live.

The execution of BODY is silently skipped if BUF is not a live
buffer."
  (declare (indent 1))
  (let ((tempvar (make-symbol "buf")))
    `(let ((,tempvar ,buf))
       (when (buffer-live-p ,tempvar)
         (with-current-buffer ,tempvar
           ,@body)))))

(defun mistty--bol (pos &optional n)
  "Return the Nth beginning of line position at POS."
  (save-excursion
    (goto-char pos)
    (pos-bol n)))

(defun mistty--eol (pos &optional n)
  "Return the Nth end of line position at POS."
  (save-excursion
    (goto-char pos)
    (pos-eol n)))

(defun mistty--line-width ()
  "Return the column number at EOL."
  (save-excursion
    (goto-char (pos-eol))
    (current-column)))

(defun mistty--repeat-string (n segment)
  "Return a string containing SEGMENT N times."
  (let ((segment-len (length segment)))
    (if (= 1 segment-len)
        (make-string n (aref segment 0))
      (let ((str (make-string (* n segment-len) ?\ )))
        (dotimes (i n)
          (dotimes (j segment-len)
            (aset str (+ (* i segment-len) j) (aref segment j))))
        str))))

(defun mistty--safe-bufstring (start end)
  "Return buffer content from START to END, or an empty string.

Given an invalid buffer range, this alternative to
`buffer-substring-no-properties' returns an empty string instead
of failing."
  (let ((start (max (point-min) (min (point-max) start)))
        (end (max (point-min) (min (point-max) end))))
    (if (> end start)
        (buffer-substring-no-properties start end)
      "")))

(defun mistty--safe-pos (pos)
  "Make sure POS is within the range `point-min' to `point-max'."
  (min (point-max) (max (point-min) pos)))

(defun mistty--lines ()
  "Return list of markers to the beginning of the buffer's line."
  (save-excursion
    (goto-char (point-min))
    (let ((lines (list (point-min-marker))))
      (while (search-forward "\n" nil t)
        (push (point-marker) lines))
      (nreverse lines))))

(defun mistty--col (pos)
  "Return the column number at POS."
  (- pos (mistty--bol pos)))

(defun mistty--line (pos)
  "Return the line number at POS."
  (save-excursion
    (let ((count 0))
      (goto-char pos)
      (while (zerop (forward-line -1))
        (setq count (1+ count)))
      count)))

(defsubst mistty--nonempty-str-p (str)
  "Return non-nil if STR is a nonempty string."
  (and (stringp str)
       (length> str 0)))

(defun mistty--same-line-p (a b)
  "Return non-nil if positions A and B are on the same line."
  (= (mistty--bol a) (mistty--bol b)))

(defun mistty--remove-text-with-property (prop &optional pred)
  "Remove text with property PROP whose value matches PRED.

If PRED is unspecified, remove any PROP with a non-nil value."
  (let ((pos (point-min))
        (pred (or pred #'identity)))
    (while (setq pos (text-property-not-all pos (point-max) prop nil))
      (let ((next-pos (next-single-property-change pos prop nil (point-max))))
        (if (funcall pred (get-text-property pos prop))
            (delete-region pos next-pos)
          (setq pos next-pos))))))

(defun mistty--remove-fake-newlines (start end)
  "Remove newlines marked \\='term-line-wrap between START and END."
  (save-excursion
    (goto-char start)
    (while (search-forward "\n" end 'noerror)
      (when (get-text-property (1- (point)) 'term-line-wrap)
        (setq end (1- end))
        (replace-match "" nil t)))))

(defun mistty-self-insert-p (key)
  "Return non-nil if KEY is a key that is normally just inserted."
  (and (length= key 1)
       (characterp (aref key 0))
       (not (string= "Cc"
                     (get-char-code-property (aref key 0)
                                             'general-category)))))

(defun mistty--truncate-string (str n)
  "Truncate STR to N chars, if necessary.

Add an ellipsis if STR is truncated."
  (if (length> str n)
      (concat (substring str 0 n) "...")
    str))

(defun mistty--has-text-properties (pos props)
  "Return non-nil if properties at POS include PROP.

Return nil if PROP is nil."
  (when props
    (let ((actual (text-properties-at pos))
          (current props)
          (ret t))
      (while (and current ret)
        (let ((p (pop current))
              (val (pop current)))
          (unless (equal val (plist-get actual p))
            (setq ret nil))))
      ret)))

(defun mistty--count-lines (beg end &optional pred)
  "Return number of newlines between BEG and END, including invisible ones.

If END < BEG, return a negative number.

If specified, PRED is a function that takes a position pointing to a
newline and return non-nil if that newline should be counted."
    (save-excursion
    (let ((count 0)
          (sign (if (> beg end) -1 1))
          (beg (min beg end))
          (end (max beg end))
          (pred (or pred (lambda (_) t))))
      (goto-char beg)
      (while (search-forward "\n" end 'noerror)
        (when (funcall pred (match-beginning 0))
          (cl-incf count)))

      (* sign count))))

(provide 'mistty-util)

;;; mistty-util.el ends here
