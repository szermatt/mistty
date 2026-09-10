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

(require 'cl-lib)

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

(defun mistty--column-count ()
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

(defun mistty--cleanup-scrollback (start end)
  "Cleanup portions of the screen before transitioning to scrollback.

Cleanup means:
 - remove newlines marked \\='term-line-wrap between START and END.
 - remove trailing spaces, marked with \\='mistty-skip set to \\='trailing"
  (when (> end start)
    (let ((end (copy-marker end)))
      (unwind-protect
          (save-excursion
            (goto-char start)
            (while (and (> end (point))
                        (search-forward "\n" end 'noerror))
              (let ((nl (match-beginning 0)))
                (if (get-text-property nl 'term-line-wrap)
                    ;; If it's a line wrap delete it and don't worry about
                    ;; spaces; they're not trailing spaces.
                    (replace-match "" nil t)
                  ;; If it's a real newline, look for trailing spaces and
                  ;; delete them.
                  (let ((pos nl))
                    (while (and (eq ?  (char-before pos))
                                (eq 'trailing (get-text-property (1- pos) 'mistty-skip))
                                (> pos start))
                      (cl-decf pos))
                    (when (> nl pos)
                      (delete-region pos nl)))))))

        ;; help the garbage collector get rid of the marker
        (set-marker end nil)))))

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

(defun mistty--last-non-ws ()
  "Return the position of the last non-whitespace in the buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward "[:blank:]\n\r")
    (point)))

(defun mistty--cap-at-blank-end (pos)
  "Make sure POS is before the last non-empty character in the buffer."
  (min pos (mistty--blank-end-start)))

(defun mistty--blank-end-start ()
  "Return the position of the last non-empty character in the buffer.

This skips empty newlines at end, the final newline and trailing spaces
on the last line."
  (save-excursion
    (goto-char (point-max))
    (while (eq (char-before) ?\n)
      (goto-char (1- (point))))
    (while (and (not (bobp))
                (get-text-property (1- (point)) 'mistty-skip))
      (goto-char (1- (point))))
    (point)))

(defun mistty--has-text-properties (pos props)
  "Return non-nil if properties at POS include PROPS.

Return nil if PROPS is nil."
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

(defun mistty--fake-nl-p (&optional pos)
  "Check whether newline at POS is a fake newline.

POS defaults to the current point."
  (get-text-property (or pos (point)) 'term-line-wrap))

(defun mistty--real-nl-p (&optional pos)
  "Check whether char at POS is a real newline."
  (let ((pos (or pos (point))))
    (and (eq ?\n (char-after pos))
         (not (mistty--fake-nl-p pos)))))

(cl-defstruct (mistty--fifo
               (:constructor mistty--make-fifo ())
               (:conc-name mistty--fifo-))
  "A FIFO datastructure based on a doubly-linked list.

`mistty--fifo-enqueue' adds to the fifo. `mistty--fifo-dequeue'
removes and returns the oldest item in the fifo."
  ;; nodes of type (cons (cons ITEM OLDER) NEWER)
  newest
  oldest)

(defun mistty--fifo-empty-p (fifo)
  "Return non-nil if FIFO is empty."
  (null (mistty--fifo-oldest fifo)))

(defun mistty--fifo-clear (fifo)
  "Clear the FIFO."
  (setf (mistty--fifo-newest fifo) nil)
  (setf (mistty--fifo-oldest fifo) nil))

(defun mistty--fifo-enqueue (fifo item)
  "Add ITEM into the FIFO."
  (let* ((older (mistty--fifo-newest fifo))
         (node (cons (cons item older) nil)))
    (setf (mistty--fifo-newest fifo) node)
    (if older
        (setcdr older node)
      (setf (mistty--fifo-oldest fifo) node))))

(defun mistty--fifo-dequeue (fifo)
  "Remove the oldest entry from FIFO and return it.

Return nil if there are no entry."
  (when-let* ((oldest (mistty--fifo-oldest fifo)))
    (let ((item (caar oldest))
          (newer (cdr oldest)))
      (setf (mistty--fifo-oldest fifo) newer)
      (if newer
          (setcdr (car newer) nil)
        (setf (mistty--fifo-newest fifo) nil))

      item)))

(defun mistty--fifo-to-list (fifo)
  "Destructively convert FIFO into a list."
  (let ((list (mistty--fifo-oldest fifo)))
    (mistty--fifo-clear fifo)

    ;; Turn (cons (cons ITEM OLDER) NEWER)
    ;; into (cons ITEM NEWER)
    (let ((cur list))
      (while cur
        (setcar cur (caar cur))
        (setq cur (cdr cur))))

    list))

(provide 'mistty-util)

;;; mistty-util.el ends here
