;;; mistty-scrolline.el --- Library for working with scrollines -*- lexical-binding: t -*-

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
;; When writing a line on a that is larger than the terminal width,
;; that line will be displayed over several lines on the terminal.
;; When the terminal is resized, the line is redrawn and may take up
;; fewer or more lines on the terminal.
;;
;; In this project, the full, logical line is called a *scrolline*.
;; The lines displayed on the terminal are called *terminal lines*.
;; When rendering a large scrolline on a terminal, the terminal will
;; add *fake newlines*, that is \n with the text property
;; term-line-wrap set to t. A scrolline ends with a *real newline*, a
;; \n not marked with the text property term-line-wrap.
;;
;; As the terminal moves down, scrollines enter the scrollback area
;; and fake newlines are removed, letting Emacs render them normally.
;;
;; Scrolline numbering is absolute, that is, the number doesn't change
;; as the buffer is truncated. As a result, the first scrolline on a
;; buffer may not be scrolline 0.
;;
;; This file exposes a library that can be used to work with such
;; lines in a buffer. For this to work, the buffer must be initialized
;; and given both a marker and the scrolline number it correspond to.
;; This library uses this to manage absolute scrolline numbers.

;;; Code:

(defvar-local mistty--scrolline-home-mark nil
  "Marker known to be within scrolline `mistty--scrolline-home-num'.

Note that it's not necessarily at the start of a scrolline.

Normally initialized with `scrolline-init'and later on modified by
`scrolline-update` or by changing the marker or number separately.")

(defvar-local mistty--scrolline-home-num 0
  "Scrolline number of `mistty-scrolline-home-mark'.

Normally initialized with `scrolline-init'.")

(defun mistty--init-scrolline (marker number)
  "Initialize buffer to work with absolute scrolline number.

This is necessary when scrollines numbers don't just start at 0 at the
beginning of the buffer.

MARKER is used to mark a position that is at scrolline NUMBER. Note that
the marker is not copied, so changing it afterwards changes the origin."
  (setq mistty--scrolline-home-mark marker
        mistty--scrolline-home-num number))

(defun mistty--update-scrolline (pos number)
  "Redefine scrolline at POS to be NUMBER.

The marker position is moved to POS and is set to point to scrolline
NUMBER. If no marker was set for scrolline, a new one is created."
  (if mistty--scrolline-home-mark
      (move-marker mistty--scrolline-home-mark pos)
    (setq mistty--scrolline-home-mark (copy-marker pos)))
  (setq mistty--scrolline-home-num number))

(defun mistty--scrolline-at (pos)
  "Return the scrolline number at POS."
  (+ mistty--scrolline-home-num
     (mistty--count-scrollines
      (or mistty--scrolline-home-mark (point-min)) pos)))

(defsubst mistty--scrolline-at-point ()
  "Return the scrolline number at point."
  (mistty--scrolline-at (point)))

(defun mistty--find-scrolline (num)
  "Find scrolline number NUM on the current buffer.

Return nil if the scrolline isn't available, otherwise return the
position of the start of the scrolline."
  (save-excursion
    (goto-char (or mistty--scrolline-home-mark (point-min)))
    (if (= num mistty--scrolline-home-num)
        (mistty--scrolline-start-pos)
      (when (zerop (mistty--move-scrollines (- num mistty--scrolline-home-num)))
        (point)))))

(defun mistty--scrolline-start-pos ()
  "Return the position of the start of the current scrolline."
  (save-excursion
    (mistty--goto-scrolline-start)
    (point)))

(defun mistty--scrolline-end-pos ()
  "Return the position of the end of the current scrolline.

The returned position points to the newline at the end of the scrolline
or to EOB (point-max)."
  (save-excursion
    (mistty--goto-scrolline-end)
    (point)))

(defun mistty--scrolline-range ()
  "Return the start and end of the current scrolline."
  (cons (mistty--scrolline-start-pos)
        (mistty--scrolline-end-pos)))

(defun mistty--goto-scrolline-start ()
  "Go to the start of the current scrolline."
  (let (found)
    (while (and (setq found (search-backward "\n" nil 'noerror))
                (get-text-property (match-beginning 0) 'term-line-wrap)))
    (when found
      (goto-char (match-end 0)))))

(defun mistty--goto-scrolline-end ()
  "Go to the end of the current scrolline.

This positions the pointer on the real newline at the end of the
scrolline or at EOB."
  (let (found)
    (while (and (setq found (search-forward "\n" nil 'noerror))
                (get-text-property (match-beginning 0) 'term-line-wrap)))
    (when found
      (goto-char (match-beginning 0)))))

(defun mistty--move-scrollines (num)
  "Move NUM scrollines up or down.

If NUM is > 0, go down that many scrollines .
If NUM is < 0, go up that many scrollines .

Put point at the beginning of a scrolline.

Go as far up as possible and return the remaining number of scrollines
move to, normally 0, always >= 0."
  (cond
   ((> num 0)
    (while (and (> num 0)
                (search-forward "\n" nil 'noerror))
      (unless (get-text-property (match-beginning 0) 'term-line-wrap)
        (cl-decf num))))
   ((< num 0)
    (setq num (abs num))
    (while (and (> num 0)
                (search-backward "\n" nil 'noerror))
      (unless (get-text-property (match-beginning 0) 'term-line-wrap)
        (cl-decf num)))))

   (mistty--goto-scrolline-start)

   num)

(defun mistty--count-scrollines (beg end)
  "Count the number of scrollines between BEG and END.

If END < BEG, return a negative number."
  (save-excursion
    (let ((count 0)
          (sign (if (> beg end) -1 1))
          (beg (min beg end))
          (end (max beg end)))
      (goto-char beg)
      (while (search-forward "\n" end 'noerror)
        (unless (get-text-property (match-beginning 0) 'term-line-wrap)
          (cl-incf count)))

      (* sign count))))

(defun mistty--for-each-scrolline (func &optional beg end)
  "Call FUNC for each scroll for each scrolline from BEG to END.

FUNC is called for each scrolline with two arguments, the beginning and
the end position of the scrolline, including the final newline, if any.

Empty lines are reported as a single newline. If the buffer ends with a
newline, no final empty line is reported.

If BEG and END are in the middle of a line, the first line starts at BEG
and the last one ends at END; check with `mistty--scrolline-start-pos'
or `mistty--scrolline-end-pos' to get the real start and end in such
case."
  (let ((beg (or beg (point-min)))
        (end (or end (point-max))))
      (save-excursion
        (goto-char beg)
        (while (> end (point))
          (let ((pos (point)))
            (mistty--goto-scrolline-end)
            (when (eq ?\n (char-after))
              (goto-char (1+ (point))))
            (save-excursion
              (funcall func pos (min end (point)))))))))

(defun mistty--unwrap-lines (beg end)
  "Remove fake newlines from the region BEG to END.

BEG is inclusive, END exclusive.

Return the number of fake newlines that were removed."
  (let ((beg (min beg end))
        (end (max beg end))
        (removed 0))
    (when (> end beg)
      (save-excursion
        (goto-char end)
        (while (search-backward "\n" beg 'noerror)
          (when (get-text-property (match-beginning 0) 'term-line-wrap)
            (replace-match "")
            (cl-incf removed)))))

    removed))

(defun mistty--unwrapped-scrolline-text ()
  "Return the text within the current scrolline.

The line wrap are removed from the returned string."
  (string-replace "\n" ""
                  (buffer-substring-no-properties
                   (mistty--scrolline-start-pos)
                   (mistty--scrolline-end-pos))))

(defun mistty--unwrapped-scrolline-text-to-point ()
  "Return the text before point within the current scrolline.

The line wrap are removed from the returned string."
  (string-replace "\n" ""
                  (buffer-substring-no-properties
                   (mistty--scrolline-start-pos)
                   (point))))

(provide 'mistty-scrolline)

;;; mistty-scrolline.el ends here
