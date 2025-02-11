;;; mistty-osc-colors.el --- Add support for OSC 10/11 to term-mode  -*- lexical-binding: t -*-

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
;; This file defines a handler for OSC 10 and 11 for querying
;; foreground and background colors.
;;

(require 'faces)

;;; Code:

(defun mistty-osc-query-color (code seq)
  (when (string= seq "?")
    (let* ((color (cond
                   ((string= "10" code)
                    (color-values (face-attribute 'term :foreground nil t)))
                   ((string= "11" code)
                    (color-values (face-attribute 'term :background nil t))))))
      (when color
        (process-send-string (get-buffer-process (current-buffer))
                             (format "\e]%s;rgb:%04x/%04x/%04x\e\\"
                                     code (nth 0 color) (nth 1 color) (nth 2 color)))))))

(provide 'mistty-osc-colors)

;;; mistty-osc-colors.el ends here
