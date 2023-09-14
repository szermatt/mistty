;;; mistty-reverse-input-decode-map.el --- generate keymaps for mistty-term.el -*- lexical-binding: t -*-

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
;; This is a development utility for changing or updating
;; `mistty-term-key-map', defined in mistty-term.el.
;;
;; This is not part of MisTTY's distribution.

(require 'seq)
(require 'help-fns)

;;; Code:

;;;###autoload
(defun mistty-reverse-input-decode-map (map)
  "Generate elisp code for `mistty-term-key-map' given a MAP.

This command reverses input-decode maps, such as the ones defined
in elisp/term/ and outputs a definition that's appropriate to use
as `mistty-term-key-map' into a buffer.

You might find it useful if you'd like MisTTY to generate a set
of keys from a different terminal than xterm."
  (interactive
   (let ((def (variable-at-point))
	 (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
	 accept choice)
     (setq accept
           (lambda (sym)
             (let ((resolved (and (symbolp sym)
                                  (boundp sym)
                                  (or (buffer-local-value sym orig-buffer)
                                      (symbol-value sym)))))
               (if (keymapp resolved) resolved))))
     (setq choice
           (completing-read
            (format-prompt "Reverse map " (funcall accept def))
            #'help--symbol-completion-table
            accept
            t nil nil
            (if (funcall accept def) (symbol-name def))))
     (list (if (equal choice "")
	       choice (funcall accept (intern choice))))))
  (with-current-buffer (get-buffer-create "*mistty-reverse-map*")
    (delete-region (point-min) (point-max))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (insert "(let ((map (make-sparse-keymap)))\n")
    (let ((start (point))
          (exists-table (make-hash-table :test #'equal)))
      (map-keymap
       (lambda (e b)
         (mistty--reverse-input-decode-map-1 e b [] exists-table))
       map)
      (sort-lines nil start (point)))
    (insert "\n    map)\n")
    (goto-char (point-min))))

(defun mistty--reverse-input-decode-map-1 (event-type binding prefix exists-table)
  "Recursive function to follow BINDINGS and output statements.

This function goes into a map, recursively, if necessary, until
it gets to a leaf binding, a binding of a key to a string.

EVENT-TYPE is the key of BINDING, BINDING is the current binding,
which can be a sequence to reverse or a map to enter. PREFIX
tracks the current key binding context when going into maps
recursively.

EXISTS-TABLE is a table of key bindings that have already been
output. If a decoder map contains more than one control sequence
that corresponds to a key, only the first one is output."
  (let ((full-event (vconcat prefix (vector event-type))))
    (cond
     ((keymapp binding)
      (map-keymap
       (lambda (e b)
         (mistty--reverse-input-decode-map-1 e b full-event exists-table))
       binding))
     ((functionp binding))
     ((sequencep binding) ; a key seq
      (let ((key (key-description binding)))
        (unless (gethash key exists-table)
          (puthash key t exists-table)
          (insert (format
                   "    (define-key map (kbd %S) \"%s\")\n"
                   key
                   (seq-mapcat #'mistty--char-string full-event 'string)))))))))

(defun mistty--char-string (c)
  "Format C for inclusion into a nice elisp string."
  (cond
   ((= c ?\e) "\\e")
   ((= c ?\t) "\\t")
   ((= c ?\a) "\\a")
   ((= c ?\d) "\\d")
   ((and (>= c ?\ ) (<= c 126) (make-string 1 c)))
   (t (format "\\x%2.2x" c))))

(provide 'mistty-reverse-input-decode-map)

;;; mistty-reverse-input-decode-map.el ends here
