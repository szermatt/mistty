;;; mistty-undo.el --- mistty undo utilities -*- lexical-binding: t -*-

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
;; This file collects undo-related utilities used by mistty.el.

;;; Code:

(require 'mistty-util)

(eval-when-compile
  (require 'cl-lib))

(cl-defstruct (mistty--undo-data
               (:constructor mistty--make-undo-data
                             (start &aux (inserted "")))
               (:conc-name mistty--undo-data-)
               (:copier nil))
  ;; the position of the beginning of a chain of undoable keys, never nil
  start

  ;; what was inserted so far after start, may be empty but never nil
  inserted

  ;; The type of the last key that was sent, if the last command was a
  ;; send key command for an undoable key.
  ;;
  ;; Can be nil, 'self-insert 'delete-char or 'backward-delete-char.
  key-type)

(defvar-local mistty--this-undo-data nil)

(defvar-local mistty--last-undo-data nil)

(defmacro mistty--inhibit-undo (&rest body)
  "Execute BODY with undo disabled."
  (let ((saved (make-symbol "saved-buffer-undo-list")))
    `(let ((,saved buffer-undo-list))
       (setq buffer-undo-list t)
       (unwind-protect
           (progn ,@body)
         (setq buffer-undo-list ,saved)))))

(defun mistty--pre-command-for-undo ()
  "Prepare undo data before a command.

This is meant to be called from the pre-command hook."
  (setq mistty--this-undo-data nil))

(defun mistty--post-command-for-undo ()
  "Finish undo data after a command.

This is meant to be called from the post-command hook."
  (setq mistty--last-undo-data mistty--this-undo-data))

(defun mistty--maybe-add-key-to-undo (n key cursor)
  "Add KEY N times to `buffer-undo-list'.

A key that's sent directly to the process connected to the
terminal isn't automatically added to the undo list. This
function does that, for `self-inserted keys', `delete-char', and
`backward-delete-char'."
  (when-let ((c (and (length= key 1) (elt key 0)))
             (key-type
              (cond
               ((mistty-self-insert-p key)
                'self-insert)
               ((eq ?\x7f c) ;; DEL
                'backward-delete-char)
               ((eq ?\C-h c) ;; BS
                'backward-delete-char)
               ((eq ?\C-d c) ;; C-d
                'delete-char))))
    (let ((n (or n 1))
          (undo-data (or mistty--last-undo-data
                         (mistty--make-undo-data cursor)))
          pos entry)
      (cl-assert (> n 0))

      (setq mistty--this-undo-data undo-data)

      ;; Amalgamate if the key type is the same as last time.
      (when (eq key-type (mistty--undo-data-key-type undo-data))
        (setq buffer-undo-list (cdr buffer-undo-list)))
      (setf (mistty--undo-data-key-type undo-data) key-type)

      ;; Use position from undo data, as when typing fast enough, or
      ;; on slow connection, cursor might not be up-to-date.
      (setq pos (+ (length (mistty--undo-data-inserted undo-data))
                   (mistty--undo-data-start undo-data)))

      (pcase key-type
        ('self-insert
         (setq entry (cons pos (+ pos n)))
         (setf (mistty--undo-data-inserted undo-data)
               (concat (mistty--undo-data-inserted undo-data)
                       (make-string n c))))

        ('delete-char
         (setq entry (cons (mistty--safe-bufstring pos (+ pos n)) pos)))

        ('backward-delete-char
         ;; Get the text to be deleted first from the inserted data,
         ;; then from the buffer at START, as the buffer content
         ;; might not have been updated when typing fast enough.
         (let* ((inserted (mistty--undo-data-inserted undo-data))
                (inserted-len (length inserted))
                (start (mistty--undo-data-start undo-data))
                (buffer-n (max 0 (- n inserted-len)))
                (inserted-n (min n inserted-len))
                (del-text (concat
                           (mistty--safe-bufstring (- start buffer-n)
                                                   start)
                           (substring inserted
                                      (- inserted-len inserted-n)
                                      inserted-len))))
           (setq entry (cons del-text (- pos (length del-text))))
           (setf (mistty--undo-data-inserted undo-data)
                 (substring inserted 0 (- inserted-len inserted-n))))))

      (when entry
        (push entry buffer-undo-list)))))

(provide 'mistty-undo)

;;; mistty-undo.el ends here
