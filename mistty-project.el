;;; mistty-project.el --- Project integration for mistty -*- lexical-binding: t -*-

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
;; This file defines a function `mistty-in-project' for starting a
;; MisTTY shell buffer in a project directory, tied to that project.
;;
;; For full integration, it is recommended to run
;; `mistty-project-init-kill-buffer' some time during initialization
;; so MisTTY buffers created by `mistty-in-project' can be killed just
;; like comint buffers are. See that function docstring.

(require 'project)

;;; Code:

;;;###autoload
(defun mistty-in-project ()
  "Start or go to a MisTTY buffer in the project's root directory.

If a MisTTY buffer already exists for running a shell in the
project's root, switch to it. If we're already on that buffer,
create a new buffer in that project, like `mistty' does.

Otherwise, create a new MisTTY shell buffer. With
\\[universal-argument] prefix arg, create a new shell buffer even
if one already exists."
  (interactive)
  (let* ((pr (project-current t))
         (bufs (project-buffers pr))
         (default-directory (project-root pr))
         (mistty-buf (mistty nil (lambda (buf) (memq buf bufs)))))
    (unless (memq mistty-buf bufs)
      (rename-buffer (generate-new-buffer-name
                      (project-prefixed-buffer-name "mistty"))))
    mistty-buf))

(defun mistty-project-init-kill-buffer ()
  "Have `project-kill-buffers' treat MisTTY buffers as comint.

If `project-kill-buffer-conditions' is configured to kill
`comint-mode' buffers, modify the configuratin to kill
`mistty-mode' buffers as well, as they're meant to be used the
same way.

Usage example:

  (use-package mistty-project
     :config
     (mistty-project-init-kill-buffer))"
  (let ((comint-cond
         (member '(derived-mode . comint-mode)
                 project-kill-buffer-conditions)))
    (setcdr comint-cond
            (cons '(derived-mode . mistty-mode)
                  (cdr comint-cond)))))

(provide 'mistty-project)

;;; mistty-project.el ends here
