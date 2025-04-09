;;; mistty-launch.el --- Convenience TRAMP-based launcher for MisTTY -*- lexical-binding: t -*-

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
;; This file defines convenience functions for starting mistty buffers
;; with TRAMP.

(require 'pcmpl-unix)

;;; Code:

(defvar mistty-ssh-history nil
  "History variable for host completion for `mistty-ssh'.")

(defvar mistty-docker-history nil
  "History variable for host completion for `mistty-docker'.")

(defvar mistty-docker-exe "docker"
  "Path to the docker command-line tool.

This is used by `mistty-docker'")

;;;###autoload
(defun mistty-ssh (&optional host command other-window)
  "Open SSH running on HOST in a MisTTY buffer.

This is a convenience function that uses TRAMP to connect to HOST and
executor COMMAND.

For more control, run \\[mistty-create] with an ; argument. That'll
allow you to setup any TRAMP remote path you'd like without
restrictions.

See Info Node `(mistty)Remote Shells with TRAMP' for more
details.

If OTHER-WINDOW is nil, execute the default action configured by
`display-comint-buffer-action'. If OTHER-WINDOW is a function, it is
passed to `pop-to-buffer` to be used as a `display-buffer' action. With
any other non-nil value, display the buffer in another window.

    You might prefer configuring `display-buffer-alist' for
    comint category buffers to get the exact behavior you want instead
    of passing OTHER-WINDOW."
  (interactive
   (list
    (completing-read "Host: " (pcmpl-ssh-hosts)
                     nil nil nil 'mistty-ssh-history)))
  (let ((default-directory (format "/ssh:%s:~" host)))
    (cd default-directory) ;; Connect, ask password
    (mistty-create command other-window)))

;;;###autoload
(defun mistty-docker (&optional instance command other-window)
  "Connect to a docker instance in a MisTTY buffer.

This connects to docker INSTANCE and executo COMMAND.

This is a convenience function that uses TRAMP to connect to a host and
open a shell to it. For more control, run \\[mistty-create] with an
argument. That'll allow you to setup any TRAMP remote path you'd like
without restrictions.

See Info Node `(mistty)Remote Shells with TRAMP' for details.

Note that docker instances often run older versions of Bash; MisTTY will
work on Bash versions < 5.1, with some limitations. Additionally, for
Bash 4.3 and earlier, you might have to set `mistty-set-EMACS' to
non-nil for directory tracking to work. You can set it per instance as a
connection-local variable. See Info Anchor `(mistty)shells
bash-dirtrack' for details.

If OTHER-WINDOW is nil, execute the default action configured by
`display-comint-buffer-action'. If OTHER-WINDOW is a function, it is
passed to `pop-to-buffer` to be used as a `display-buffer' action. With
any other non-nil value, display the buffer in another window.

    You might prefer configuring `display-buffer-alist' for comint
    category buffers to get the exact behavior you want instead of
    passing OTHER-WINDOW."
  (interactive
   (list
    (completing-read
     "Instance: "
     (string-split
      (shell-command-to-string
       (concat mistty-docker-exe " ps --format '{{.Names}}'"))
      "\n"  'omit-nulls "[[:space:]]+")
     nil nil nil 'mistty-docker-history)))
  (let ((default-directory (format "/docker:%s:~" instance)))
    (cd default-directory) ;; Connect, ask password
    (mistty-create command other-window)))

(provide 'mistty-launch)

;;; mistty-launch.el ends here
