;;; mistty-launch.el --- Convenience TRAMP-based launcher for MisTTY -*- lexical-binding: t -*-

(require 'pcmpl-unix)

(defvar mistty-ssh-history nil
  "History variable for host completion for mistty-ssh.")

;;;###autoload
(defun mistty-ssh (&optional host command other-window)
  "Open SSH running on HOST in a MisTTY buffer.

This is a convenience function that uses TRAMP to connect to a
host and open a shell to it. For more control, run
C-u \\[mistty-create]. That'll allow you to setup any TRAMP
remote path you'd like without restrictions.

See Info Node `(mistty)Remote Shells with TRAMP' for more
details."
  (interactive
   (list
    (completing-read "Host: " (pcmpl-ssh-hosts)
                     nil nil nil 'mistty-ssh-history)))
  (let ((default-directory (format "/ssh:%s:~" host)))
    (cd default-directory) ;; Connect, ask password
    (mistty-create command other-window)))
