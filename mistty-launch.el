;;; mistty-launch.el --- Convenience TRAMP-based launcher for MisTTY -*- lexical-binding: t -*-

(require 'pcmpl-unix)

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

;;;###autoload
(defun mistty-docker (&optional instance command other-window)
  "Connect to a docker instance in a MisTTY buffer.

This is a convenience function that uses TRAMP to connect to a
host and open a shell to it. For more control, run
C-u \\[mistty-create]. That'll allow you to setup any TRAMP
remote path you'd like without restrictions.

See Info Node `(mistty)Remote Shells with TRAMP' for details.

Note that docker instances often run older versions of Bash;
MisTTY will work on Bash versions < 5.1, with some limitations.
Additionally, for Bash 4.3 and earlier, you might have to set
`mistty-set-EMACS' to non-nil for directory tracking to work. You
can set it per instance as a connection-local variable. See Info
Anchor `(mistty)shells bash-dirtrack' for details."
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
