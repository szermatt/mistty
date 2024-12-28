;;; mistty-log.el --- Logging infrastructure for mistty.el. -*- lexical-binding: t -*-

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
;; This file contains helper utilities for mistty.el for logging
;; events, for debugging.
;;
;; To turn on logging in a buffer, call the command
;; `mistty-start-log'. This command creates a buffer that gets filled
;; with events reported to the function `mistty-log' on that buffer
;; and any other buffer on which `mistty-start-log' was called.

;;; Code:

(require 'ring)
(eval-when-compile
  (require 'cl-lib))

(defvar mistty-log-buffer nil
  "Buffer when log messages are directed, might not be live.")

(defvar mistty-log nil
  "Whether logging is enabled globally.

This is usually set by calling `mistty-start-log'.

Calling the function `mistty-log' is a no-op unless this is set.")

(defvar mistty-log-to-messages noninteractive
  "If non-nil, send log to messages instead of mistty log buffer.")

(defcustom mistty-backlog-size 0
  "Log entries to track when logging is disabled.

As many as `mistty-backlog-size' entries will be backfilled
by `mistty-start-log' when logging is enabled.

Setting this value allows turning on logging once something wrong
has happened."
  :group 'mistty
  :type '(integer))

(defvar-local mistty--backlog nil
  "If non-nil, a ring of `mistty--log' arguments.")

(defvar-local mistty--log-start-time nil
  "Base for logged times.

This is also the time the log buffer was created.")

(defface mistty-log-header-face '((t (:italic t)))
  "Face applied to the headers in `mistty-log' buffer.

This applies to log entries created by the function `mistty-log'."
  :group 'mistty)

(defface mistty-log-message-face nil
  "Face applied to the message in `mistty-log' buffer.

This applies to log entries created by the function `mistty-log'."
  :group 'mistty)

(defsubst mistty-log (str &rest args)
  "Format STR with ARGS and add them to the debug log buffer, when enabled.

String arguments are formatted and decoded to UTF-8, so terminal
communication can safely be sent out.

This does nothing unless logging is enabled for the current
buffer. It is usually enabled by calling mistty-start-log."
  (when (or mistty-log (> mistty-backlog-size 0))
    (mistty--log str args)))

(defun mistty-start-log ()
  "Enable logging and display the log buffer.

If logging is already enabled, just show the buffer."
  (interactive)
  (if (and mistty-log (buffer-live-p mistty-log-buffer))
      (switch-to-buffer-other-window mistty-log-buffer)
    (setq mistty-log t)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (ring-p mistty--backlog)
          (dolist (args (nreverse (ring-elements mistty--backlog)))
            (apply #'mistty--log args))
          (setq mistty--backlog nil))))
    (setq mistty--backlog nil)
    (mistty--log "Log enabled." nil)
    (when (buffer-live-p mistty-log-buffer)
      (switch-to-buffer-other-window mistty-log-buffer))))

(defun mistty-stop-log ()
  "Disable logging for all MisTTY buffers."
  (interactive)
  (when mistty-log
    (unwind-protect
        (when (buffer-live-p mistty-log-buffer)
          (mistty-log "Log disabled"))
      (setq mistty-log nil))))

(defun mistty-drop-log ()
  "Disable logging and kill the log buffer."
  (interactive)
  (setq mistty-log nil)
  (when (buffer-live-p mistty-log-buffer)
    (kill-buffer mistty-log-buffer))
  (setq mistty-log-buffer nil))

(defun mistty--log (format-str args &optional event-time)
  "Append FORMAT-STR and ARGS to the log.

This is normally called from `mistty-log', which first checks
whether logging or the backlog are enabled.

If logging is disabled, but the backlog is enabled, add a new
entry to the backlog.

If logging is enabled, add a new entry to the buffer
*mistty-log*.

EVENT-TIME is the time of the event to log, a float time, as
returned by `float-time'. It defaults to the current time.

Calling this function creates `mistty-log-buffer' if it doesn't
exit already."
  (let ((event-time (or event-time (float-time)))
        (calling-buffer (current-buffer)))
    (cond

     ;; not enabled; add to backlog
     ((and (not mistty-log) (> mistty-backlog-size 0))
      (ring-insert
       (or mistty--backlog
           (setq mistty--backlog (make-ring mistty-backlog-size)))
       (list format-str args event-time)))

     ;; enabled; interactive: log to buffer
     ((and mistty-log (not mistty-log-to-messages))
      (with-current-buffer
          (or (and (buffer-live-p mistty-log-buffer) mistty-log-buffer)
              (setq mistty-log-buffer
                    (get-buffer-create "*mistty-log*")))
        (add-hook 'kill-buffer-hook #'mistty-stop-log nil t)
        (setq-local window-point-insertion-type t)
        (goto-char (point-max))
        (insert-before-markers
         (propertize (mistty--log-header event-time calling-buffer)
                     'face 'mistty-log-header-face))
        (insert-before-markers
         (propertize
          (if args
              (apply #'format format-str (mapcar #'mistty--format-log-arg args))
            format-str)
          'face 'mistty-log-message-face))
        (insert-before-markers "\n")))

     ;; enabled; batch: output
     ((and mistty-log mistty-log-to-messages)
      (message "%s %s"
               (mistty--log-header event-time calling-buffer)
               (apply #'format format-str (mapcar #'mistty--format-log-arg args)))))))

(defun mistty--log-header (event-time buf)
  "Format a header for the macro `mistty-log'.

The header include EVENT-TIME and the name of BUF."
  (format "[%s] %3.3f "
          (buffer-name buf)
          (- event-time
             (or mistty--log-start-time
                 (setq mistty--log-start-time event-time)))))

(defun mistty--format-log-arg (arg)
  "Escape special characters in ARG if it is a string.

Return ARG unmodified if it's not a string."
  (if (stringp arg)
      (progn
        (setq arg (decode-coding-string arg locale-coding-system t))
        (seq-mapcat
         (lambda (elt)
           (if (and (characterp elt) (< elt 128))
               (text-char-description elt)
             (make-string 1 elt)))
         arg
         'string))
    arg))

(provide 'mistty-log)

;;; mistty-log.el ends here
