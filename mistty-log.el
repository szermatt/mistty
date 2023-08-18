;;; mistty-log.el --- Logging infrastructure for mistty.el. -*- lexical-binding: t -*-

(defvar mistty-log-buffer nil
  "Buffer when log messages are directed, might not be live.")

(defvar-local mistty-log-enabled nil
  "Whether logging is enabled on the current buffer.

Calling `mistty-log' is a no-op unless this is set.")

(defvar-local mistty--log-start-time nil
  "Base for logged times.

This is also the time the log buffer was created.")

(defface mistty-log-header-face '((t (:italic t)))
  "Face applied to the headers in `mistty-log' buffer."
  :group 'mistty)

(defface mistty-log-message-face nil
  "Face applied to the message in `mistty-log' buffer."
  :group 'mistty)

(defsubst mistty-log (str &rest args)
  "Format STR with ARGS and add them to the debug log buffer, when enabled.

String arguments are formatted and decoded to UTF-8, so terminal
communication can safely be sent out.

This does nothing unless logging is enabled for the current
buffer. It is usually enabled by calling mistty-start-log."
  (when mistty-log-enabled
    (mistty--log str args)))

(defun mistty-start-log ()
  "Enable logging for the current buffer and display that buffer.

If logging is already enabled, just show the buffer."
  (interactive)
  (if (and mistty-log-enabled mistty-log-buffer)
      (switch-to-buffer-other-window mistty-log-buffer)
    (setq mistty-log-enabled t)
    (mistty-log "Log enabled for %s" (buffer-name))
    (switch-to-buffer-other-window mistty-log-buffer)))

(defun mistty-stop-log ()
  "Disable logging for the current buffer."
  (interactive)
  (when mistty-log-enabled
    (when (buffer-live-p mistty-log-buffer)
      (mistty-log "Log disabled for %s" (buffer-name)))
    (setq mistty-log-enabled nil)))

(defun mistty-drop-log ()
  "Disable logging for all buffers and kill the log buffer."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when mistty-log-enabled
        (setq mistty-log-enabled nil))))
  (when (buffer-live-p mistty-log-buffer)
    (kill-buffer mistty-log-buffer)))

(defun mistty--log (str args)
  "Logging function, normally called from `mistty-log'.

Calling this function creates `mistty-log-buffer' if it doesn't
exit already."
  (with-current-buffer
      (or (and (buffer-live-p mistty-log-buffer) mistty-log-buffer)
          (setq mistty-log-buffer
                (get-buffer-create "*mistty-log*")))
    (setq-local window-point-insertion-type t)
    (unless mistty--log-start-time
      (setq mistty--log-start-time (float-time)))
    (let ((args (mapcar #'mistty--format-log-arg args)))
      (goto-char (point-max))
      (insert-before-markers
       (propertize
        (format "[%s] %3.3f "
                (buffer-name)
                (- (float-time) mistty--log-start-time))
        'face 'mistty-log-header-face))
      (insert-before-markers
       (propertize
        (apply #'format str args)
        'face 'mistty-log-message-face))
      (insert-before-markers "\n"))))

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
