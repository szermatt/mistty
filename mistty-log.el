;;; mistty-log.el --- Logging infrastructure for mistty.el. -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-lib))

(defvar mistty-log-buffer nil
  "Buffer when log messages are directed, might not be live.")

(defvar-local mistty-log-enabled nil
  "Whether logging is enabled on the current buffer.

Calling `mistty-log' is a no-op unless this is set.")

(defvar mistty-backlog-size 0
  "Log entries to track when logging is disabled.

As many as `mistty-backlog-size' entries will be backfilled
by `mistty-start-log' when logging is enabled.

Setting this value allows turning on logging once something wrong
has happened.")

(defvar-local mistty--backlog nil
  "If non-nil, a mistty--backlog struct of `mistty--log' arguments.")

(defvar-local mistty--log-start-time nil
  "Base for logged times.

This is also the time the log buffer was created.")

(cl-defstruct (mistty--backlog
               (:constructor mistty--make-backlog
                             (size &aux (idx 0) (array (make-vector size nil))))
               (:conc-name mistty--backlog-)
               (:copier nil))
  ;; number of slots in array
  size
  ;; index of the first element in array
  idx
  ;; the array containing argument list for mistty-log
  array)

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
  (when (or mistty-log-enabled (> mistty-backlog-size 0))
    (mistty--log str args)))

(defun mistty-start-log ()
  "Enable logging for the current buffer and display that buffer.

If logging is already enabled, just show the buffer."
  (interactive)
  (if (and mistty-log-enabled (buffer-live-p mistty-log-buffer))
      (switch-to-buffer-other-window mistty-log-buffer)
    (setq mistty-log-enabled t)
    (mistty--backlog-foreach
     mistty--backlog
     (lambda (args)
       (apply #'mistty--log args)))
    (setq mistty--backlog nil)
    (mistty--log "Log enabled" nil)
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

(defun mistty--log (str args &optional event-time)
  "Logging function, normally called from `mistty-log'.

Calling this function creates `mistty-log-buffer' if it doesn't
exit already."
  (let ((event-time (or event-time (float-time)))
        (calling-buffer (current-buffer)))
    (if (and (not mistty-log-enabled) (> mistty-backlog-size 0))
        ;; not enabled; add to backlog
        (mistty--backlog-add
         (or mistty--backlog
             (setq mistty--backlog (mistty--make-backlog mistty-backlog-size)))
         (list str args event-time))
      
      ;; enabled; log
      (with-current-buffer
          (or (and (buffer-live-p mistty-log-buffer) mistty-log-buffer)
              (setq mistty-log-buffer
                    (progn
                      (get-buffer-create "*mistty-log*"))))
        (setq-local window-point-insertion-type t)
        (goto-char (point-max))
        (insert-before-markers
         (propertize
          (format "[%s] %3.3f "
                  (buffer-name calling-buffer)
                  (- event-time
                     (or mistty--log-start-time
                         (setq mistty--log-start-time event-time))))
          'face 'mistty-log-header-face))
        (insert-before-markers
         (propertize
          (apply #'format str (mapcar #'mistty--format-log-arg args))
          'face 'mistty-log-message-face))
        (insert-before-markers "\n")))))

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

(defun mistty--backlog-add (backlog elt)
  (aset (mistty--backlog-array backlog)
        (mistty--backlog-idx backlog)
        elt)
  (cl-incf (mistty--backlog-idx backlog))
  (when (>= (mistty--backlog-idx backlog)
            (mistty--backlog-size backlog))
    (setf (mistty--backlog-idx backlog) 0)))

(defun mistty--backlog-foreach (backlog func)
  (when backlog
    (let ((arr (mistty--backlog-array backlog))
          (size (mistty--backlog-size backlog))
          (idx (mistty--backlog-idx backlog))
          (i (mistty--backlog-idx backlog)))
      (while (< i size)
        (when-let ((elt (aref arr i)))
          (funcall func elt))
        (setq i (1+ i)))
      (setq i 0)
      (while (< i idx)
        (when-let ((elt (aref arr i)))
          (funcall func elt))
        (setq i (1+ i))))))


(provide 'mistty-log)
