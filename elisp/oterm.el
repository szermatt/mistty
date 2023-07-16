;; -*- lexical-binding: t -*-

(require 'term)

(defvar-local oterm-work-buffer nil)
(defvar-local oterm-term-buffer nil)
(defvar-local oterm-term-proc nil)
(defvar-local oterm-sync-marker nil)

(defvar oterm-mode-map
  (let ((oterm-mode-map (make-sparse-keymap)))
    (define-key oterm-mode-map [remap self-insert-command] 'oterm-self-insert-command )
    (define-key oterm-mode-map [(return)] 'oterm-send-input)
    oterm-mode-map
    ))

(define-derived-mode oterm-mode fundamental-mode "One Term" "Major mode for One Term"
  (let ((work-buffer (current-buffer))
        (term-buffer (generate-new-buffer (concat " oterm tty " (buffer-name)) 'inhibit-buffer-hooks)))
    (setq oterm-work-buffer work-buffer)
    (setq oterm-term-buffer term-buffer)
    (setq oterm-sync-marker (copy-marker 0))
    (with-current-buffer term-buffer
      (term-mode)
      (setq oterm-work-buffer work-buffer)
      (setq oterm-term-buffer term-buffer)
      (setq oterm-sync-marker (copy-marker 0))
      (setq-local term-suppress-hard-newline t
                  term-char-mode-buffer-read-only t
                  term-char-mode-point-at-process-mark t
                  term-buffer-maximum-size 0
                  term-height 24
                  term-width 80)
      (term--reset-scroll-region))

    (message "(3)work: %s/%s term: %s/%s"
             (buffer-local-value 'oterm-work-buffer work-buffer)
             (buffer-local-value 'oterm-work-buffer term-buffer)
             (buffer-local-value 'oterm-term-buffer work-buffer)
             (buffer-local-value 'oterm-term-buffer term-buffer))
    ))

(defun oterm--exec (program &rest args)
  (oterm-mode)
  (with-current-buffer oterm-term-buffer
    (term-exec oterm-term-buffer (buffer-name oterm-term-buffer) program nil args)
    (term-char-mode))
  (let ((proc (get-buffer-process oterm-term-buffer)))
    (with-current-buffer oterm-term-buffer
      (setq oterm-term-proc oterm-term-proc))
    (with-current-buffer oterm-work-buffer
      (setq oterm-term-proc oterm-term-proc))
    (set-process-filter proc #'oterm-emulate-terminal)))

(defsubst oterm--buffer-p (buffer)
  "Return the buffer if the buffer is a live oterm buffer."
  (if (and buffer
           (bufferp buffer)
           (eq 'oterm-mode (buffer-local-value 'major-mode buffer))
           (buffer-live-p buffer)
           (buffer-local-value 'oterm-term-proc buffer)
           (process-live-p (buffer-local-value 'oterm-term-proc buffer)))
      buffer))

(defun oterm--buffers ()
  "List of live term buffers, sorted."
  (sort (delq nil (mapcar #'oterm--buffer-p (buffer-list)))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

(defun oterm ()
  (interactive)
  (let ((existing (oterm--buffers)))
    (if (or current-prefix-arg         ; command prefix was given
            (null existing)            ; there are no oterm buffers
            (and (null (cdr existing)) ; the current buffer is the only oterm buffer
                 (eq (current-buffer) (car existing))))
        ;; create a new one
        (oterm-create)
      (oterm--goto-next existing))))

(defun oterm--goto-next (existing)
  (let ((existing-tail (or (cdr (member (current-buffer) existing))
                           existing)))
    (if existing-tail
        (switch-to-buffer (car existing-tail))
      (error "no next oterm buffer"))))

(defun oterm-create ()
  (interactive)
  (with-current-buffer (generate-new-buffer "*oterm*")
    (oterm--exec (or explicit-shell-file-name shell-file-name (getenv "ESHELL")))
    (switch-to-buffer (current-buffer))
    ))

(defun oterm-emulate-terminal (proc str)
  (let ((old-pmark (marker-position (process-mark proc))))
    (term-emulate-terminal proc str)
    (oterm--term-to-work)
    (when (/= old-pmark (marker-position (process-mark proc)))
      (oterm--pmarker-to-point))))

(defun oterm--pmarker-to-point ()
  (with-current-buffer oterm-work-buffer
    (goto-char (+ oterm-sync-marker (with-current-buffer oterm-term-buffer
                                      (- (point) oterm-sync-marker))))))

(defun oterm--update-sync-markers ()
  (with-current-buffer oterm-term-buffer
    (when (< oterm-sync-marker term-home-marker)
      (save-excursion
        (goto-char oterm-sync-marker)
        (let ((lines (count-lines oterm-sync-marker term-home-marker)))
          (with-current-buffer oterm-work-buffer
            (save-excursion
              (goto-char oterm-sync-marker)
              (forward-line lines)
              (move-marker oterm-sync-marker (point))))
          (move-marker oterm-sync-marker term-home-marker))))))

(defun oterm--term-to-work ()
  (with-current-buffer oterm-term-buffer
    (save-restriction
      (narrow-to-region oterm-sync-marker (point-max-marker))
      (with-current-buffer oterm-work-buffer
        (save-restriction
          (narrow-to-region oterm-sync-marker (point-max-marker))
          (replace-buffer-contents oterm-term-buffer))))))

(defun oterm-send-raw-string (str)
  (with-current-buffer oterm-term-buffer
    (term-send-raw-string str)))

(defun oterm-self-insert-command (n &optional c)
  (interactive "p")
  (with-current-buffer oterm-term-buffer
    (let ((keys (or c (this-command-keys))))
        (term-send-raw-string (make-string n (aref keys (1- (length keys))))))))

(defun oterm-send-input ()
  (interactive)
  (oterm-self-insert-command 1 "\n"))

(provide 'oterm)
