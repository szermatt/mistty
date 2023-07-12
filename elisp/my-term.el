
(defvar my-term-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'my-term-self-insert-command )
    (define-key map [remap term-send-input] 'my-term-send-input)
    map
    ))

(define-derived-mode my-term-mode term-mode "My Term" "Major mode for my term" )

(defsubst my-term--buffer-p (buffer)
  "Return the buffer if the buffer is a live my-term buffer."
  (if (and buffer
           (bufferp buffer)
           (eq 'my-term-mode (buffer-local-value 'major-mode buffer))
           (buffer-live-p buffer)
           (get-buffer-process buffer)
           (process-live-p (get-buffer-process buffer)))
      buffer))

(defun my-term--buffers ()
  "List of live term buffers, sorted."
  (let ((remote-id (if default-directory (file-remote-p default-directory) nil)))
    (sort (delq nil (mapcar #'my-term--buffer-p (buffer-list)))
          (lambda (a b) (string< (buffer-name a) (buffer-name b))))))


(defun my-term ()
  (interactive)
  (let ((existing (my-term--buffers)))
    (if (or current-prefix-arg         ; command prefix was given
            (null existing)            ; there are no my-term buffers
            (and (null (cdr existing)) ; the current buffer is the only my-term buffer
                 (eq (current-buffer) (car existing))))
        ;; create a new one
        (my-term-create)
      (my-term--goto-next existing))))

(defun my-term--goto-next (existing)
  (let ((existing-tail (or (cdr (member (current-buffer) existing))
                           existing)))
    (if existing-tail
        (switch-to-buffer (car existing-tail))
      (error "no next my-term buffer"))))

(defun my-term-create ()
  (interactive)
  (with-current-buffer (generate-new-buffer "*my-term*")
    (my-term-mode)
    (switch-to-buffer (current-buffer))
    (term-exec
     (current-buffer)
     (buffer-name)
     (or explicit-shell-file-name shell-file-name (getenv "ESHELL"))
     nil nil)))

(defun my-term-self-insert-command (n &optional c)
  (interactive "p")
  (if (let ((proc (get-buffer-process (current-buffer))))
        (= (point) (marker-position (process-mark proc))))
      (let ((keys (this-command-keys)))
        (term-send-raw-string (make-string n (aref keys (1- (length keys))))))
      (self-insert-command n c)))

(defun my-term-send-input ()
  (interactive)
  (my-term-self-insert-command 1 "\n"))

(provide 'my-term)
