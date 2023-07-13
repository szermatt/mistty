
(defvar my-term-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] 'my-term-self-insert-command )
    (define-key map [remap term-send-input] 'my-term-send-input)
    map
    ))

(defvar my-term--pre-point nil)
(make-local-variable 'my-term--pre-point)

(defvar my-term--outside-pregion nil)
(make-local-variable 'my-term--outside-pregion)

(define-derived-mode my-term-mode term-mode "My Term" "Major mode for my term"
  (my-term-enable)
  )

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
     nil nil)
    (set-process-filter (get-buffer-process (current-buffer)) #'my-term-emulate-terminal)
    ))

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

(defun my-term-char-enable ()
  (interactive)
  (add-hook 'pre-command-hook #'term-set-goto-process-mark nil t)
  (add-hook 'post-command-hook #'term-goto-process-mark-maybe nil t))

(defun my-term-char-disable ()
  (interactive)
  (remove-hook 'pre-command-hook #'term-set-goto-process-mark t)
  (remove-hook 'post-command-hook #'term-goto-process-mark-maybe t))

(defun my-term-enable ()
  (interactive)
  (add-hook 'pre-command-hook #'my-term--pre-command nil t)
  (add-hook 'post-command-hook #'my-term--post-command nil t))

(defun my-term-disable ()
  (interactive)
  (remove-hook 'pre-command-hook #'my-term--pre-command t)
  (remove-hook 'post-command-hook #'my-term--post-command t))

(defun my-term--pre-command ()
  (setq my-term-pre-point (point)))

(defun my-term--post-command ()
  (when (/= (point) my-term-pre-point)
    (when (or
           (and (eq my-term--outside-pregion '<) (>= (point) (marker-position (term-process-mark))))
           (and (eq my-term--outside-pregion '>) (<= (point) (marker-position (term-process-mark)))))
      (setq my-term--outside-pregion nil))
    (when (not my-term--outside-pregion)
      (let* ((p (point))
             (move-status (my-term--try-to-move-pmark-to p)))
        (setq my-term--outside-pregion move-status)
        (if (null move-status)
            (setq buffer-read-only nil)
          (setq buffer-read-only t)
          (goto-char p))))))

(defun my-term--try-to-move-pmark-to (goal)
  (let* ((proc (get-buffer-process (current-buffer)))
         (pmark (marker-position (process-mark proc)))
         (diff (- pmark goal )))
    (when (/= 0 diff)
      (let ((str (make-string (* 3 (abs diff)) ?\ ))
            (direction-char (if (< diff 0) ?C ?D)))
        (dotimes (i (abs diff))
          (aset str (* i 3) ?\e)
          (aset str (+ (* i 3) 1) ?\O)
          (aset str (+ (* i 3) 2) direction-char))
        (term-send-raw-string str)
        (accept-process-output proc 0 50 t)
        (let ((pmark (marker-position (process-mark proc))))
          (cond
           ((< goal pmark) '<)
           ((> goal pmark '>)
            nil)))))))

(defun my-term-emulate-terminal (proc str)
  (setq my-term--outside-pregion nil)
  (term-emulate-terminal proc str))

(provide 'my-term)
