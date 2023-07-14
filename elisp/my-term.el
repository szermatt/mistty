;; -*- lexical-binding: t -*-

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

(defvar my-term--changes nil)
(make-local-variable 'my-term--changes)


(define-derived-mode my-term-mode term-mode "My Term" "Major mode for my term"
  (setq my-term--changes nil)
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
  (sort (delq nil (mapcar #'my-term--buffer-p (buffer-list)))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

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

(defun my-term--setup (program &rest args)
  (my-term-mode)
  (term-exec (current-buffer) (buffer-name) program nil args)
  (set-process-filter (get-buffer-process (current-buffer)) #'my-term-emulate-terminal))

(defun my-term-create ()
  (interactive)
  (with-current-buffer (generate-new-buffer "*my-term*")
    (my-term--setup (or explicit-shell-file-name shell-file-name (getenv "ESHELL")))
    (switch-to-buffer (current-buffer))
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
  (add-hook 'post-command-hook #'my-term--post-command nil t)
  (add-hook 'before-change-functions #'my-term--before-change nil t)
  (add-hook 'after-change-functions #'my-term--after-change nil t) )

(defun my-term-disable ()
  (interactive)
  (remove-hook 'pre-command-hook #'my-term--pre-command t)
  (remove-hook 'post-command-hook #'my-term--post-command t)
  (remove-hook 'before-change-functions #'my-term--before-change t)
  (remove-hook 'after-change-functions #'my-term--after-change t))

(defun my-term--before-change (beg end)
  (push (list 'before beg end (buffer-substring-no-properties beg end)) my-term--changes))

(defun my-term--after-change (beg end old-length)
  (push (list 'after beg end old-length (buffer-substring-no-properties beg end)) my-term--changes))

(defun my-term--pre-command ()
  (setq my-term-pre-point (point)))

(defun my-term--post-command ()
  (let ((after-command-point (point)))
    (when my-term--changes
      (my-term--reconcile-changes)
      (setq my-term--outside-pregion nil)
      (goto-char after-command-point))
    (my-term--reconcile-point)))

(defun my-term--reconcile-changes ()
  (interactive)
  (when my-term--changes
    (let ((changes my-term--changes)
          (inhibit-modification-hooks t)
          (buffer-read-only nil))
      (setq my-term--changes nil)
      (my-term--revert-changes changes)
      (my-term--replay-changes (nreverse changes))
      ;; 2. replay, going this time from older to newer
      ;; 3. let it percolate
      (accept-process-output (get-buffer-process (current-buffer)) 0 50 t))))

(defun my-term--revert-changes (changes)
  (dolist (c changes)
    (pcase c
      (`(after ,beg ,end ,old-length ,_)
       (delete-region beg end)
       (goto-char beg)
       ;; insert a placeholder so future positions are correct
       (insert (make-string old-length ?\ )))
      (`(before ,beg ,end ,old-content)
       (delete-region beg end)
       (goto-char beg)
       (insert old-content)))))

(defvar my-term--current-pmark nil)
(make-local-variable 'my-term--current-pmark)

(defun my-term--replay-changes (changes)
  (let ((str (let ((my-term--current-pmark (marker-position (term-process-mark))))
               (mapconcat
                (lambda (c)
                  (pcase c
                    (`(after ,beg ,end ,old-length ,new-content)
                     (prog1 (concat
                             (my-term--str-to-move-pmark my-term--current-pmark beg)
                             new-content
                             (my-term--repeat-string old-length "\eOC")
                             (my-term--repeat-string old-length "\b"))
                       (setq my-term--current-pmark end)))
                    (t "")))
                changes ""))))
    (term-send-raw-string str)
    (accept-process-output (get-buffer-process (current-buffer)) 0 50 t)))

(defun my-term--reconcile-point ()
  (when (/= (point) my-term-pre-point)
    (when (or
           (and (eq my-term--outside-pregion '<) (>= (point) (marker-position (term-process-mark))))
           (and (eq my-term--outside-pregion '>) (<= (point) (marker-position (term-process-mark)))))
      (setq my-term--outside-pregion nil))
    (when (not my-term--outside-pregion)
      (let* ((p (point))
             (diff (my-term--try-to-move-pmark-to p)))
        (if (zerop diff)
            (setq my-term--outside-pregion nil
                  buffer-read-only nil)
          (setq my-term--outside-pregion (if (< diff 0) '< '>)
                buffer-read-only t)
          (goto-char p))))))

(defun my-term--repeat-string (count elt)
  (let ((elt-len (length elt)))
    (if (= 1 elt-len)
        (make-string count (aref elt 0))
      (let ((str (make-string (* count elt-len) ?\ )))
        (dotimes (i count)
          (dotimes (j elt-len)
            (aset str (+ (* i elt-len) j) (aref elt j))))
        str))))
  
(defun my-term--try-to-move-pmark-to (goal)
  (let* ((proc (get-buffer-process (current-buffer)))
         (str (my-term--str-to-move-pmark (marker-position (process-mark proc)) goal)))
    (if (zerop (length str))
        0
      (term-send-raw-string str)
      (accept-process-output proc 0 50 t)
      (- goal (marker-position (process-mark proc))))))

(defun my-term--str-to-move-pmark (pmark goal)
  (let ((diff (- pmark goal)))
    (if (zerop diff)
        ""
      (my-term--repeat-string (abs diff) (if (< diff 0) "\eOC" "\eOD")))))

(defun my-term-emulate-terminal (proc str)
  (setq my-term--outside-pregion nil)
  (let ((inhibit-modification-hooks t))
    (term-emulate-terminal proc str)))

(provide 'my-term)
