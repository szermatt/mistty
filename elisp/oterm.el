;; -*- lexical-binding: t -*-

(require 'term)

(defvar-local oterm-work-buffer nil)
(defvar-local oterm-term-buffer nil)
(defvar-local oterm-term-proc nil)
(defvar-local oterm-sync-marker nil)
(defvar oterm--inhibit-sync nil)

(defconst oterm-left-str "\eOD")
(defconst oterm-right-str "\eOC")
(defconst oterm-bracketed-paste-start-str "\e[200~")
(defconst oterm-bracketed-paste-end-str "\e[201~")

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
    (add-hook 'kill-buffer-hook 'oterm--kill-term-buffer nil t)))

(defun oterm--kill-term-buffer ()
  (message "kill term buffer")
  (kill-buffer oterm-term-buffer))

(defun oterm--exec (program &rest args)
  (oterm-mode)
  (with-current-buffer oterm-term-buffer
    (term-exec oterm-term-buffer (buffer-name oterm-term-buffer) program nil args)
    (term-char-mode))
  (let ((proc (get-buffer-process oterm-term-buffer)))
    (with-current-buffer oterm-work-buffer
      (setq oterm-term-proc proc))
    (with-current-buffer oterm-term-buffer
      (setq oterm-term-proc proc)
      (process-put proc 'oterm-work-buffer oterm-work-buffer)
      (process-put proc 'oterm-term-buffer oterm-term-buffer)
      (message "bind process filter")
      (set-process-filter proc #'oterm-emulate-terminal)
      (set-process-sentinel proc #'oterm-sentinel))))

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

(defun oterm-sentinel (proc msg)
  (when (memq (process-status proc) '(signal exit))
    (let ((work-buffer (process-get proc 'oterm-work-buffer))
          (term-buffer (process-get proc 'oterm-term-buffer)))
      (if (buffer-live-p work-buffer)
          (progn
            (while (accept-process-output proc 0 0 t))
            (term-sentinel proc msg)
            (with-current-buffer work-buffer
              (oterm--term-to-work))
            (kill-buffer term-buffer))
        (term-sentinel proc msg)))))
    
(defun oterm-emulate-terminal (proc str)
  (let ((old-pmark (marker-position (process-mark proc)))
        (work-buffer (process-get proc 'oterm-work-buffer)))
    (term-emulate-terminal proc str)
    (when (and (not oterm--inhibit-sync) (buffer-live-p work-buffer))
      (with-current-buffer work-buffer
        (oterm--refresh-work-buffer-after-change old-pmark)))))

(defun oterm--refresh-work-buffer-after-change (old-pmark)
  (with-current-buffer oterm-work-buffer
    (when (buffer-live-p oterm-term-buffer)
      (goto-char (oterm--work-pmark))
      (oterm--term-to-work)
      (oterm--update-properties)
      (when (/= old-pmark (marker-position (process-mark oterm-term-proc)))
        (oterm--pmarker-to-point)))))

(defun oterm--work-pmark ()
  "The terminal process mark as a position within the work buffer."
  (with-current-buffer oterm-work-buffer
    (+ oterm-sync-marker (with-current-buffer oterm-term-buffer
                           (- (point) oterm-sync-marker)))))

(defun oterm--pmarker-to-point ()
  (when (buffer-live-p oterm-term-buffer)
    (with-current-buffer oterm-work-buffer
      (goto-char (+ oterm-sync-marker (with-current-buffer oterm-term-buffer
                                        (- (point) oterm-sync-marker)))))))

(defun oterm--update-sync-markers ()
  (when (buffer-live-p oterm-term-buffer)
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
            (move-marker oterm-sync-marker term-home-marker)))))))

(defun oterm--term-to-work ()
  (when (buffer-live-p oterm-term-buffer)
    (with-current-buffer oterm-term-buffer
      (save-restriction
        (narrow-to-region oterm-sync-marker (point-max-marker))
        (with-current-buffer oterm-work-buffer
          (save-restriction
            (narrow-to-region oterm-sync-marker (point-max-marker))
            (replace-buffer-contents oterm-term-buffer)))))))

(defun oterm--update-properties ()
  (with-current-buffer oterm-work-buffer
    (let ((pmark (oterm--work-pmark))
          new-pmark beg end)
      (save-excursion
        (goto-char pmark)
        (setq beg (line-beginning-position))
        (setq end (line-end-position)))
      (when (/= end beg)
        (if (and (not (text-property-any beg pmark 'oterm 'prompt))
                 (not (text-property-any beg pmark 'oterm 'command)))
            ;; This is the first time pmark is on this line. Try to
            ;; figure out where the command and prompt are.
            (progn
              (when (> pmark beg)
                (oterm--send-and-wait (oterm--repeat-string (- pmark beg) oterm-left-str)))
              (setq new-pmark (oterm--work-pmark))
              (when (and (>= new-pmark beg) (<= new-pmark end) (<= new-pmark pmark))
                ;; sanity check on new-pmark, in case the process is in the middle of updating
                ;; its content.
                (when (> new-pmark beg)
                  (add-text-properties beg new-pmark '(oterm prompt)))
                (when (< new-pmark end)
                  (add-text-properties new-pmark end '(oterm command field oterm-command)))
                (when (> pmark new-pmark)
                  (oterm--send-and-wait (oterm-repeat-string (- pmark new-pmark) oterm-right-str))))
              ;; Since we used send-and wait, which inhibit work
              ;; buffer update, it might be necessary to update the
              ;; buffer content now.
              (oterm--refresh-work-buffer-after-change pmark))
          (add-text-properties
           (or (next-single-property-change beg 'oterm) pmark)
           end
           '(oterm command field oterm-command)))))))

(defun oterm--send-and-wait (str)
  (let ((oterm--inhibit-sync t))
    (oterm-send-raw-string str)
    (when (accept-process-output oterm-term-proc 1 nil t) ;; TODO: tune the timeout
      (while (accept-process-output oterm-term-proc 0 nil t)))))

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

(defun oterm--repeat-string (count elt)
  (let ((elt-len (length elt)))
    (if (= 1 elt-len)
        (make-string count (aref elt 0))
      (let ((str (make-string (* count elt-len) ?\ )))
        (dotimes (i count)
          (dotimes (j elt-len)
            (aset str (+ (* i elt-len) j) (aref elt j))))
        str))))
(provide 'oterm)
