;;; oterm.el --- One Terminal -*- lexical-binding: t -*-

;; Copyright (C) 2023 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Version: 0.1
;; Package-Requires: ((emacs "28.2"))
;; Keywords: convenience, unix
;; URL: http://github.com/szermatt/mixterm


;;; Commentary:
;; 

(require 'term)
(require 'subr-x)
(require 'text-property-search)

;;; Code:

(defvar-local oterm-work-buffer nil)
(defvar-local oterm-term-buffer nil)
(defvar-local oterm-term-proc nil)
(defvar-local oterm-sync-marker nil)
(defvar-local oterm-cmd-start-marker nil)
(defvar-local oterm-sync-ov nil)
(defvar-local oterm-bracketed-paste nil)
(defvar-local oterm--old-point nil)
(defvar oterm--inhibit-sync nil)

(defconst oterm-left-str "\eOD")
(defconst oterm-right-str "\eOC")
(defconst oterm-bracketed-paste-start-str "\e[200~")
(defconst oterm-bracketed-paste-end-str "\e[201~")

(defface oterm-debug-face
  '((t (:box (:line-width (2 . 2) :color "red" :style released-button))))
  "Face used to highlight `oterm-sync-ov' for debugging.")

(defface oterm-debug-prompt-face '((t (:background "dark green")))
  "Face used to highlight prompts for debugging.")

(defface oterm-debug-insert-face '((t (:background "cyan")))
  "Face used to highlight prompts for debugging.")

(defvar oterm-mode-map
  (let ((oterm-mode-map (make-sparse-keymap)))
    (define-key oterm-mode-map (kbd "C-c C-c") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-z") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-\\") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-p") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-n") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-r") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-s") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-g") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-a") 'oterm-goto-pmark-and-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-e") 'oterm-goto-pmark-and-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-n") 'oterm-next-prompt)
    (define-key oterm-mode-map (kbd "C-c C-p") 'oterm-previous-prompt)
    oterm-mode-map))

(defvar oterm-prompt-map
  (let ((oterm-prompt-map (make-sparse-keymap)))
    (define-key oterm-prompt-map (kbd "RET") 'oterm-send-command)
    (define-key oterm-prompt-map [S-return] 'newline)
    (define-key oterm-prompt-map (kbd "TAB") 'oterm-send-tab)
    (define-key oterm-prompt-map (kbd "DEL") 'oterm-send-backspace)
    (define-key oterm-prompt-map (kbd "C-d") 'oterm-delchar-or-maybe-eof)
    (define-key oterm-prompt-map [remap self-insert-command] 'oterm-self-insert-command )
    oterm-prompt-map))

(define-derived-mode oterm-mode fundamental-mode "One Term" "Major mode for One Term."
  (let ((work-buffer (current-buffer))
        (term-buffer (generate-new-buffer (concat " oterm tty " (buffer-name)) 'inhibit-buffer-hooks)))
    (setq oterm-work-buffer work-buffer)
    (setq oterm-term-buffer term-buffer)
    (setq oterm-sync-marker (copy-marker (point-min)))
    (setq oterm-cmd-start-marker (copy-marker (point-min)))
    (setq oterm-sync-ov (make-overlay (point-min) (point-max) nil nil 'rear-advance))
    (overlay-put oterm-sync-ov 'face 'oterm-debug-face)
    (overlay-put oterm-sync-ov 'keymap oterm-prompt-map)
    ;;(overlay-put oterm-sync-ov 'insert-in-front-hooks (list #'oterm--modification-hook))
    (overlay-put oterm-sync-ov 'modification-hooks (list #'oterm--modification-hook))
    (overlay-put oterm-sync-ov 'insert-behind-hooks (list #'oterm--modification-hook))
    (with-current-buffer term-buffer
      (term-mode)
      (setq oterm-work-buffer work-buffer)
      (setq oterm-term-buffer term-buffer)
      (setq oterm-sync-marker (copy-marker (point-min)))
      (setq-local term-suppress-hard-newline t
                  term-char-mode-buffer-read-only t
                  term-char-mode-point-at-process-mark t
                  term-buffer-maximum-size 0
                  term-height 24
                  term-width 80)
      (term--reset-scroll-region))
    (add-hook 'kill-buffer-hook #'oterm--kill-term-buffer nil t)))

(defmacro oterm--with-live-buffer (buf &rest body)
  (declare (indent 1))
  (let ((tempvar (make-symbol "buf")))
    `(let ((,tempvar ,buf))
       (when (buffer-live-p ,tempvar)
         (with-current-buffer ,tempvar
           ,@body)))))
    
(defun oterm--kill-term-buffer ()
  (kill-buffer oterm-term-buffer))

(defun oterm--exec (program &rest args)
  (oterm-mode)
  (with-current-buffer oterm-term-buffer
    (term-exec oterm-term-buffer (buffer-name oterm-term-buffer) program nil args)
    (term-char-mode))
  (let ((proc (get-buffer-process oterm-term-buffer)))
    (with-current-buffer oterm-term-buffer
      (setq oterm-term-proc proc)
      (process-put proc 'oterm-work-buffer oterm-work-buffer)
      (process-put proc 'oterm-term-buffer oterm-term-buffer)
      (set-process-filter proc #'oterm-process-filter)
      (set-process-sentinel proc #'oterm-process-sentinel))
    (with-current-buffer oterm-work-buffer
      (setq oterm-term-proc proc)
      (add-hook 'pre-command-hook #'oterm-pre-command nil t)
      (add-hook 'post-command-hook #'oterm-post-command nil t))))

(defsubst oterm--buffer-p (buffer)
  "Return the BUFFER if the buffer is a live oterm buffer."
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

(defun oterm-process-sentinel (proc msg)
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

(defun oterm-process-filter (proc str)
  (let ((old-pmark (marker-position (process-mark proc)))
        (work-buffer (process-get proc 'oterm-work-buffer))
        (term-buffer (process-get proc 'oterm-term-buffer))
        (bracketed-paste-turned-on nil)
        (inhibit-modification-hooks t))
    (setq bracketed-paste-turned-on (oterm-emulate-terminal proc str))
    (oterm--with-live-buffer term-buffer
      (goto-char (process-mark proc)))
    (oterm--with-live-buffer work-buffer
      (when (buffer-live-p term-buffer)
        (setq default-directory (buffer-local-value 'default-directory term-buffer))
        (unless oterm--inhibit-sync
          (oterm--term-to-work)
          (when bracketed-paste-turned-on
            (oterm--move-sync-mark (oterm--pmark) 'set-prompt))
          (when (/= old-pmark (marker-position (process-mark proc)))
            (goto-char (oterm--pmark))))))))

(defun oterm-emulate-terminal (proc str)
  "Handle special terminal codes, then call `term-emlate-terminal'.

This functions intercepts some extented sequences term.el. This
all should rightly be part of term.el."
  (let ((start 0)
        (bracketed-paste-turned-on nil)
        found)
    (while (setq found (string-match "\e\\[\\(\\?2004[hl]\\)" str start))
      (let ((ext (match-string 1 str))
            (next (match-end 0)))
        (term-emulate-terminal proc (substring str start next))
        (oterm--with-live-buffer (process-get proc 'oterm-work-buffer)
          (cond
           ((equal ext "?2004h")
            (setq oterm-bracketed-paste t
                  bracketed-paste-turned-on t))
           ((equal ext "?2004l")
            (setq oterm-bracketed-paste nil))))
        (setq start next)))
    (let ((final-str (substring str start)))
      (unless (zerop (length final-str))
        (term-emulate-terminal proc final-str)))
    bracketed-paste-turned-on))

(defun oterm--maybe-bracketed-str (str)
  (when (string-match "\t" str)
    (setq str (replace-string "\t" (make-string tab-width " "))))
  (cond
   ((not oterm-bracketed-paste) str)
   ((not (string-match "[[:cntrl:]]" str)) str)
   (t (concat oterm-bracketed-paste-start-str
              str
              oterm-bracketed-paste-end-str
              oterm-left-str
              oterm-right-str))))

(defun oterm--pmark ()
  "The terminal process mark as a position within the current buffer (work or term)."
  (+ oterm-sync-marker (with-current-buffer oterm-term-buffer
                         (- (point) oterm-sync-marker))))

(defun oterm--term-to-work ()
  (let ((inhibit-modification-hooks t))
    (with-current-buffer oterm-term-buffer
      (save-restriction
        (narrow-to-region oterm-sync-marker (point-max-marker))
        (with-current-buffer oterm-work-buffer
          (let ((saved-undo buffer-undo-list))
            (save-excursion
              (save-restriction
                (narrow-to-region oterm-sync-marker (point-max-marker))
                (let ((inhibit-modification-hooks t))
                  ;; Clear text properties that might have been set by
                  ;; the tty in a previous call.
                  (when (< oterm-cmd-start-marker (point-max))
                    (set-text-properties oterm-cmd-start-marker (point-max) nil))
                  (condition-case nil
                      (replace-buffer-contents oterm-term-buffer)
                    (text-read-only
                     ;; Replace-buffer-contents attempted to modify the prompt.
                     ;; Remove it and try again.
                     (let ((inhibit-read-only t))
                       (set-text-properties (point-min) (point-max) nil)
                       (move-marker oterm-cmd-start-marker oterm-sync-marker)
                       (replace-buffer-contents oterm-term-buffer)))))))
            (setq buffer-undo-list saved-undo)))))
    
    ;; Next time, only sync the visible portion of the terminal.
    (with-current-buffer oterm-term-buffer
      (when (< oterm-sync-marker term-home-marker)
        (oterm--move-sync-mark term-home-marker)))

    ;; Truncate the term buffer, since scrolling back is available on
    ;; the work buffer anyways. This has to be done now, after syncing
    ;; the marker, and not in term-emulate-terminal, which is why
    ;; term-buffer-maximum-size is set to 0.
    (with-current-buffer oterm-term-buffer
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char term-home-marker)
          (forward-line -5)
          (delete-region (point-min) (point)))))

    ))

(defun oterm--move-sync-mark (pos &optional set-prompt)
  (let ((chars-from-bol (- pos (oterm--bol-pos-from pos)))
        (chars-from-end (- (point-max) (oterm--bol-pos-from pos))))
    (with-current-buffer oterm-term-buffer
      (move-marker oterm-sync-marker (- (point-max) chars-from-end)))
    (with-current-buffer oterm-work-buffer
      (when (> oterm-cmd-start-marker oterm-sync-marker)
        (let ((inhibit-read-only t))
          (remove-text-properties oterm-sync-marker oterm-cmd-start-marker '(read-only t))))
      (let* ((sync-pos (- (point-max) chars-from-end))
             (cmd-start-pos (+ sync-pos chars-from-bol)))
        (move-marker oterm-sync-marker sync-pos)
        (move-marker oterm-cmd-start-marker cmd-start-pos)
        (move-overlay oterm-sync-ov sync-pos (point-max))
        (when (and set-prompt (> cmd-start-pos sync-pos))
          (add-text-properties sync-pos cmd-start-pos
                               '(oterm prompt
                                       field 'oterm-prompt
                                       rear-nonsticky t
                                       face oterm-debug-prompt-face))
          (add-text-properties sync-pos cmd-start-pos
                               '(read-only t front-sticky t)))))))

(defun oterm-send-raw-string (str)
  (when (and str (not (zerop (length str))))
    (with-current-buffer oterm-term-buffer
      (term-send-raw-string str))))

(defun oterm--at-prompt-1 (&optional inexact)
  (let ((pmark (oterm--pmark)))
    (if inexact
        (or (>= (point) pmark)
            (>= (oterm--bol-pos-from (point))
                (oterm--bol-pos-from pmark)))
        (= (point) pmark))))

(defun oterm--bol-pos-from (pos)
  (save-excursion
    (goto-char pos)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position))))

(defun oterm--eol-pos-from (pos)
  (save-excursion
    (goto-char pos)
    (let ((inhibit-field-text-motion t))
      (line-end-position))))

(defun oterm-send-command ()
  "Send the current command to the shell."
  (interactive)
  (oterm-send-raw-string "\n"))

(defun oterm-send-tab ()
  "Send TAB to the shell."
  (interactive)
  (oterm-send-raw-string "\t"))

(defun oterm-send-backspace ()
  "Send DEL to the shell."
  (interactive)
  (when (get-pos-property (point) 'read-only)
    (signal 'text-read-only nil))
  (oterm-send-raw-string "\b"))

(defun oterm-self-insert-command (n &optional c)
  (interactive "p")
  (when (get-pos-property (point) 'read-only)
    (signal 'text-read-only nil))
  (let ((keys (this-command-keys)))
    (oterm-send-raw-string (make-string n (aref keys (1- (length keys)))))))

(defun oterm-send-raw-key ()
  (interactive)
  (let ((keys (this-command-keys)))
    (oterm-send-raw-string (make-string 1 (aref keys (1- (length keys)))))))

(defun oterm-goto-pmark-and-send-raw-key ()
  (interactive)
  (goto-char (oterm--pmark))
  (let ((keys (this-command-keys)))
    (oterm-send-raw-string (make-string 1 (aref keys (1- (length keys)))))))

(defun oterm-delchar-or-maybe-eof (arg)
  (interactive "p")
  (if (zerop (length (replace-regexp-in-string "[[:blank:]]*" (buffer-substring-no-properties oterm-sync-marker (oterm--eol-pos-from oterm-sync-marker)) "")))
      (oterm-send-raw-string (kbd "C-d"))
    (delete-char arg)))

(defun oterm--modification-hook (_ov is-after orig-beg orig-end &optional old-length)
  (when (and (buffer-live-p oterm-term-buffer)
             is-after
             (>= orig-end oterm-cmd-start-marker))
    ;; Attempt to replay the change in the terminal.
    (let* ((pmark (oterm--pmark))
           (initial-point (point))
           (beg (max orig-beg oterm-cmd-start-marker))
           (end (max orig-end oterm-cmd-start-marker))
           (content-to-replay (buffer-substring-no-properties beg end))
           (chars-to-delete (- old-length (- beg orig-beg))))

      (if (and
           (> chars-to-delete 0)
           (zerop (length content-to-replay))
           (= pmark (+ beg chars-to-delete)))
          (progn
            (oterm--send-and-wait (oterm--move-str pmark (+ beg chars-to-delete)))
            (setq pmark (oterm--pmark))
            (oterm--send-and-wait (oterm--repeat-string (- pmark beg) "\b")))
        
        (oterm--send-and-wait (oterm--move-str pmark beg))
        (setq pmark (oterm--pmark))
        ;; pmark is as close to beg as we can make it
        
        ;; We couldn't move pmark as far back as beg. Presumably, the
        ;; process mark points to the leftmost modifiable position of
        ;; the command line. Update the sync marker to start sync there
        ;; from now on and avoid getting this hook called unnecessarily.
        ;; This is done from inside the term buffer as the modifications
        ;; of the work buffer could interfere. TODO: What if the process
        ;; is just not accepting any input at this time? We might move
        ;; sync mark to far down.
        (when (> pmark beg)
          (oterm--move-sync-mark pmark 'set-prompt))
        
        ;; Replay the portion of the change that we think we can
        ;; replay.
        ;;
        ;; TODO: what if [beg, end] start in the command-line portion
        ;; of the screen and end in the portion of the screen
        ;; containing zsh completion? We'd be "replaying" zsh
        ;; completion results.
        (cond
         ((< pmark beg)
          (setq content-to-replay "" chars-to-delete 0))
         ((> pmark beg)
          (setq content-to-replay (if (<= pmark end) (substring content-to-replay (- pmark beg)) "")
                chars-to-delete (- chars-to-delete (- pmark beg)))))
        (oterm--send-and-wait
         (concat
          (when (> chars-to-delete 0)
            (concat (oterm--repeat-string chars-to-delete oterm-right-str)
                    (oterm--repeat-string chars-to-delete "\b")))
          (oterm--maybe-bracketed-str content-to-replay))))
        
      ;; Copy the modifications on the term buffer to the work buffer.
      ;; This might undo part of the modification that couldn't be
      ;; replayed, but only those after the *new* sync marker.
      (oterm--term-to-work)
      
      ;; If insertion went right, the pmark should be at the end of the
      ;; insertion zone, so moving the point to the pmark guarantees
      ;; that the pointer position makes sense, even after the changes
      ;; applied to the work buffer by oterm--term-to-work.
      (when (equal initial-point end)
        (goto-char (oterm--pmark))))))

(defun oterm--send-and-wait (str)
  (when (and str (not (zerop (length str))))
    (let ((oterm--inhibit-sync t))
      (oterm-send-raw-string str)
      (when (accept-process-output oterm-term-proc 1 nil t) ;; TODO: tune the timeout
        (while (accept-process-output oterm-term-proc 0 nil t))))))

(defun oterm--move-str (from to)
  (let ((diff (- from to)))
    (if (zerop diff)
        nil
      (oterm--repeat-string
       (abs diff)
       (if (< diff 0) oterm-right-str oterm-left-str)))))

(defun oterm--move-pmark-str (to)
  (oterm--move-str (oterm--pmark) to))

(defun oterm--repeat-string (count elt)
  (let ((elt-len (length elt)))
    (if (= 1 elt-len)
        (make-string count (aref elt 0))
      (let ((str (make-string (* count elt-len) ?\ )))
        (dotimes (i count)
          (dotimes (j elt-len)
            (aset str (+ (* i elt-len) j) (aref elt j))))
        str))))

(defun oterm-next-prompt (n)
  (interactive "p")
  (let (found)
    (dotimes (_ n)
      (if (setq found (text-property-any (point) (point-max) 'oterm 'prompt))
          (goto-char (or (next-single-property-change found 'oterm) (point-max)))
        (error "No next prompt")))))

(defun oterm-previous-prompt (n)
  (interactive "p")
  (dotimes (_ n)
    (unless (text-property-search-backward 'oterm 'prompt)
      (error "No previous prompt"))))

(defun oterm-pre-command ()
  (setq oterm--old-point (point)))

(defun oterm-post-command ()
  (when (and oterm--old-point
             (/= (point) oterm--old-point)
             (markerp oterm-sync-marker)
             (>= (point) oterm-sync-marker)
             (process-live-p oterm-term-proc)
             (buffer-live-p oterm-term-buffer))
      (oterm-send-raw-string (oterm--move-pmark-str (point)))))

(provide 'oterm)

;;; oterm.el ends here
