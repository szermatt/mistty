;;; mistty.el --- One Terminal -*- lexical-binding: t -*-

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

(defvar mistty-osc-hook nil
  "Hook run when unknown OSC sequences have been received.

This hook is run on the term-mode buffer. It is passed the
content of OSC sequence - everything between OSC (ESC ]) and
ST (ESC \\ or \\a) and may chooose to handle them.

The hook is allowed to modify the term-mode buffer to add text
properties, for example." )

(defvar-local mistty-work-buffer nil)
(defvar-local mistty-term-buffer nil)
(defvar-local mistty-term-proc nil)
(defvar-local mistty-sync-marker nil)
(defvar-local mistty-cmd-start-marker nil)
(defvar-local mistty-sync-ov nil)
(defvar-local mistty-bracketed-paste nil)
(defvar-local mistty-fullscreen nil)
(defvar-local mistty--old-point nil)
(defvar-local mistty--inhibit-sync nil)
(defvar-local mistty--deleted-point-max nil)
(defvar-local mistty--point-follows-next-pmark nil)
(defvar-local mistty--possible-prompt nil)

(eval-when-compile
  ;; defined in term.el
  (defvar term-home-marker))
 
(defconst mistty-left-str "\eOD")
(defconst mistty-right-str "\eOC")
(defconst mistty-bracketed-paste-start-str "\e[200~")
(defconst mistty-bracketed-paste-end-str "\e[201~")
(defconst mistty-fullscreen-mode-message
  (let ((s "Fullscreen mode ON. C-c C-j switches between the tty and scrollback buffer."))
    (add-text-properties 0 (length s) '(mistty message) s)
    s))

(defvar mistty-prompt-re "[#$%>.] *$")

(defvar mistty-positional-keys "\t\C-w\C-t\C-k-C-y")

(defvar mistty-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-d") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-z") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-\\") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-p") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-n") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-r") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-s") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-g") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-a") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-e") 'mistty-send-raw-key)
    (define-key map (kbd "C-c C-n") 'mistty-next-prompt)
    (define-key map (kbd "C-c C-p") 'mistty-previous-prompt)
    (define-key map (kbd "C-c C-j") 'mistty-switch-to-fullscreen-buffer)
    (define-key map (kbd "C-c <up>") 'mistty-send-up)
    (define-key map (kbd "C-c <down>") 'mistty-send-down)
    (define-key map (kbd "C-c <left>") 'mistty-send-left)
    (define-key map (kbd "C-c <right>") 'mistty-send-right)
    (define-key map (kbd "C-c <home>") 'mistty-send-home)
    (define-key map (kbd "C-c <end>") 'mistty-send-end)
    (define-key map (kbd "C-c <insert>") 'mistty-send-insert)
    (define-key map (kbd "C-c <prior>") 'mistty-send-prior)
    (define-key map (kbd "C-c <next>") 'mistty-send-next)
    map))

(defvar mistty-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'mistty-send-command)
    (define-key map [S-return] 'newline)
    (define-key map (kbd "TAB") 'mistty-send-tab)
    (define-key map (kbd "DEL") 'mistty-send-backspace)
    (define-key map (kbd "C-d") 'mistty-delchar-or-maybe-eof)
    (define-key map [remap self-insert-command] 'mistty-self-insert-command )
    map))

(defun mistty-send-up () (interactive) (mistty-send-raw-string "\eOA"))
(defun mistty-send-down () (interactive) (mistty-send-raw-string "\eOB"))
(defun mistty-send-right () (interactive) (mistty-send-raw-string "\eOC"))
(defun mistty-send-left () (interactive) (mistty-send-raw-string "\eOD"))
(defun mistty-send-home () (interactive) (mistty-send-raw-string "\e[1~"))
(defun mistty-send-end () (interactive) (mistty-send-raw-string "\e[4~"))
(defun mistty-send-prior () (interactive) (mistty-send-raw-string "\e[5~"))
(defun mistty-send-next () (interactive) (mistty-send-raw-string "\e[6~"))
(defun mistty-send-insert ()
  (interactive)
  (mistty-before-positional)
  (mistty-send-raw-string "\e[2~"))

(defmacro mistty--with-live-buffer (buf &rest body)
  (declare (indent 1))
  (let ((tempvar (make-symbol "buf")))
    `(let ((,tempvar ,buf))
       (when (buffer-live-p ,tempvar)
         (with-current-buffer ,tempvar
           ,@body)))))

(define-derived-mode mistty-mode fundamental-mode "misTTY" "Line-based TTY."
  :interactive nil
  (setq buffer-read-only nil)
  (setq mistty-work-buffer (current-buffer)))
(put 'mistty-mode 'mode-class 'special)

(defun mistty--exec (program &rest args)
  (mistty-mode)
  (mistty--attach (mistty--create-term program args)))

(defun mistty--create-term (program args)
  (let ((term-buffer (generate-new-buffer (concat " mistty tty " (buffer-name)) 'inhibit-buffer-hooks)))
    (with-current-buffer term-buffer
      (term-mode)
      (setq-local term-char-mode-buffer-read-only t
                  term-char-mode-point-at-process-mark t
                  term-buffer-maximum-size 0
                  term-height (or (floor (window-screen-lines)) 24)
                  term-width (or (window-max-chars-per-line) 80))
      (term--reset-scroll-region)
      (term-exec term-buffer (buffer-name mistty-term-buffer) program nil args)
      (term-char-mode))
    term-buffer))

(defun mistty--attach (term-buffer)
  (let ((work-buffer (current-buffer))
        (proc (get-buffer-process term-buffer)))

    (when proc
      (process-put proc 'mistty-work-buffer work-buffer)
      (process-put proc 'mistty-term-buffer term-buffer))

    (setq mistty-term-proc proc)
    (setq mistty-term-buffer term-buffer)
    (setq mistty-sync-marker (mistty--create-or-reuse-marker mistty-sync-marker (point-max)))
    (setq mistty-cmd-start-marker (copy-marker mistty-sync-marker))
    (setq mistty-sync-ov (make-overlay mistty-sync-marker (point-max) nil nil 'rear-advance))

    (with-current-buffer term-buffer
      (setq mistty-term-proc proc)
      (setq mistty-work-buffer work-buffer)
      (setq mistty-term-buffer term-buffer)
      (setq mistty-sync-marker (mistty--create-or-reuse-marker mistty-sync-marker term-home-marker)))

    (overlay-put mistty-sync-ov 'keymap mistty-prompt-map)
    (overlay-put mistty-sync-ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put mistty-sync-ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (when proc
      (set-process-filter proc #'mistty-process-filter)
      (set-process-sentinel proc #'mistty-process-sentinel))

    (add-hook 'kill-buffer-hook #'mistty--kill-term-buffer nil t)
    (add-hook 'window-size-change-functions #'mistty--window-size-change nil t)
    (add-hook 'pre-command-hook #'mistty-pre-command nil t)
    (add-hook 'post-command-hook #'mistty-post-command nil t)
    
    (mistty--term-to-work)
    (when proc (goto-char (mistty-pmark)))))

(defun mistty--create-or-reuse-marker (m initial-pos)
  (if (not (markerp m))
      (copy-marker initial-pos)
    (when (= 1 (marker-position m))
      (move-marker m initial-pos))
    m))

(defun mistty--detach (&optional keep-sync-markers)
  (remove-hook 'kill-buffer-hook #'mistty--kill-term-buffer t)
  (remove-hook 'window-size-change-functions #'mistty--window-size-change t)
  (remove-hook 'pre-command-hook #'mistty-pre-command t)
  (remove-hook 'post-command-hook #'mistty-post-command t)
  
  (when mistty-sync-ov
    (delete-overlay mistty-sync-ov)
    (setq mistty-sync-ov nil))
  (when mistty-term-proc
    (set-process-filter mistty-term-proc #'term-emulate-terminal)
    (set-process-sentinel mistty-term-proc #'term-sentinel)
    (setq mistty-term-proc nil))
  (when mistty-cmd-start-marker
    (move-marker mistty-cmd-start-marker nil)
    (setq mistty-cmd-start-marker nil))
  (unless keep-sync-markers
    (when mistty-sync-marker
      (move-marker mistty-sync-marker nil)
      (setq mistty-sync-marker nil))
    (mistty--with-live-buffer mistty-term-buffer
      (move-marker mistty-sync-marker nil)
      (setq mistty-sync-marker nil))))

(defun mistty--kill-term-buffer ()
  (let ((term-buffer mistty-term-buffer))
    (mistty--detach)
    (when (buffer-live-p term-buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer term-buffer)))))
      
(defsubst mistty--buffer-p (buffer)
  "Return the BUFFER if the buffer is a live mistty buffer."
  (if (and buffer
           (bufferp buffer)
           (eq 'mistty-mode (buffer-local-value 'major-mode buffer))
           (buffer-live-p buffer)
           (buffer-local-value 'mistty-term-proc buffer)
           (process-live-p (buffer-local-value 'mistty-term-proc buffer)))
      buffer))

(defun mistty--buffers ()
  "List of live term buffers, sorted."
  (sort (delq nil (mapcar #'mistty--buffer-p (buffer-list)))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

(defun mistty ()
  (interactive)
  (let ((existing (mistty--buffers)))
    (if (or current-prefix-arg         ; command prefix was given
            (null existing)            ; there are no mistty buffers
            (and (null (cdr existing)) ; the current buffer is the only mistty buffer
                 (eq (current-buffer) (car existing))))
        ;; create a new one
        (mistty-create)
      (mistty--goto-next existing))))

(defun mistty--goto-next (existing)
  (let ((existing-tail (or (cdr (member (current-buffer) existing))
                           existing)))
    (if existing-tail
        (switch-to-buffer (car existing-tail))
      (error "no next mistty buffer"))))

(defun mistty-create ()
  (interactive)
  (with-current-buffer (generate-new-buffer "*mistty*")
    (mistty--exec (or explicit-shell-file-name shell-file-name (getenv "ESHELL")))
    (switch-to-buffer (current-buffer))
    ))

(defun mistty-process-sentinel (proc msg)
  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-buffer proc)))
    (if (buffer-live-p work-buffer)
        (when (memq (process-status proc) '(signal exit))
          (while (accept-process-output proc 0 0 t))
          (term-sentinel proc msg)
          (with-current-buffer work-buffer
            (mistty--term-to-work)
            (mistty--detach))
          (kill-buffer term-buffer)))
    ;; detached term buffer
    (term-sentinel proc msg)))

(defun mistty--fs-process-sentinel (proc msg)
  (let ((process-dead (memq (process-status proc) '(signal exit)))
        (term-buffer (process-get proc 'mistty-term-buffer))
        (work-buffer (process-get proc 'mistty-work-buffer)))
    (cond
     ((and process-dead (buffer-live-p term-buffer) (buffer-live-p work-buffer))
      (mistty--leave-fullscreen proc "")
      (mistty-process-sentinel proc msg))
     ((and process-dead (not (buffer-live-p term-buffer)) (buffer-live-p work-buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (process-get proc 'mistty-work-buffer)))
      (term-sentinel proc msg))
     (t (term-sentinel proc msg)))))

(defun mistty-process-filter (proc str)
  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-get proc 'mistty-term-buffer)))
    (cond
     ;; detached term buffer
     ((or (not (buffer-live-p work-buffer)) (not (buffer-live-p term-buffer)))
      (term-emulate-terminal proc str))
     
     ;; switch to fullscreen
     ((string-match "\e\\[\\(\\??47\\|\\?104[79]\\)h" str)
      (let ((smcup-pos (match-beginning 0)))
        (mistty-process-filter proc (substring str 0 smcup-pos))
        (with-current-buffer work-buffer
          (mistty--enter-fullscreen proc (substring str smcup-pos)))))
     
     ;; reset
     ((string-match "\ec" str)
      (let ((rs1-after-pos (match-end 0)))
        (mistty-emulate-terminal proc (substring str 0 rs1-after-pos))
        (with-current-buffer work-buffer
          (setq mistty-bracketed-paste nil)
          (mistty--reset-markers))
        (mistty-process-filter proc (substring str rs1-after-pos))))
     
     ;; normal processing
     (t (let ((bracketed-paste-turned-on nil)
              (inhibit-modification-hooks t)
              (inhibit-read-only t)
              (old-sync-position (mistty--with-live-buffer term-buffer (marker-position mistty-sync-marker)))
              (point-on-pmark (or mistty--point-follows-next-pmark
                                  (mistty--with-live-buffer work-buffer
                                    (or (= (point) (mistty-pmark))
                                        (and (= (point) (point-max))
                                             (>= (mistty-pmark) (point-max))))))))
          (setq mistty--point-follows-next-pmark nil)
          (setq bracketed-paste-turned-on (mistty-emulate-terminal proc str))
          (mistty--with-live-buffer term-buffer
            (goto-char (process-mark proc))
            (when (or (< mistty-sync-marker old-sync-position)
                      (< (point) mistty-sync-marker))
              (mistty--reset-markers)
              (setq point-on-pmark t)))
          (mistty--with-live-buffer work-buffer
            (condition-case nil
                (setq default-directory (buffer-local-value 'default-directory term-buffer))
              (error nil))
            (unless mistty--inhibit-sync
              (let ((pmark-on-new-line (> (mistty-pmark) (point-max))))
                (mistty--term-to-work)
                (when bracketed-paste-turned-on
                  (mistty--move-sync-mark (mistty-pmark) 'set-prompt))
                (when pmark-on-new-line
                  (mistty--detect-possible-prompt))
                (when point-on-pmark
                  (goto-char (mistty--safe-pos (mistty-pmark))))))))))))

(defun mistty--detect-possible-prompt ()
  (let* ((pmark (mistty-pmark))
         (bol (mistty--bol-pos-from pmark)))
    (when (and (> pmark bol)
               (= (point-max)
                  (save-excursion
                    (goto-char pmark)
                    (skip-chars-forward ":space:")
                    (point)))
               (string-match
                mistty-prompt-re
                (buffer-substring-no-properties bol pmark)))
      (setq mistty--possible-prompt
            `(,bol ,(+ bol (match-end 0))
                   ,(buffer-substring-no-properties bol (+ bol (match-end 0))))))))
 
(defun mistty--reset-markers ()
  (mistty--with-live-buffer mistty-work-buffer
    (goto-char (point-max))
    (skip-chars-backward "[:space:]")
    (let ((inhibit-read-only t))
      (delete-region (point) (point-max))
      (insert "\n"))
    (move-marker mistty-sync-marker (point-max))
    (move-marker mistty-cmd-start-marker (point-max)))
  (mistty--with-live-buffer mistty-term-buffer
    (save-excursion
      (goto-char term-home-marker)
      (skip-chars-forward "[:space:]")
      (move-marker mistty-sync-marker (point)))))

(defun mistty-emulate-terminal (proc str)
  "Handle special terminal codes, then call `term-emlate-terminal'.

This functions intercepts some extented sequences term.el. This
all should rightly be part of term.el."
  (cl-letf ((start 0)
            (bracketed-paste-turned-on nil)
            ;; Using term-buffer-vertical-motion causes strange
            ;; issues; avoid it. Using mistty's window to compute
            ;; vertical motion is correct since the window dimension
            ;; are kept in sync with the terminal size. Falling back
            ;; to using the selected window, on the other hand, is
            ;; questionable.
            ((symbol-function 'term-buffer-vertical-motion)
             (lambda (count)
               (vertical-motion count (or (get-buffer-window mistty-work-buffer)
                                          (selected-window))))))
    (while (string-match "\e\\(\\[\\?\\(2004\\|25\\)[hl]\\|\\]\\([\x08-0x0d\x20-\x7e]*?\\)\\(\e\\\\\\|\a\\)\\)" str start)
      (let ((ext (match-string 1 str))
            (osc (match-string 3 str))
            (seq-start (match-beginning 0))
            (seq-end (match-end 0))
            (term-buffer (process-get proc 'mistty-term-buffer))
            (work-buffer (process-get proc 'mistty-work-buffer)))
        (cond
         ((equal ext "[?2004h") ; enable bracketed paste
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer work-buffer
            (setq mistty-bracketed-paste t
                  bracketed-paste-turned-on t)))
         ((equal ext "[?2004l") ; disable bracketed paste
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer work-buffer
            (setq mistty-bracketed-paste nil)))
         ((equal ext "[?25h") ; make cursor visible
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer work-buffer
            (setq cursor-type t)))
         ((equal ext "[?25l") ; make cursor invisible
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer work-buffer
            (setq cursor-type nil)))
         (osc
          (term-emulate-terminal proc (substring str start seq-start))
          (mistty--with-live-buffer term-buffer
            (let ((inhibit-read-only t))
              (run-hook-with-args 'mistty-osc-hook osc)))))
          (setq start seq-end)))
    (let ((final-str (substring str start)))
      (unless (zerop (length final-str))
        (term-emulate-terminal proc final-str)))
    bracketed-paste-turned-on))

(defun mistty--fs-process-filter (proc str)
  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-get proc 'mistty-term-buffer)))
    (if (and (string-match "\e\\[\\(\\??47\\|\\?104[79]\\)l\\(\e8\\|\e\\[\\?1048l\\)?" str)
             (buffer-live-p work-buffer)
             (buffer-live-p term-buffer))
        (let ((after-rmcup-pos (match-beginning 0)))
          (mistty-emulate-terminal proc (substring str 0 after-rmcup-pos))
          (with-current-buffer work-buffer
            (mistty--leave-fullscreen proc (substring str after-rmcup-pos))))
      ;; normal processing
      (mistty-emulate-terminal proc str))))

(defun mistty--maybe-bracketed-str (str)
  (let ((str (string-replace "\t" (make-string tab-width ? ) str)))
    (cond
     ((not mistty-bracketed-paste) str)
     ((not (string-match "[[:cntrl:]]" str)) str)
     (t (concat mistty-bracketed-paste-start-str
                str
                mistty-bracketed-paste-end-str
                mistty-left-str
                mistty-right-str)))))

(defun mistty-pmark ()
  (mistty--from-pos-of (process-mark mistty-term-proc) mistty-term-buffer))

(defun mistty--from-pos-of (pos buffer-of-pos)
  "Return the local equivalent to POS defined in BUFFER-OF-POS."
  (+ mistty-sync-marker (with-current-buffer buffer-of-pos
                         (- pos mistty-sync-marker))))

(defun mistty--term-to-work ()
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (with-current-buffer mistty-work-buffer
      (let ((initial-undo-list buffer-undo-list)
            (initial-point (point))
            (initial-mark (marker-position (mark-marker)))
            (initial-mark-active mark-active)
            (prompt-length (- mistty-cmd-start-marker mistty-sync-marker)))
        (save-restriction
          (widen)
          (when (and (> prompt-length 0)
                     (not (string=
                           (with-current-buffer mistty-term-buffer
                             (buffer-substring-no-properties
                              mistty-sync-marker (mistty--safe-pos (+ mistty-sync-marker prompt-length))))
                           (buffer-substring-no-properties
                            mistty-sync-marker mistty-cmd-start-marker))))
            ;; the prompt was modified; reset it
            (move-marker mistty-cmd-start-marker mistty-sync-marker)
            (setq prompt-length 0))
          (goto-char mistty-cmd-start-marker)
          (insert (with-current-buffer mistty-term-buffer
                    (buffer-substring (+ mistty-sync-marker prompt-length) (point-max))))
          (delete-region (point) (point-max))
          
          ;; recover buffer state possibly destroyed by delete-region.
          (goto-char (mistty--safe-pos initial-point))
          (move-marker (mark-marker) (when initial-mark (mistty--safe-pos initial-mark)))
          (setq mark-active initial-mark-active)
          (setq buffer-undo-list initial-undo-list))))
    
    (with-current-buffer mistty-term-buffer
      ;; Next time, only sync the visible portion of the terminal.
      (when (< mistty-sync-marker term-home-marker)
        (mistty--move-sync-mark term-home-marker))
      
      ;; Truncate the term buffer, since scrolling back is available on
      ;; the work buffer anyways. This has to be done now, after syncing
      ;; the marker, and not in term-emulate-terminal, which is why
      ;; term-buffer-maximum-size is set to 0.
      (save-excursion
        (goto-char term-home-marker)
        (forward-line -5)
        (delete-region (point-min) (point))))))

(defun mistty--move-sync-mark (pos &optional set-prompt)
  (let ((chars-from-bol (- pos (mistty--bol-pos-from pos)))
        (chars-from-end (- (point-max) (mistty--bol-pos-from pos))))
    (with-current-buffer mistty-term-buffer
      (move-marker mistty-sync-marker (- (point-max) chars-from-end)))
    (with-current-buffer mistty-work-buffer
      (when (> mistty-cmd-start-marker mistty-sync-marker)
        (let ((inhibit-read-only t))
          (remove-text-properties mistty-sync-marker mistty-cmd-start-marker '(read-only t))))
      (let* ((sync-pos (- (point-max) chars-from-end))
             (cmd-start-pos (+ sync-pos chars-from-bol)))
        (move-marker mistty-sync-marker sync-pos)
        (move-marker mistty-cmd-start-marker cmd-start-pos)
        (move-overlay mistty-sync-ov sync-pos (point-max))
        (when (and set-prompt (> cmd-start-pos sync-pos))
          (let ((inhibit-read-only t))
            (add-text-properties sync-pos cmd-start-pos
                                 '(mistty prompt
                                         field 'mistty-prompt
                                         rear-nonsticky t))
            (add-text-properties sync-pos cmd-start-pos
                                 '(read-only t front-sticky t))))))))

(defun mistty-send-raw-string (str)
  (when (and str (not (zerop (length str))))
    (with-current-buffer mistty-term-buffer
      (term-send-raw-string str))))

(defun mistty--at-prompt-1 (&optional inexact)
  (let ((pmark (mistty-pmark)))
    (if inexact
        (or (>= (point) pmark)
            (>= (mistty--bol-pos-from (point))
                (mistty--bol-pos-from pmark)))
        (= (point) pmark))))

(defun mistty--bol-pos-from (pos &optional n)
  (save-excursion
    (goto-char pos)
    (let ((inhibit-field-text-motion t))
      (line-beginning-position n))))

(defun mistty--eol-pos-from (pos)
  (save-excursion
    (goto-char pos)
    (let ((inhibit-field-text-motion t))
      (line-end-position))))

(defun mistty-send-command ()
  "Send the current command to the shell."
  (interactive)
  (goto-char (mistty-pmark))
  (setq mistty--point-follows-next-pmark t)
  (mistty-send-raw-string "\C-m"))

(defun mistty-send-tab ()
  "Send TAB to the shell."
  (interactive)
  (mistty-before-positional)
  (setq mistty--point-follows-next-pmark t)
  (mistty-send-raw-string "\t"))

(defun mistty-send-backspace ()
  "Send DEL to the shell."
  (interactive)
  (when (get-pos-property (point) 'read-only)
    (signal 'text-read-only nil))
  (mistty-before-positional)
  (setq mistty--point-follows-next-pmark t)
  (mistty-send-raw-string "\b"))

(defun mistty-self-insert-command (n &optional key)
  (interactive "p")
  (when (get-pos-property (point) 'read-only)
    (signal 'text-read-only nil))
  (setq mistty--point-follows-next-pmark t)
  (mistty-before-positional)
  (mistty-send-raw-string (make-string (or n 1 ) (or key (mistty-last-key)))))

(defun mistty-send-raw-key (n)
  (interactive "p")
  (let ((key (mistty-last-key)))
    (when (mistty-positional-key-p key)
      (mistty-before-positional))
    (mistty-send-raw-string (make-string n key))))

(defun mistty-last-key ()
  (let ((keys (this-command-keys)))
    (aref keys (1- (length keys)))))

(defun mistty-delchar-or-maybe-eof (n)
  (interactive "p")
  (if (or
       (zerop (length
               (replace-regexp-in-string
                "[[:space:]]+"
                ""
                (buffer-substring-no-properties
                 mistty-cmd-start-marker (mistty--eol-pos-from (point))))))
       (progn
         (mistty-before-positional)
         (not (mistty-on-prompt-p (point)))))
      (mistty-send-raw-string (make-string n ?\C-d))
    (delete-char n)))

(defun mistty--modification-hook (_ov is-after orig-beg orig-end &optional old-length)
  (when (and is-after
             mistty-cmd-start-marker
             (>= orig-end mistty-cmd-start-marker))
    (let ((inhibit-read-only t)
          (beg (max orig-beg mistty-cmd-start-marker))
          (end (max orig-end mistty-cmd-start-marker))
          (old-end (max (+ orig-beg old-length) mistty-cmd-start-marker))
          shift pos)
      ;; Mark the text that was inserted
      (put-text-property beg end 'mistty-change '(inserted))

      ;; Update the shift value of everything that comes after.
      (setq shift (- old-end end))
      (setq pos end)
      (while (< pos (point-max))
        (let ((next-pos (next-single-property-change pos 'mistty-change (current-buffer) (point-max))))
          (pcase (get-text-property pos 'mistty-change)
            (`(shift ,old-shift)
             (put-text-property pos next-pos 'mistty-change `(shift ,(+ old-shift shift))))
            ('() (put-text-property pos next-pos 'mistty-change `(shift ,shift))))
          (setq pos next-pos)))
      (when (> old-end (point-max))
        (setq mistty--deleted-point-max t)))))

(defun mistty--collect-modifications ()
  (let ((changes nil)
        (last-shift 0)
        (intervals (mistty--collect-modification-intervals)))
    (while intervals
      (pcase intervals
        ;; insert in the middle, possibly replacing a section of text
        (`((,start inserted) (,end shift ,end-shift) . ,_)
         (push (list (+ start last-shift)
                     (buffer-substring-no-properties start end)
                     (- (+ end end-shift) (+ start last-shift)))
               changes)
         ;; processed 2 entries this loop, instead of just 1
         (setq intervals (cdr intervals)))

        ;; insert at end, delete everything after
        (`((,start inserted) (,end deleted-to-end))
         (push (list (+ start last-shift)
                     (buffer-substring-no-properties start end)
                     -1)
               changes)
         ;; processed 2 entries this loop, instead of just 1
         (setq intervals (cdr intervals)))

        ;; insert at end
        (`((,start inserted))
         (push (list (+ start last-shift)
                     (buffer-substring-no-properties start (point-max))
                     0)
               changes))

        ;; delete a section of original text
        ((and `((,pos shift ,shift) . ,_)
              (guard (> shift last-shift)))
         (push (list (+ pos last-shift)
                     ""
                     (- shift last-shift))
               changes))

        ;; delete to the end of the original text
        (`((,pos deleted-to-end))
         (push (list (+ pos last-shift) "" -1)
               changes)))
      
      ;; prepare for next loop
      (pcase (car intervals)
        (`(,_ shift ,shift) (setq last-shift shift)))
      (setq intervals (cdr intervals)))
    changes))

(defun mistty--collect-modification-intervals ()
  (save-excursion
    (save-restriction
      (narrow-to-region mistty-cmd-start-marker (point-max))
      (let ((last-point (point-min))
            intervals last-at-point )
        (goto-char last-point)
        (while (let ((at-point (get-text-property (point) 'mistty-change)))
                 (when last-at-point
                   (push `(,last-point . ,last-at-point) intervals))
                 (setq last-at-point at-point)
                 (setq last-point (point))
                 (goto-char (next-single-property-change (point) 'mistty-change (current-buffer) (point-max)))
                 (< (point) (point-max))))
        (when last-at-point
          (push `(,last-point . ,last-at-point) intervals))
        (when mistty--deleted-point-max
          (push `(,(point-max) deleted-to-end) intervals))
        (setq mistty--deleted-point-max nil)
        (let ((inhibit-read-only t)
              (inhibit-modification-hooks t))
          (remove-text-properties (point-min) (point-max) '(mistty-change t)))
        
        (nreverse intervals)))))
  
(defun mistty--replay-modification (orig-beg content old-length)
  (let* ((pmark (mistty-pmark))
         (beg orig-beg)
         (end (+ orig-beg (length content)))
         (old-end (if (> old-length 0) (+ orig-beg old-length) (mistty--from-pos-of
                                                                (with-current-buffer mistty-term-buffer (point-max))
                                                                mistty-term-buffer))))
    (when (> end beg)
      (mistty--send-and-wait (mistty--move-str pmark beg))
      (setq pmark (mistty-pmark))
      ;; pmark is as close to beg as we can make it
      
      ;; We couldn't move pmark as far back as beg. Presumably, the
      ;; process mark points to the leftmost modifiable position of
      ;; the command line. Update the sync marker to start sync there
      ;; from now on and avoid getting this hook called unnecessarily.
      ;; This is done from inside the term buffer as the modifications
      ;; of the work buffer could interfere. TODO: What if the process
      ;; is just not accepting any input at this time? We might move
      ;; sync mark to far down.
      (when (> (mistty--distance-on-term beg pmark) 0)
        (mistty--move-sync-mark pmark 'set-prompt))
      
      (setq beg (max beg pmark)))
    
    (when (> old-end beg)
      (mistty--send-and-wait (mistty--move-str pmark old-end))
      (setq pmark (mistty-pmark))
      (setq old-end (max beg (min old-end pmark))))

    (let ((replay-seq
           (concat
            (when (> old-end beg)
              (mistty--repeat-string
               (mistty--distance-on-term beg old-end) "\b"))
            (when (> end beg)
              (mistty--maybe-bracketed-str
               (substring content (max 0 (- beg orig-beg)) (min (length content) (max 0 (- end orig-beg)))))))))
      (if replay-seq
          (progn
            (when (= (point) end)
              (setq mistty--point-follows-next-pmark t))
            (mistty-send-raw-string replay-seq)
            (when (accept-process-output mistty-term-proc 1 nil t)
              (while (accept-process-output mistty-term-proc 0 nil t))))
        
        ;; update the work buffer in case unrelated changes were sent during mistty--send-and-wait
        (mistty--term-to-work)))))

(defun mistty--send-and-wait (str)
  (when (and str (not (zerop (length str))))
    (let ((mistty--inhibit-sync t))
      (mistty-send-raw-string str)
      (when (accept-process-output mistty-term-proc 1 nil t) ;; TODO: tune the timeout
        (while (accept-process-output mistty-term-proc 0 nil t))))))

(defun mistty--move-str (from to)
  (let ((diff (mistty--distance-on-term from to)))
    (if (zerop diff)
        nil
      (mistty--repeat-string
       (abs diff)
       (if (< diff 0) mistty-left-str mistty-right-str)))))

(defun mistty--safe-pos (pos)
  (min (point-max) (max (point-min) pos)))

(defun mistty--distance-on-term (beg end)
  "Compute the number of cursor moves necessary to get from BEG to END.

This function skips over the `term-line-wrap' newlines introduced
by term as if they were not here.

While it takes BEG and END as work buffer positions, it looks in
the term buffer to figure out, so it's important for the BEG and
END section to be valid in the term buffer."
  (with-current-buffer mistty-term-buffer
    (let ((beg (mistty--safe-pos (mistty--from-pos-of (min beg end) mistty-work-buffer)))
          (end (mistty--safe-pos (mistty--from-pos-of (max beg end) mistty-work-buffer)))
          (sign (if (< end beg) -1 1)))
      (let ((pos beg) (nlcount 0))
        (while (and (< pos end) (setq pos (text-property-any pos end 'term-line-wrap t)))
          (setq pos (1+ pos))
          (setq nlcount (1+ nlcount)))
        (* sign (- (- end beg) nlcount))))))

(defun mistty--repeat-string (count elt)
  (let ((elt-len (length elt)))
    (if (= 1 elt-len)
        (make-string count (aref elt 0))
      (let ((str (make-string (* count elt-len) ?\ )))
        (dotimes (i count)
          (dotimes (j elt-len)
            (aset str (+ (* i elt-len) j) (aref elt j))))
        str))))

(defun mistty-next-prompt (n)
  (interactive "p")
  (let (found)
    (dotimes (_ n)
      (if (setq found (text-property-any (point) (point-max) 'mistty 'prompt))
          (goto-char (or (next-single-property-change found 'mistty) (point-max)))
        (error "No next prompt")))))

(defun mistty-previous-prompt (n)
  (interactive "p")
  (dotimes (_ n)
    (unless (text-property-search-backward 'mistty 'prompt)
      (error "No previous prompt"))))

(defun mistty-pre-command ()
  (setq mistty--old-point (point)
        mistty--inhibit-sync t))

(defun mistty-post-command ()
  (setq mistty--inhibit-sync nil)
  
  ;; Show cursor again if the command moved the point.
  (when (and mistty--old-point (/= (point) mistty--old-point))
    (setq cursor-type t))
  
  (run-at-time 0 nil #'mistty-post-command-1 mistty-work-buffer))

(defun mistty-post-command-1 (buf)
  ;; replay modifications recorded during the command
  (mistty--with-live-buffer buf
    (when (and (process-live-p mistty-term-proc)
               (buffer-live-p mistty-term-buffer))
      (let ((changes (save-excursion (mistty--collect-modifications))))
        (dolist (c changes)
          (apply #'mistty--replay-modification c)))))

  ;; move process mark to follow point
  (when (and mistty--old-point
             (/= (point) mistty--old-point)
             (markerp mistty-sync-marker)
             (>= (point) mistty-sync-marker)
             (process-live-p mistty-term-proc)
             (buffer-live-p mistty-term-buffer)
             (mistty-on-prompt-p (point)))
    (mistty-send-raw-string (mistty--move-str (mistty-pmark) (point)))))

(defun mistty--window-size-change (&optional _win)
  (when (process-live-p mistty-term-proc)
    (let* ((adjust-func (or (process-get mistty-term-proc 'adjust-window-size-function)
                            window-adjust-process-window-size-function))
           (size (funcall adjust-func mistty-term-proc (get-buffer-window-list))))
      (when size
        (mistty--set-process-window-size (car size) (cdr size))))))

(defun mistty--set-process-window-size (width height)
  (mistty--with-live-buffer mistty-term-buffer
    (set-process-window-size mistty-term-proc height width)
    (term-reset-size height width)))

(defun mistty--enter-fullscreen (proc terminal-sequence)
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (mistty--detach 'keep-sync-markers)
    (setq mistty-fullscreen t)

    (save-excursion
      (goto-char (point-max))
      (insert mistty-fullscreen-mode-message))
    
    (let ((bufname (buffer-name)))
      (rename-buffer (generate-new-buffer-name (concat bufname " scrollback")))
      (with-current-buffer mistty-term-buffer
        (local-set-key [remap term-line-mode] #'mistty-switch-to-scrollback-buffer)
        (rename-buffer bufname)
        (turn-on-font-lock)))
    (mistty--replace-buffer-everywhere mistty-work-buffer mistty-term-buffer)

    (message mistty-fullscreen-mode-message)

    (set-process-filter proc #'mistty--fs-process-filter)
    (set-process-sentinel proc #'mistty--fs-process-sentinel)
    
    (when (length> terminal-sequence 0)
      (funcall (process-filter proc) proc terminal-sequence))))

(defun mistty--leave-fullscreen (proc terminal-sequence)
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (setq mistty-fullscreen nil)

    (mistty--attach (process-buffer proc))
    
    (let ((bufname (buffer-name mistty-term-buffer)))
      (with-current-buffer mistty-term-buffer
        (rename-buffer (generate-new-buffer-name (concat " mistty tty " bufname))))
      (rename-buffer bufname))

    (mistty--replace-buffer-everywhere mistty-term-buffer mistty-work-buffer)
    (with-current-buffer mistty-term-buffer
      (font-lock-mode -1))

    (when (length> terminal-sequence 0)
      (funcall (process-filter proc) proc terminal-sequence))))

(defun mistty--replace-buffer-everywhere (oldbuf newbuf)
  (walk-windows
   (lambda (win)
     (let ((prev-buffers (window-prev-buffers win))
           (modified nil))
       (when (eq (window-buffer win) oldbuf)
         (set-window-buffer win newbuf)
         (setq modified t))
       (dolist (entry prev-buffers)
         (when (eq (car entry) oldbuf)
           (setcar entry newbuf)
           (setq modified t)))
       (when modified
         (set-window-prev-buffers win prev-buffers))))))

(defun mistty-switch-to-fullscreen-buffer ()
  (interactive)
  (if (and mistty-fullscreen (buffer-live-p mistty-term-buffer))
      (switch-to-buffer mistty-term-buffer)
    (error "No fullscreen buffer available.")))

(defun mistty-switch-to-scrollback-buffer ()
  (interactive)
  (if (and (buffer-live-p mistty-work-buffer)
           (buffer-local-value 'mistty-fullscreen mistty-work-buffer))
      (switch-to-buffer mistty-work-buffer)
    (error "No scrollback buffer available.")))

(defun mistty-positional-key-p (key)
  (seq-contains mistty-positional-keys key))

(defun mistty-on-prompt-p (pos)
  (and (> pos mistty-cmd-start-marker)
       (or mistty-bracketed-paste
           (and 
            (> mistty-cmd-start-marker mistty-sync-marker)
            (>= pos mistty-cmd-start-marker)
            (<= pos (mistty--eol-pos-from mistty-cmd-start-marker))))))

(defun mistty-before-positional ()
  (let ((pmark (mistty-pmark)))
    (when (and (not (= pmark (point)))
               (not (mistty-on-prompt-p (point)))
               (mistty--possible-prompt-p)
               (mistty--possible-prompt-contains (point)))
      (mistty--realize-possible-prompt)
      (mistty-send-raw-string (mistty--move-str pmark (point))))))

(defun mistty--realize-possible-prompt ()
  (pcase mistty--possible-prompt
    (`(,_ ,end ,_)
     (mistty--move-sync-mark end 'set-prompt))))

(defun mistty--possible-prompt-p ()
  (pcase mistty--possible-prompt
    (`(,start ,end ,content)
     (let ((pmark (mistty-pmark)))
       (and (> start mistty-cmd-start-marker)
            (>= pmark end)
            (<= pmark (mistty--eol-pos-from start))
            (string= content (buffer-substring-no-properties start end)))))))

(defun mistty--possible-prompt-contains (pos)
  (pcase mistty--possible-prompt
    (`(,start ,line-start ,_)
     (and (>= pos line-start) (<= pos (mistty--eol-pos-from start))))))

;; (defun mistty--possible-prompt-intersects (range-start range-end)
;;   (pcase mistty--possible-prompt
;;     (`(,prompt-start ,line-start ,_)
;;      (let ((line-end (mistty--eol-pos-from prompt-start)))
;;        (and (> range-end line-start) (<= range-start line-end))))))

(provide 'mistty)

;;; mistty.el ends here
