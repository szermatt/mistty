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

;;; Code:

(defvar-local oterm-work-buffer nil)
(defvar-local oterm-term-buffer nil)
(defvar-local oterm-term-proc nil)
(defvar-local oterm-sync-marker nil)
(defvar-local oterm-sync-ov nil)
(defvar-local oterm-bracketed-paste nil)
(defvar oterm--inhibit-sync nil)

(defconst oterm-left-str "\eOD")
(defconst oterm-right-str "\eOC")
(defconst oterm-bracketed-paste-start-str "\e[200~")
(defconst oterm-bracketed-paste-end-str "\e[201~")

(defvar oterm-mode-map
  (let ((oterm-mode-map (make-sparse-keymap)))
    (define-key oterm-mode-map (kbd "C-c C-c") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-z") 'oterm-send-raw-key)
    (define-key oterm-mode-map (kbd "C-c C-\\") 'oterm-send-raw-key)
    oterm-mode-map))

(defvar oterm-prompt-map
  (let ((oterm-prompt-map (make-sparse-keymap)))
    (define-key oterm-prompt-map (kbd "RET") 'oterm-send-command-if-at-prompt)
    (define-key oterm-prompt-map [S-return] 'newline)
    (define-key oterm-prompt-map (kbd "TAB") 'oterm-send-tab-if-at-prompt)
    (define-key oterm-prompt-map (kbd "C-d") 'oterm-delchar-or-maybe-eof)
    oterm-prompt-map))

(define-derived-mode oterm-mode fundamental-mode "One Term" "Major mode for One Term."
  (let ((work-buffer (current-buffer))
        (term-buffer (generate-new-buffer (concat " oterm tty " (buffer-name)) 'inhibit-buffer-hooks)))
    (setq oterm-work-buffer work-buffer)
    (setq oterm-term-buffer term-buffer)
    (setq oterm-sync-marker (copy-marker (point-min)))
    (setq oterm-sync-ov (make-overlay (point-min) (point-max) nil nil 'rear-advance))
    (overlay-put oterm-sync-ov 'face '(background-color . "black"))
    (overlay-put oterm-sync-ov 'keymap oterm-prompt-map)
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
    (add-hook 'kill-buffer-hook 'oterm--kill-term-buffer nil t)))

(defun oterm--kill-term-buffer ()
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
      (set-process-filter proc #'oterm-process-filter)
      (set-process-sentinel proc #'oterm-process-sentinel))))

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
  (let ((inhibit-modification-hooks t)
        (old-pmark (marker-position (process-mark proc)))
        (work-buffer (process-get proc 'oterm-work-buffer))
        (term-buffer (process-get proc 'oterm-term-buffer)))
    (oterm-emulate-terminal proc str)
    (when (buffer-live-p term-buffer)
      (with-current-buffer term-buffer
        (goto-char (process-mark proc))))
    (when (and (not oterm--inhibit-sync) (buffer-live-p work-buffer))
      (with-current-buffer work-buffer
        (when (buffer-live-p oterm-term-buffer)
              (oterm--term-to-work)
              (when (/= old-pmark (marker-position (process-mark proc)))
                (oterm--pmarker-to-point))
              )))))

(defun oterm-emulate-terminal (proc str)
  "Handle special terminal codes, then call `term-emlate-terminal'.

This functions intercepts some extented sequences term.el. This
all should rightly be part of term.el."
  (let ((start 0) found)
    (while (setq found (string-match "\e\\[\\(\\?2004[hl]\\)" str start))
      (let ((ext (match-string 1 str))
            (next (match-end 0)))
        (term-emulate-terminal proc (substring str start next))
        (let ((buf (process-get proc 'oterm-work-buffer)))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (cond
               ((equal ext "?2004h")
                (setq oterm-bracketed-paste t))
               ((equal ext "?2004l")
                (setq oterm-bracketed-paste nil))))))
        (setq start next)))
    (let ((final-str (substring str start)))
      (unless (zerop (length final-str))
        (term-emulate-terminal proc final-str)))))

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

(defun oterm--pmarker-to-point ()
  (when (buffer-live-p oterm-term-buffer)
    (with-current-buffer oterm-work-buffer
      (goto-char (+ oterm-sync-marker (with-current-buffer oterm-term-buffer
                                        (- (point) oterm-sync-marker)))))))

(defun oterm--term-to-work ()
  (with-current-buffer oterm-term-buffer
    (save-restriction
      (narrow-to-region oterm-sync-marker (point-max-marker))
      (with-current-buffer oterm-work-buffer
        (let ((saved-undo buffer-undo-list))
          (save-excursion
            (save-restriction
              (narrow-to-region oterm-sync-marker (point-max-marker))
              (let ((inhibit-modification-hooks t))
                (replace-buffer-contents oterm-term-buffer))))
          (setq buffer-undo-list saved-undo)))))
  ;; Next time, only sync the visible portion of the terminal.
  (with-current-buffer oterm-term-buffer
    (when (< oterm-sync-marker term-home-marker)
      (oterm--move-sync-mark term-home-marker))))

(defun oterm--move-sync-mark (pos)
  (let ((chars-from-end (- (point-max)
                           (save-excursion (goto-char pos) (line-beginning-position)))))
    (with-current-buffer oterm-term-buffer
      (move-marker oterm-sync-marker (- (point-max) chars-from-end)))
    (with-current-buffer oterm-work-buffer
      (let ((sync-pos (- (point-max) chars-from-end)))
        (move-marker oterm-sync-marker sync-pos)
        (move-overlay oterm-sync-ov sync-pos (point-max)))))

  ;; Truncate the term buffer, since scrolling back is available on
  ;; the work buffer anyways. This has to be done now, after syncing
  ;; the marker, and not in term-emulate-terminal, which is why
  ;; term-buffer-maximum-size is set to 0.
  (with-current-buffer oterm-term-buffer
    (let ((inhibit-read-only t))
      (save-excursion
        (goto-char oterm-sync-marker)
        (forward-line -5)
        (delete-region (point-min) (point))))))

(defun oterm-send-raw-string (str)
  (with-current-buffer oterm-term-buffer
    (term-send-raw-string str)))

(defun oterm--at-prompt-1 (&optional inexact)
  (let ((pmark (oterm--pmark)))
    (if inexact
        (or (>= (point) pmark)
            (>= (save-excursion (line-beginning-position))
                (save-excursion (goto-char pmark) (line-beginning-position))))
        (= (point) pmark))))

(defun oterm--at-prompt-p (&optional inexact)
  "Figure out whether a command should be sent to the terminal.

Terminal commands should be sent to the terminal if the point is
at the prompt otherwise it should be applied directly to the work
buffer."
  (if (oterm--at-prompt-1 inexact)
      t
    (oterm--send-and-wait (oterm--move-pmark-str (point)))
      (prog1 (oterm--at-prompt-1 inexact)
        (let ((pmark (oterm--pmark)))
          (when (> pmark (point))
            (oterm--move-sync-mark pmark)))
        (oterm--term-to-work))))

(defun oterm-send-command-if-at-prompt ()
  "Send the current command to the shell if point is at prompt, otherwise
send a newline."
  (interactive)
  (if (oterm--at-prompt-p 'inexact)
      (oterm-send-command)
    (newline)))

(defun oterm-send-raw-key ()
  (interactive)
  (let ((keys (this-command-keys)))
    (oterm-send-raw-string (make-string 1 (aref keys (1- (length keys)))))))

(defun oterm-delchar-or-maybe-eof (arg)
  (interactive "p")
  (if (or (eobp) (progn
                   (oterm--send-and-wait (oterm--move-pmark-str (point-max)))
                   (prog1 (= (oterm--pmark) (point))
                     (oterm--term-to-work))))
      (oterm-send-raw-string (kbd "C-d"))
    (delete-char arg)))

(defun oterm-send-command ()
  "Send the current command to the shell."
  (interactive)
  (oterm-send-raw-string "\n"))

(defun oterm-send-tab-if-at-prompt ()
  (interactive)
  (if (oterm--at-prompt-p)
      (oterm-send-raw-string "\t")
    (call-interactively 'indent-for-tab-command)))

(defun oterm-send-self-if-at-prompt ()
  "Send the current key if the point is at prompt, otherwise
execute the remapped command."
  (interactive)
  (if (oterm--at-prompt-p)
      (let ((keys (this-command-keys)))
        (oterm-send-raw-string (make-string 1 (aref keys (1- (length keys))))))
    (call-interactively this-original-command)))

(defun oterm--modification-hook (_ov is-after beg end &optional old-length)
  (when (and (buffer-live-p oterm-term-buffer) is-after)
    ;; Attempt to replay the change in the terminal.
    (let ((pmark (oterm--pmark))
          (initial-point (point)))
      (oterm--send-and-wait (oterm--move-str pmark beg))
      (setq pmark (oterm--pmark))
      ;; pmark is as close to beg as we can make it

      (when (> pmark beg)
        ;; We couldn't move pmark as far back as beg. Presumably,
        ;; pmark is now at the leftmost modifiable position of the
        ;; command line. Update the sync marker to start sync there
        ;; from now on and avoid getting this hook called
        ;; unnecessarily. TODO: What if the process is just not
        ;; accepting any input at this time? We might move sync mark
        ;; to far down.
        (oterm--move-sync-mark pmark))

      (when (>= pmark beg)
        ;; Replay the portion of the change that we think we can
        ;; replay.
        ;;
        ;; TODO: what if [beg, end] start in the command-line portion
        ;; of the screen and end in the portion of the screen
        ;; containing zsh completion? We'd be "replaying" zsh
        ;; completion results.
        (let ((content-to-replay (if (<= pmark end) (buffer-substring-no-properties pmark end) ""))
              (chars-to-delete (- (or old-length 0) (- pmark beg))))
          (oterm--send-and-wait
           (concat
            (when (> chars-to-delete 0)
              (concat (oterm--repeat-string chars-to-delete oterm-right-str)
                      (oterm--repeat-string chars-to-delete "\b")))
            (oterm--maybe-bracketed-str content-to-replay)))))

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

(provide 'oterm)

;;; oterm.el ends here
