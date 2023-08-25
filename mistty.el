;;; mistty.el --- One Terminal -*- lexical-binding: t -*-

;; Copyright (C) 2023 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Version: 0.9
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, unix
;; URL: http://github.com/szermatt/mixterm

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

;;; Commentary:
;;

(require 'term)
(require 'seq)
(require 'subr-x)
(require 'pcase)
(require 'generator)
(require 'text-property-search)
(require 'fringe)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-changeset)
(require 'mistty-term)
(require 'mistty-util)
(require 'mistty-log)
(require 'mistty-queue)

;;; Code:

;; Customization:
(defgroup mistty nil
  "Shell/Comint alternative with a real terminal."
  :group 'shell)

(defcustom mistty-prompt-re
  "[^[:alnum:][:cntrl:][:blank:]][[:blank:]]?$"
  "Regexp used to identify prompts.

New, empty lines that might be prompts are evaluated against this
regexp. This regexp should match something that looks like the
end of a prompt with no commands.

Customizing this string is usually not necessary, but if you
notice that MisTTY doesn't detect prompts or detects prompts that
aren't there, you might want to adapt this regexp to match only
the prompts of the commands you actually use."
  :type '(regexp)
  :group 'mistty)

(defcustom mistty-variables-to-copy
  '(default-directory
    ansi-osc-window-title)
  "List of buffer-local variables to copy from term to mistty.

These are typically variable set from OSC handlers, configured in
`mistty-osc-handlers'. As OSC handlers run in the terminal
buffer, they need to be copied to be available in the main MisTTY
buffer."
  :type '(list variable)
  :group 'mistty)

(defcustom mistty-positional-keys "\t\C-d\C-w\C-t\C-k\C-y"
  "Set of control characters that are defined as positional.

A key is defined as positional if it traditionally have an effect
that modifies what is displayed on the terminal in a way that
depends on where the cursor is on the terminal.

Graphical characters are always considered positional, so don't
appear on this list.

MisTTY moves the cursor to the point and before sending a
positional key to the terminal."
  :type '(string)
  :group 'mistty)

(defcustom mistty-fringe-enabled t
  "If non-nil, highlight the synced region with a left fringe or margin.

This makes the synced region visible, which is useful as
modifications made inside of the synced region are treated
differently from modifications made inside of the synced region."
  :type '(boolean)
  :group 'mistty)

(defface mistty-fringe-face '((t (:foreground "purple")))
  "Color of the left fringe or margin that highlights the synced region.

On the terminal, a margin is created instead of a fringe. This
face is used as well in such case.

You can turn this off completely by setting
`mistty-fringe-enabled'."
  :group 'mistty)

(defface mistty-prompt-face nil
  "Face that highlights the current, detected prompt.

This is generally only set when debugging prompt detection."
  :group 'mistty)

(defcustom mistty-buffer-maximum-size 8192
  "The maximum size in lines for MisTTY buffers

Buffers that grow larger than the given size might be truncated.
Set to 0 to disable truncation."
  :group 'mistty
  :type 'natnum)

(defvar mistty-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-n") 'mistty-next-prompt)
    (define-key map (kbd "C-c C-p") 'mistty-previous-prompt)
    (define-key map (kbd "C-c C-j") 'mistty-switch-to-fullscreen-buffer)
    (define-key map (kbd "C-c C-q") 'mistty-send-key-sequence)
    (define-key map (kbd "C-e") 'mistty-end-of-line-or-goto-cursor)

    ;; mistty-send-last-key makes here some globally useful keys
    ;; available in mistty-mode buffers. More specific keys can be
    ;; input using C-q while mistty-prompt-map is active.
    (define-key map (kbd "C-c C-c") 'mistty-send-last-key)
    (define-key map (kbd "C-c C-z") 'mistty-send-last-key)
    (define-key map (kbd "C-c C-\\") 'mistty-send-last-key)
    (define-key map (kbd "C-c C-g") 'mistty-send-last-key)
    map)
  "Keymap of `mistty-mode'.

This map is active whenever the current buffer is in MisTTY mode.")

(defvar mistty-send-last-key-map '(keymap (t . mistty-send-last-key))
  "Keymap that sends everything to the terminal using `mistty-send-last-key'.")

(defvar mistty-prompt-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'mistty-send-command)
    (define-key map (kbd "TAB") 'mistty-send-key)
    (define-key map (kbd "DEL") 'mistty-send-key)
    (define-key map (kbd "C-d") 'mistty-send-key)
    (define-key map (kbd "C-a") 'mistty-send-beginning-of-line)
    (define-key map (kbd "C-e") 'mistty-send-end-of-line)

    ;; While in a shell, when bracketed paste is on, this allows
    ;; sending a newline that won't submit the current command. This
    ;; is handy for editing multi-line commands in bash.
    (define-key map (kbd "S-<return>") 'newline)

    ;; While on the prompt, "quoted-char" turns into "send the next
    ;; key directly to the terminal".
    (define-key map (kbd "C-q") mistty-send-last-key-map)

    ;; Don't bother capturing single key-stroke modifications and
    ;; replaying them; just send them to the terminal. This works even
    ;; when the terminal doesn't accept editing.
    (define-key map [remap self-insert-command] 'mistty-send-key )
    map)
  "Keymap active on the part of `mistty-mode' synced with the terminal.

This map is active only on the portion of a MisTTY mode buffer
that is kept in sync with the terminal. Modifications made on
this portion of the buffer are copied to the terminal, when
possible.

Consider adding key bindings into `mistty-mode-map' instead so
they're always available; this is more straightforward.

If you add key bindings into this map, you might also want to
have access to the same bindings on fullscreen mode, so consider
adding bindings to `mistty-fullscreen-map' as well.

It is used to send most key strokes and some keys directly to the
terminal.")

(defvar mistty-send-last-key-map '(keymap (t . mistty-send-last-key))
  "Keymap that sends everything to the terminal using `mistty-send-last-key'.

By default, it is bound to C-q in `mistty-prompt-map'.")

(defvar mistty-fullscreen-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map term-raw-map)

    (define-key map (kbd "C-q") mistty-send-last-key-map)
    (define-key map (kbd "C-c C-q") 'mistty-send-key-sequence)

    ;; Disable the "Terminal" menu; nothing that it contains should be
    ;; used on Term buffers used by MisTTY.
    (define-key map [menu-bar terminal] nil)

    ;; switching the term buffer to line mode would cause issues.
    (define-key map [remap term-line-mode] #'mistty-switch-to-scrollback-buffer )
    map)
  "Keymap active while in fullscreen mode.

While in fullscreen mode, the buffer is a `term-mode' with its
own keymaps (`term-mod-map' and `term-raw-map')

This map is applied in addition to these as a way of making key
mapping somewhat consistent between fullscreen and normal mode.")

;; Variables:
(defvar-local mistty-work-buffer nil
  "The main `mistty-mode' buffer.

This buffer keeps a modifiable history of all commands at the top
and a view of the terminal modified by the current command at the
bottom.

In normal mode, this is the buffer that's displayed to the user.
In fullscreen mode, this buffer is kept as historical scrollback
buffer that can be independently switched to with
`mistty-switch-to-scrollback-buffer'.

While there is normally a terminal buffer, available as
`mistty-term-buffer' as well as a process, available as
`mistty-proc' either or both of these might be nil, such as
after the process died.


This variable is available in both the work buffer and the term
buffer.")

(defvar-local mistty-term-buffer nil
  "Secondary `term-mode' buffer.

This buffer contains the current screen state, as drawn by the
different commands. In normal mode, changes made in this buffer
are normally copied to `mistty-work-buffer'. In fullscreen mode,
this is the buffer that's displayed to the user. In normal mode,
this buffer is hidden.

While there is normally a work buffer, available as
`mistty-work-buffer' as well as a process, available as
`mistty-proc` either or both of these might be nil in some
cases.

This variable is available in both the work buffer and the term
buffer.")

(defvar-local mistty-proc nil
  "The process that controls the terminal, usually a shell.

This process is associated to `mistty-term-buffer' and is set up
to output first to that buffer.

The process property `mistty-work-buffer' links the work buffer
and, for consistency, the process property `mistty-term-buffer'
links to the term buffer.

This variable is available in both the work buffer and the term
buffer.")


(defvar-local mistty-fullscreen nil
  "Whether MisTTY is in full-screen mode.

When MisTTY is in full-screen mode, this variable evaluates to
true, the `term-mode' buffer is the buffer shown to the user,
while the `mistty-mode' buffer is kept aside, detached from the
process.

This variable is available in both the work buffer and the term
buffer.")

(defvar-local mistty-goto-cursor-next-time nil
  "Non-nil if the point should be moved to the cursor.

This variable tells `mistty--refresh' that it should move
the point to the cursor next time it copies the state of the
terminal to the work buffer.

This variable is available in the work buffer.")

(defvar-local mistty--queue nil
  "A queue of data to send to the process; a mistty--queue struct.

See mistty-queue.el.")

(defvar-local mistty-sync-marker nil
  "A marker that links `mistty-term-buffer' to `mistty-work-buffer'.

The region of the terminal that's copied to the work buffer
starts at `mistty-sync-marker' and ends at `(point-max)' on both
buffers. The two markers must always be kept in sync and updated
at the same time.

This variable is available in both the work buffer and the term
buffer.")

(defvar-local mistty--cmd-start-marker nil
  "Mark the end of the prompt; the beginning of the command line.

The region [`mistty-sync-marker', `mistty--cmd-start-marker']
marks the prompt of the command line, if one was detected. If no
prompt was detected, this marker points to the same position as
`mistty-sync-marker'.

This variable is available in the work buffer.")

(defvar-local mistty--sync-ov nil
  "An overlay that covers the region [`mistty-sync-marker', `(point-max)'].

This overlay covers the region of the work buffer that's
currently kept in sync with the terminal. MisTTY tries to send
any modifications made to this region to the terminal for
processing. Such modification might be rejected and eventually
undone, accepted or accepted with modifications.

The special keymap `mistty-prompt-map' is active when the pointer
is on this overlay.

This variable is available in the work buffer.")

(defvar-local mistty--prompt-ov nil
  "An overlay that covers the current detected prompt, if any.")

(defvar-local mistty--old-point nil
  "The position of the point captured in `pre-command-hook'.

It is used in the `post-command-hook'.

This variable is available in the work buffer.")

(defvar-local mistty--end-prompt nil
  "End the prompt after the next refresh.

When this variable is non-nil, it contains a position in the work
buffer that's on the last line of the current prompt. The line
after that is going to be a process output or a new prompt.")

(defvar-local mistty--possible-prompt nil
  "Region of the work buffer identified as possible prompt.

This variable is either nil or a list that contains:
 - the start of the prompt, a position in the work buffer
 - the end of the prompt
 - the content of the prompt

While start and end points to positions in the work buffer, such
position might not contain any data yet - if they haven't been
copied from the terminal, or might contain data - if they have
since been modified.

This variable is available in the work buffer.")

(defvar-local mistty--cursor-after-last-refresh nil
  "The position of the cursor at the end of `mistty--refresh'.

This variable is meant for `mistty--refresh' to detect
whether the cursor has moved since its last call. It's not meant
to be modified or accessed by other functions.

This variable is available in the work buffer.")

(defvar-local mistty--inhibit-refresh nil
  "When non-nil, prevent `mistty--refresh' from copying data.

When this variable is true, `mistty--refresh' does nothing
unless it is forced; it just sets
`mistty--need-refresh'. This is useful to temporarily
prevent changes to the terminal to be reflected to the work
buffer and shown to the user.

This variable is available in the work buffer.")

(defvar-local mistty--need-refresh nil
  "If true, the work buffer is known to be out-of-date.

This variable is set by `mistty--refresh' when copying data
from the terminal to the work buffer is disabled. It signals that
`mistty--refresh' should be called after setting
`mistty--refresh' to true again.

This variable is available in the work buffer.")

(defvar-local mistty--refresh-timer nil
  "A timer for calling refresh after some delay.

This is used to cover the case where modifications that should
cause changes are just ignored by the command.")

(defvar-local mistty--truncate-timer nil
  "An idle timer that'll truncate the buffer.

Truncation is configured by `mistty-buffer-maximum-size'.")

(eval-when-compile
  ;; defined in term.el
  (defvar term-home-marker))

(defconst mistty--ws "[:blank:]\n\r"
  "A character class that matches spaces and newlines, for MisTTY.")

(define-derived-mode mistty-mode fundamental-mode "misTTY" "Line-based TTY."
  :interactive nil
  (setq buffer-read-only nil)
  (setq mistty-work-buffer (current-buffer))

  ;; scroll down only when needed. This typically keeps the point at
  ;; the end of the window. This seems to be more in-line with what
  ;; commands such as more expect than the default Emacs behavior.
  (setq-local scroll-conservatively 1024)

  (when mistty-fringe-enabled
    (if (window-system)
        (unless (fringe-bitmap-p 'mistty-bar)
          (define-fringe-bitmap
            'mistty-bar (make-vector 40 7) nil 3 'center))
      
      ;; on a terminal, set margin width, and call set-window-buffer to make
      ;; sure it has taken effect.
      (setq left-margin-width 1)
      (let ((buf (current-buffer)))
        (dolist (win (get-buffer-window-list buf))
          (set-window-buffer win buf))))))

(put 'mistty-mode 'mode-class 'special)

(defsubst mistty--require-work-buffer ()
  "Asserts that the current buffer is the work buffer."
  (unless (eq mistty-work-buffer (current-buffer)) (error "Work buffer required")))

(defsubst mistty--require-term-buffer ()
  "Asserts that the current buffer is the term buffer."
  (unless (eq mistty-term-buffer (current-buffer)) (error "Term buffer required")))

(defun mistty--exec (program &rest args)
  "Execute PROGRAM ARGS in the current buffer.

The buffer is switched to `mistty-mode'."
  (mistty-mode)
  (mistty--attach
   (mistty--create-term
    (concat " mistty tty " (buffer-name)) program args
    ;; local-map
    mistty-fullscreen-map
    ;; width
    (- (window-max-chars-per-line) left-margin-width)
    ;; height
    (floor (window-screen-lines))))
  (mistty--update-mode-lines))

(defun mistty--attach (term-buffer)
  "Attach the current `mistty-mode' buffer to TERM-BUFFER.

This sets `mistty-term-buffer' to TERM-BUFFER in the current
buffer and `mistty-proc' to that buffer's process."
  (let ((work-buffer (current-buffer))
        (proc (get-buffer-process term-buffer)))

    (when proc
      (process-put proc 'mistty-work-buffer work-buffer)
      (process-put proc 'mistty-term-buffer term-buffer))

    (setq mistty-proc proc)
    (setq mistty-term-buffer term-buffer)
    (setq mistty-sync-marker (mistty--create-or-reuse-marker mistty-sync-marker (point-max)))
    (setq mistty--cmd-start-marker (copy-marker mistty-sync-marker))
    (setq mistty--sync-ov (make-overlay mistty-sync-marker (point-max) nil nil 'rear-advance))
    (setq mistty--prompt-ov (make-overlay mistty-sync-marker mistty--cmd-start-marker))
    (setq mistty--queue (mistty--make-queue proc))

    (with-current-buffer term-buffer
      (setq mistty-proc proc)
      (setq mistty-work-buffer work-buffer)
      (setq mistty-term-buffer term-buffer)
      (setq mistty-sync-marker (mistty--create-or-reuse-marker mistty-sync-marker term-home-marker)))

    (overlay-put mistty--sync-ov 'keymap mistty-prompt-map)
    (overlay-put mistty--sync-ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put mistty--sync-ov 'insert-behind-hooks (list #'mistty--modification-hook))

    ;; highlight the synced region in the fringe or margin
    (when mistty-fringe-enabled
      (overlay-put
       mistty--sync-ov
       'line-prefix
       (propertize " " 'display
                   (if (window-system)
                       '(left-fringe mistty-bar mistty-fringe-face)
                     `((margin left-margin) ,(propertize "┃" 'face 'mistty-fringe-face))))))
    (overlay-put mistty--prompt-ov 'face 'mistty-prompt-face)

    (when proc
      (set-process-filter proc #'mistty--process-filter)
      (set-process-sentinel proc #'mistty--process-sentinel))

    (add-hook 'kill-buffer-hook #'mistty--kill-term-buffer nil t)
    (add-hook 'window-size-change-functions #'mistty--window-size-change nil t)
    (add-hook 'pre-command-hook #'mistty--pre-command nil t)
    (add-hook 'post-command-hook #'mistty--post-command nil t)

    (mistty--refresh)
    (when proc
      (mistty-goto-cursor))))

(defun mistty--create-or-reuse-marker (m initial-pos)
  "Create the marker M set to INITIAL-POS or move it to that position.

Returns M or a new marker."
  (if (not (markerp m))
      (copy-marker initial-pos)
    (when (= 1 (marker-position m))
      (move-marker m initial-pos))
    m))

(defun mistty--detach (&optional keep-sync-markers)
  "Detach the current `mistty-mode' buffer from its process.

If KEEP-SYNC-MARKERS is non-nil, leave `mistty-sync-marker' set
in both the work and term buffers."
  (mistty--require-work-buffer)

  (remove-hook 'kill-buffer-hook #'mistty--kill-term-buffer t)
  (remove-hook 'window-size-change-functions #'mistty--window-size-change t)
  (remove-hook 'pre-command-hook #'mistty--pre-command t)
  (remove-hook 'post-command-hook #'mistty--post-command t)

  (when mistty--queue
    (mistty--cancel-queue mistty--queue)
    (setq mistty--queue nil))
  (when mistty--sync-ov
    (delete-overlay mistty--sync-ov)
    (setq mistty--sync-ov nil))
  (when mistty-proc
    (set-process-filter mistty-proc #'term-emulate-terminal)
    (set-process-sentinel mistty-proc #'term-sentinel)
    (setq mistty-proc nil))
  (when mistty--cmd-start-marker
    (move-marker mistty--cmd-start-marker nil)
    (setq mistty--cmd-start-marker nil))
  (unless keep-sync-markers
    (when mistty-sync-marker
      (move-marker mistty-sync-marker nil)
      (setq mistty-sync-marker nil))
    (mistty--with-live-buffer mistty-term-buffer
      (move-marker mistty-sync-marker nil)
      (setq mistty-sync-marker nil))))

(defun mistty--kill-term-buffer ()
  "Kill-buffer-hook handler that kills `mistty-term-buffer'."
  (let ((term-buffer mistty-term-buffer))
    (mistty--detach)
    (mistty--update-mode-lines)
    (when (buffer-live-p term-buffer)
      (let ((kill-buffer-query-functions nil))
        (kill-buffer term-buffer)))))

(defun mistty-live-buffer-p (buffer)
  "Return the BUFFER if the buffer is a MisTTY buffer.

The process attached to the buffer must be live.

When in fullscreen mode, the main MisTTY buffer is actually a
term-mode buffer, not the scrollback buffer."
  (and
   (buffer-live-p buffer)
   (pcase (buffer-local-value 'major-mode buffer)
     ('mistty-mode (not (buffer-local-value 'mistty-fullscreen buffer)))
     ('term-mode (buffer-local-value 'mistty-fullscreen buffer)))
   (buffer-local-value 'mistty-proc buffer)
   (process-live-p (buffer-local-value 'mistty-proc buffer))
   ;; returns
   buffer))

(defun mistty-list-live-buffers ()
  "List of live MisTTY buffers, sorted."
  (sort (delq nil (mapcar #'mistty-live-buffer-p (buffer-list)))
        (lambda (a b) (string< (buffer-name a) (buffer-name b)))))

;;;###autoload
(defun mistty ()
  "Go to the next MisTTY buffer, or create a new one.

The first time this command is called, it creates a new MisTTY
buffer. Afterwards, this command goes to a MisTTY buffer. If
already on a MisTTY buffer, go to the next one or create another
one.

To create a new buffer unconditionally, call `mistty-create'."
  (interactive)
  (let ((existing (mistty-list-live-buffers)))
    (if (or current-prefix-arg         ; command prefix was given
            (null existing)            ; there are no mistty buffers
            (and (null (cdr existing)) ; the current buffer is the only mistty buffer
                 (eq (current-buffer) (car existing))))
        ;; create a new one
        (mistty-create)
      (mistty--goto-next existing))))

(defun mistty--goto-next (existing)
  "Go to the next buffer in EXISTING, skipping the current one."
  (let ((existing-tail (or (cdr (member (current-buffer) existing))
                           existing)))
    (if existing-tail
        (switch-to-buffer (car existing-tail))
      (error "No next mistty buffer"))))

;;;###autoload
(defun mistty-create ()
  "Create a new MisTTY buffer, running a shell.

The shell that is run can be configured by setting
`explicit-shell-file-name', `shell-file-name' or come implicitly
from the ESHELL or SHELL environment variables."
  (interactive)
  (with-current-buffer (generate-new-buffer "*mistty*")
    (mistty--exec (or explicit-shell-file-name
                      shell-file-name
                      (getenv "ESHELL")
                      (getenv "SHELL")))
    (switch-to-buffer (current-buffer))))

(defun mistty--process-sentinel (proc msg)
  "Process sentinel for MisTTY shell processes."
  (mistty--update-mode-lines proc)

  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-buffer proc)))
    (if (buffer-live-p work-buffer)
        (when (memq (process-status proc) '(signal exit))
          (while (accept-process-output proc 0 0 t))
          (term-sentinel proc msg)
          (with-current-buffer work-buffer
            (save-restriction
              (widen)
              (mistty--refresh)
              (mistty--detach)))
          (kill-buffer term-buffer)))
    ;; detached term buffer
    (term-sentinel proc msg)))

(defun mistty--fs-process-sentinel (proc msg)
  "Process sentinel for MisTTY shell processes in fullscreen mode."
  (mistty--update-mode-lines proc)

  (let ((process-dead (memq (process-status proc) '(signal exit)))
        (term-buffer (process-get proc 'mistty-term-buffer))
        (work-buffer (process-get proc 'mistty-work-buffer)))
    (cond
     ((and process-dead (buffer-live-p term-buffer) (buffer-live-p work-buffer))
      (mistty--leave-fullscreen proc "")
      (mistty--process-sentinel proc msg))
     ((and process-dead (not (buffer-live-p term-buffer)) (buffer-live-p work-buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (process-get proc 'mistty-work-buffer)))
      (term-sentinel proc msg))
     (t (term-sentinel proc msg)))))

(defun mistty--process-filter (proc str)
  "Process filter for MisTTY shell processes."
  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-get proc 'mistty-term-buffer)))
    (cond
     ;; detached term buffer
     ((or (not (buffer-live-p work-buffer)) (not (buffer-live-p term-buffer)))
      (term-emulate-terminal proc str))

     ;; switch to fullscreen
     ((string-match "\e\\[\\(\\??47\\|\\?104[79]\\)h" str)
      (let ((smcup-pos (match-beginning 0)))
        (mistty--process-filter proc (substring str 0 smcup-pos))
        (mistty--enter-fullscreen proc (substring str smcup-pos))))

     ;; reset
     ((string-match "\ec" str)
      (mistty-log "RESET")
      (let ((rs1-before-pos (match-beginning 0))
            (rs1-after-pos (match-end 0)))
        ;; The work buffer must be updated before sending the reset to
        ;; the terminal, or we'll lose data. This might interfere with
        ;; collecting and applying modifications, but then so would
        ;; reset.
        (mistty--with-live-buffer term-buffer
          (mistty--process-terminal-seq proc (substring str 0 rs1-before-pos)))
        (mistty--with-live-buffer work-buffer
          (setq mistty--need-refresh t)
          (mistty--cancel-queue mistty--queue) ; might call mistty--refresh
          (when mistty--need-refresh
            (mistty--refresh)))
        (mistty--with-live-buffer term-buffer
          (mistty--process-terminal-seq proc (substring str rs1-before-pos rs1-after-pos)))
        (mistty--with-live-buffer work-buffer
          (setq mistty-bracketed-paste nil))
        (mistty--reset-markers)
        (mistty--process-filter proc (substring str rs1-after-pos))))

     ;; normal processing
     (t
      (mistty-log "RECV[%s]" str)
      (mistty--with-live-buffer term-buffer
        (mistty--process-terminal-seq proc str))
      (mistty--with-live-buffer work-buffer
        (dolist (var mistty-variables-to-copy)
          (when-let (val (buffer-local-value var term-buffer))
            (make-local-variable var)
            (set var val)))

        (mistty--cancel-timeout mistty--queue)
        (when (or (and (mistty--queue-empty-p mistty--queue)
                       (not mistty-bracketed-paste))
                  (not (accept-process-output proc 0 0 t)))
          (mistty--refresh)
          (mistty--maybe-truncate-when-idle)
          (mistty--dequeue-with-timer mistty--queue)))))))

(defun mistty--process-terminal-seq (proc str)
  "Process STR, sent to PROC, then update MisTTY internal state."
  (mistty--require-term-buffer)
  (let ((old-sync-position (marker-position mistty-sync-marker))
        (old-last-non-ws (mistty--last-non-ws)))
    (mistty--emulate-terminal proc str mistty-work-buffer)
    (goto-char (process-mark proc))
    (when (or (/= mistty-sync-marker old-sync-position)
              (< (point) mistty-sync-marker))
      (mistty--reset-markers))
    (when (> (mistty--bol (point)) old-last-non-ws) ;; on a new line
      (mistty--detect-possible-prompt (point)))))

(defun mistty-goto-cursor ()
  "Move the point to the terminal's cursor."
  (interactive)
  (let ((cursor (mistty--safe-pos (mistty-cursor))))
    (goto-char cursor)
    (dolist (win (get-buffer-window-list mistty-work-buffer nil t))
      (mistty--recenter win))))

(defun mistty--recenter (win)
  "Make sure the cursor is visible in WIN and at the bottom of the screen."
  (with-current-buffer (window-buffer win)
    (when (and mistty-proc
               (or (eq (mistty-cursor) (window-point win))
                   (> (window-point win) (mistty--bol (point-max) -3))))
        (with-selected-window win
          (recenter (- (1+ (count-lines
                            (window-point win) (point-max)))) t)))))

(defun mistty--detect-possible-prompt (cursor)
  "Look for a new prompt at CURSOR and store its position.

This function updates `mistty--possible-prompt' after the content
of the terminal buffer has been updated."
  (mistty--require-term-buffer)
  (let* ((bol (mistty--bol cursor)))
    (when (and (> cursor bol)
               (>= cursor (mistty--last-non-ws))
               (string-match
                mistty-prompt-re
                (mistty--safe-bufstring bol cursor)))
      (let ((end (+ bol (match-end 0)))
            (content (mistty--safe-bufstring bol (+ bol (match-end 0)))))
        (mistty--with-live-buffer mistty-work-buffer
          (setq mistty--possible-prompt
                (list (mistty--from-term-pos bol)
                      (mistty--from-term-pos end)
                      content))
          (mistty-log "Possible prompt: [%s-%s] '%s'"
                      (nth 0 mistty--possible-prompt)
                      (nth 1 mistty--possible-prompt)
                      (nth 2 mistty--possible-prompt)))))))

(defun mistty--reset-markers ()
  "Reset the sync marker on both the term and work buffer.

This function should be called to fix a situation where the
markers have gone out-of-sync."
  (mistty--with-live-buffer mistty-work-buffer
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (delete-region (mistty--last-non-ws) (point-max))
      (insert "\n"))
    (move-marker mistty-sync-marker (point-max))
    (move-marker mistty--cmd-start-marker (point-max)))
  (mistty--with-live-buffer mistty-term-buffer
    (save-excursion
      (goto-char term-home-marker)
      (skip-chars-forward mistty--ws)
      (move-marker mistty-sync-marker (point)))))

(defun mistty--fs-process-filter (proc str)
  "Process filter for MisTTY in fullscreen mode.

This function detects commands to leave the fullscreen mode are
detected in STR. Failing that, it forwards PROC and STR to be
update the term buffer normally."
  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-get proc 'mistty-term-buffer)))
    (if (and (string-match "\e\\[\\(\\??47\\|\\?104[79]\\)l\\(\e8\\|\e\\[\\?1048l\\)?" str)
             (buffer-live-p work-buffer)
             (buffer-live-p term-buffer))
        (let ((after-rmcup-pos (match-beginning 0)))
          (mistty--emulate-terminal proc (substring str 0 after-rmcup-pos) work-buffer)
          (mistty--leave-fullscreen proc (substring str after-rmcup-pos)))
      ;; normal processing
      (mistty--emulate-terminal proc str work-buffer))))

(defun mistty-cursor ()
  "Return the position of the terminal cursor in the MisTTY buffer.

Note that the position might not exist in `mistty-work-buffer',
not yet, if it the work buffer is out of sync with
`mistty-term-buffer'."
  (mistty--from-pos-of (process-mark mistty-proc) mistty-term-buffer))

(defun mistty--from-pos-of (pos buffer-of-pos)
  "Return the local equivalent to POS defined in BUFFER-OF-POS."
  (+ mistty-sync-marker (with-current-buffer buffer-of-pos
                         (- pos mistty-sync-marker))))

(defun mistty--from-term-pos (pos)
  "Convert POS in the terminal to its equivalent in the work buffer.

Note that the position might not exist in `mistty-work-buffer',
not yet, if it the work buffer is out of sync with
`mistty-term-buffer'."
  (mistty--from-pos-of pos mistty-term-buffer))

(defun mistty--from-work-pos (pos)
  "Convert POS in the work buffer to its equivalent in the terminal.

Note that the position might not exist in `mistty-term-buffer',
not anymore, if it the term buffer is out of sync with
`mistty-work-buffer'."
  (mistty--from-pos-of pos mistty-work-buffer))

(defun mistty--refresh ()
  "Copy the end of the term buffer to the work buffer.

Refreshing means copying the region
[mistty-sync-marker,(point-max)] from the term buffer to the work
buffer, overwriting whatever that region of the work buffer
contains.

If refreshing is disabled, with `mistty--inhibit-refresh', this
function just sets `mistty--need-refresh' and returns.

Also updates prompt and point."
  (mistty--require-work-buffer)
  (if mistty--inhibit-refresh
      (setq mistty--need-refresh t)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (old-point (point))
          properties)
      (setq mistty--need-refresh nil)
      (when (timerp mistty--refresh-timer)
        (cancel-timer mistty--refresh-timer)
        (setq mistty--refresh-timer nil))

      (mistty-log "refresh")
      (with-current-buffer mistty-term-buffer
        (save-restriction
          (narrow-to-region mistty-sync-marker (point-max-marker))
          (setq properties (mistty--save-properties mistty-sync-marker))
          (with-current-buffer mistty-work-buffer
            (save-restriction
              (narrow-to-region mistty-sync-marker (point-max-marker))
              (replace-buffer-contents mistty-term-buffer 0.2)
              (mistty--restore-properties properties mistty-sync-marker)
              (when (> mistty--cmd-start-marker mistty-sync-marker)
                (mistty--set-prompt-properties
                 mistty-sync-marker mistty--cmd-start-marker)))
            (when (< old-point mistty-sync-marker)
              ;; restore point, possibly moved by narrow-to-region.
              (goto-char old-point)))))

      ;; Right after a mistty-send-command, we're waiting for a line
      ;; after mistty--end-prompt that's not part of the old prompt.
      (when mistty--end-prompt
        (when-let ((command-end
                    (if (get-text-property mistty-sync-marker 'mistty-prompt-id)
                        (next-single-property-change mistty-sync-marker 'mistty-prompt-id nil)
                      (mistty--bol mistty--cmd-start-marker 2))))
          (when (and (eq ?\n (char-before command-end))
                     (> (mistty--last-non-ws) command-end))
            (mistty--set-sync-mark-from-end command-end))
          (setq mistty--end-prompt nil)))

      ;; detect prompt from bracketed-past region and use that to
      ;; restrict the sync region.
      (mistty--with-live-buffer mistty-work-buffer
        (when (process-live-p mistty-proc)
          (let* ((cursor (mistty-cursor))
                 (prompt-beg
                  (let ((pos cursor))
                    (unless (and (> pos (point-min))
                                 (get-text-property (1- pos) 'mistty-prompt-id))
                      (setq pos (previous-single-property-change
                                 pos 'mistty-prompt-id nil mistty-sync-marker)))
                    (when (and (> pos (point-min))
                               (get-text-property (1- pos) 'mistty-prompt-id))
                      (setq pos (previous-single-property-change
                                 pos 'mistty-prompt-id nil mistty-sync-marker)))
                    (when (and (>= pos (point-min)) (< pos (point-max)))
                      pos))))
            (when (and prompt-beg
                       (get-text-property prompt-beg 'mistty-prompt-id)
                       (or (> prompt-beg mistty-sync-marker)
                           (and (= prompt-beg mistty-sync-marker)
                                (= mistty-sync-marker mistty--cmd-start-marker)))
                       (< prompt-beg cursor)
                       (string-match
                        mistty-prompt-re
                        (mistty--safe-bufstring
                         (mistty--bol cursor) cursor)))
              (mistty-log "Detected prompt: [%s-%s]" prompt-beg cursor)
              (mistty--set-sync-mark-from-end prompt-beg cursor)))))

      ;; Fix detected prompt.
      (mistty--with-live-buffer mistty-work-buffer
        (when (process-live-p mistty-proc)
          (let ((cursor (mistty-cursor)))
            (when (< cursor mistty--cmd-start-marker)
              ;; An overly-large prompt causes more issues than a
              ;; prompt that's just missing.
              (mistty-log "Detected prompt too large %s > %s. RESET."
                          (marker-position mistty--cmd-start-marker) cursor)
              (mistty--set-prompt mistty-sync-marker mistty-sync-marker)))))
      
      (mistty--with-live-buffer mistty-term-buffer
        ;; Next time, only sync the visible portion of the terminal.
        (when (< mistty-sync-marker term-home-marker)
          (mistty--set-sync-mark-from-end term-home-marker))

        ;; Truncate the term buffer, since scrolling back is available on
        ;; the work buffer anyways. This has to be done now, after syncing
        ;; the marker, and not in term-emulate-terminal, which is why
        ;; term-buffer-maximum-size is set to 0.
        (save-excursion
          (goto-char term-home-marker)
          (forward-line -5)
          (delete-region (point-min) (point))))

      ;; Move the point to the cursor, if necessary.
      (mistty--with-live-buffer mistty-work-buffer
        (when (process-live-p mistty-proc)
          (when (or mistty-goto-cursor-next-time
                    (null mistty--cursor-after-last-refresh)
                    (= old-point mistty--cursor-after-last-refresh))
              (mistty-goto-cursor))
          (setq mistty-goto-cursor-next-time nil)
          (setq mistty--cursor-after-last-refresh (mistty-cursor)))))))

(defun mistty--maybe-truncate-when-idle ()
  "Schedule a buffer truncation on an idle timer."
  (when (and (> mistty-buffer-maximum-size 0)
             (null mistty--truncate-timer))
    (setq mistty--truncate-timer
          (run-with-idle-timer
           1 nil #'mistty--maybe-truncate (current-buffer)))))

(defun mistty--maybe-truncate (buf)
  "Truncate BUF, if necessary and possible."
  (mistty--with-live-buffer buf
    (setq mistty--truncate-timer nil)
    (when (> mistty-buffer-maximum-size 0)
      (save-restriction
        (widen)
        (let ((cutoff (mistty--bol (point-max)
                                   (- mistty-buffer-maximum-size))))
          (when (and (> cutoff (point-min))
                     (< cutoff mistty-sync-marker))
            ;; In most cases, the solution to deal with a buffer being
            ;; truncated is to use markers, but it won't work with
            ;; possible-prompt, as it often points to locations that
            ;; don't yet exist in the work buffer.
            (when mistty--possible-prompt
              (let ((diff (- cutoff (point-min))))
                (cl-decf (nth 0 mistty--possible-prompt) diff)
                (cl-decf (nth 1 mistty--possible-prompt) diff)))
            (delete-region (point-min) cutoff)))))))

(defun mistty--save-properties (start)
  "Extracts the properties from START in the current buffer.

Returns a list of (BEG END PROPERTIES), ordered, with positions
relative to START."
  (let ((pos start) intervals)
    (while (< pos (point-max))
      (let ((props (text-properties-at pos))
            (last-pos pos))
        (setq pos (next-property-change pos nil (point-max)))
        (push `(,(- last-pos start) ,(- pos start) ,props)
              intervals)))

    intervals))

(defun mistty--restore-properties (intervals start)
  "Apply saved properties INTERVALS to the buffer at START.

This is the reverse operation of `mistty--save-properties'."
  (pcase-dolist (`(,beg ,end ,props) intervals)
    (set-text-properties (+ beg start) (+ end start) props)))

(defun mistty--set-sync-mark-from-end (sync-pos &optional cmd-pos)
  "Set the sync marker to SYNC-POS, assuming buffer ends are the same.

This function sets the `mistty-sync-marker' SYNC-POS and optionally
`mistty--cmd-start-marker' to CMD-POS.

For this to work, the term and work buffer starting with SYNC-POS
must have the same content, which is only true when SYNC-POS is
bigger than `mistty-sync-marker' and `mistty--refresh' was
called recently enough."
  (let ((chars-from-end (- (point-max) sync-pos))
        (prompt-length (if (and cmd-pos (> cmd-pos sync-pos))
                           (- cmd-pos sync-pos)
                         0)))
    (with-current-buffer mistty-term-buffer
      (move-marker mistty-sync-marker (- (point-max) chars-from-end)))
    (with-current-buffer mistty-work-buffer
      (let ((work-sync-pos (- (point-max) chars-from-end)))
        (mistty--set-prompt work-sync-pos (+ work-sync-pos prompt-length))))))

(defun mistty--move-sync-mark-with-shift (sync-pos cmd-start-pos shift)
  "Move the sync marker on the work buffer to SYNC-POS.

This function moves the sync marker to SYNC-POS on the work
buffer, the command-line start marker to CMD-START-POS. If
there's no prompt, the two positions are the same.

SHIFT specifies the current difference between the sync marker on
the work buffer and the term buffer. The shift value comes
from `mistty--modification-hook' tracking the changes."
  (let ((diff (- sync-pos mistty-sync-marker)))
    (with-current-buffer mistty-term-buffer
      (move-marker mistty-sync-marker (+ mistty-sync-marker diff shift))))
  (with-current-buffer mistty-work-buffer
    (mistty--set-prompt sync-pos cmd-start-pos)))

(defun mistty--set-prompt (sync-pos cmd-start-pos)
  "Set the sync and command start position.

SYNC-POS is used to initialize `mistty-sync-marker' and
CMD-START-POS is used to initialize `mistty--cmd-start-marker'.
They are often the same position."
  (let ((cmd-start-pos (max sync-pos cmd-start-pos))
        (inhibit-read-only t)
        (inhibit-modification-hooks t))
    (move-marker mistty-sync-marker sync-pos)
    (move-marker mistty--cmd-start-marker cmd-start-pos)
    (move-overlay mistty--sync-ov sync-pos (point-max))
    (move-overlay mistty--prompt-ov
                  (max sync-pos (mistty--bol mistty--cmd-start-marker))
                  mistty--cmd-start-marker)
    (when (> cmd-start-pos sync-pos)
      (mistty--set-prompt-properties sync-pos cmd-start-pos))))

(defun mistty--set-prompt-properties (start end)
  "Mark region from START to END as a prompt."
  (add-text-properties
   start end
   (append
    '(mistty prompt
             field mistty-prompt
             rear-nonsticky t)
    (unless (get-text-property start 'mistty-prompt-id)
      `(mistty-prompt-id ,(mistty--next-id))))))

(defun mistty-send-raw-string (str)
  "Send STR to the terminal, unprocessed.

This command is available in fullscreen mode."
  (mistty--send-string mistty-proc str))

(defun mistty--last-non-ws ()
  "Return the position of the last non-whitespace in the buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward mistty--ws)
    (point)))

(defun mistty-send-command ()
  "Send the current command to the shell."
  (interactive)
  (mistty--maybe-realize-possible-prompt)
  (setq mistty-goto-cursor-next-time t
        mistty--end-prompt t)
  (mistty-send-raw-string "\C-m"))

(defun mistty-send-last-key (&optional n)
  "Send the last key that was typed to the terminal N times.

This command extracts element of `this-command-key`, translates
it and sends it to the terminal.

This is a convenient variant to `mistty-send-key' which allows
burying key binding to send to the terminal inside of a keymap
with an arbitrary prefix.

This command is available in fullscreen mode."
  (interactive "p")
  (mistty-send-key
   (or n 1) (seq-subseq (this-command-keys-vector) -1)))

(defun mistty-positional-p (key)
  "Return non-nil if KEY is a positional key.

A key is defined as positional if it traditionally have an effect
that modifies what is displayed on the terminal in a way that
depends on where the cursor is on the terminal. See also
`mistty-positional-keys' for the set of control keys that are
defined as positional.

MisTTY will attempt to move the terminal cursor to the current
point before sending such keys to the terminal.

Non-control characters are always positional, since they're
normally just inserted.

KEY must be a string or vector such as the ones returned by `kbd'."
  (and (length= key 1)
       (characterp (aref key 0))
       (or
        (seq-contains-p mistty-positional-keys (aref key 0))
        (not (string= "Cc"
                      (get-char-code-property (aref key 0)
                                              'general-category))))))

(defun mistty-send-key (&optional n key positional)
  "Send the current key sequence to the terminal.

This command sends N times the current key sequence, or KEY if it
is specified, directly to the terminal. If the key sequence is
positional or if POSITIONAL evaluates to true, MisTTY attempts to
move the terminal's cursor to the current point.

KEY must be a string or vector as would be returned by `kbd'.

This command is available in fullscreen mode."
  (interactive "p")
  (let ((key (or key (this-command-keys-vector))))
    (mistty--with-live-buffer mistty-work-buffer
      (when (not mistty-fullscreen)
        (setq mistty-goto-cursor-next-time t)
        (when (or positional (mistty-positional-p key))
          (mistty-before-positional))))
    (mistty-send-raw-string (mistty-translate-key key n))))

(defun mistty-send-key-sequence ()
  "Send all keys to terminal until interrupted.

This function continuously read keys and sends them to the
terminal, just like `mistty-send-key', until it is interrupted
with \\[keyboard-quit] or until it is passed a key or event it
doesn't support, such as a mouse event.."
  (interactive)
  (while-let ((key
               (read-event "Sending all KEYS to terminal... Exit with C-g."
                           'inherit-input-method)))
    (mistty-send-key 1 (make-vector 1 key))))

(defun mistty-send-beginning-of-line (&optional n)
  "Send C-a (usually beginnning-of-line) to the terminal.

Possibly detect a prompt on the current line."
  (interactive "p")
  ;; While C-a is not, strictly-speaking, a positional, it's a good
  ;; sign that we're on a pointer.
  (mistty--maybe-realize-possible-prompt)
  (setq mistty-goto-cursor-next-time t)
  (mistty-send-key n "\C-a"))

(defun mistty-send-end-of-line (&optional n)
  "Send C-e (usually end-of-line) to the terminal.

Possibly detect a prompt on the current line."
  (interactive "p")
  (mistty--maybe-realize-possible-prompt)
  (mistty-goto-cursor)
  (setq mistty-goto-cursor-next-time t)
  (mistty-send-key n "\C-e"))

(defun mistty-end-of-line-or-goto-cursor (&optional n)
  "Move the point first to the end of the line, then to the cursor.

The first time this command is called, or if it is called with an
argument, it behaves like `end-of-line' and forwards N to it.

The second time it is called, it behaves like `mistty-goto-cursor'."
  (interactive "p")
  (if (and (= 1 n) (eq last-command this-command) (/= (point) (mistty-cursor)))
      (mistty-goto-cursor)
    (end-of-line n)))

(defun mistty--modification-hook (_ov is-after orig-beg orig-end &optional old-length)
  "Handler for overlay modification hooks in the synced region.

This function is called whenever the synced (purple) region of
the buffer is modified. It creates or extends a
`mistty-changeset', storing enough information about the change
to replay it afterwards."
  (when (and is-after
             mistty-sync-marker
             (>= orig-end mistty-sync-marker))
    (let ((inhibit-read-only t)
          (beg (max orig-beg mistty-sync-marker))
          (end (max orig-end mistty-sync-marker))
          (old-end (max (+ orig-beg old-length) mistty-sync-marker))
          (cs (mistty--activate-changeset)))

      ;; Temporarily stop refreshing the work buffer while collecting
      ;; modifications.
      (setq mistty--inhibit-refresh t)

      (mistty--changeset-mark-region cs beg end old-end))))

(iter-defun mistty--replay-generator (cs)
  (unwind-protect
      (let ((intervals-start (mistty--changeset-beg cs))
            (intervals-end (mistty--changeset-end cs))
            (modifications (mistty--changeset-modifications cs))
            first lower-limit upper-limit)
        (mistty-log "replay: %s" modifications)
        (setq first (car modifications))
        (while modifications
          (let* ((m (car modifications))
                 (orig-beg (nth 0 m))
                 (content (nth 1 m))
                 (old-length (nth 2 m))
                 (cursor (mistty-cursor))
                 (beg orig-beg)
                 (end (+ orig-beg (length content)))
                 (old-end (if (>= old-length 0)
                              (+ orig-beg old-length)
                            (mistty--from-pos-of (with-current-buffer mistty-term-buffer (point-max))
                                                 mistty-term-buffer)))
                 (replay-seqs nil))
            (setq modifications (cdr modifications))

            (when lower-limit
              (setq beg (max lower-limit beg)))
            (when upper-limit
              (setq end (min upper-limit end)
                    old-end (min upper-limit old-end)))

            (when (> end beg)
              (iter-yield (mistty--move-str cursor beg 'will-wait))
              (setq cursor (mistty-cursor))
              ;; cursor is as close to beg as we can make it

              ;; We couldn't move cursor as far back as beg. Presumably, the
              ;; process mark points to the leftmost modifiable position of
              ;; the command line.
              (when (and (> cursor beg)
                         (> (mistty--distance-on-term beg cursor) 0))
                (setq lower-limit cursor))
              (setq beg cursor))

            (when (> old-end beg)
              (if (eq m first)
                  (progn
                    (iter-yield
                     (mistty--move-str cursor old-end 'will-wait))
                    (setq cursor (mistty-cursor))
                    (when (and (> beg cursor)
                               (> (mistty--distance-on-term beg cursor) 0))
                      ;; If we couldn't even get to beg we'll have trouble with
                      ;; the next modifications, too, as they start left of this
                      ;; one. Remember that.
                      (setq upper-limit cursor))
                    (setq old-end (max beg (min old-end cursor))))

                ;; after the first modification, just optimistically go to
                ;; old-end if upper-limit allows it.
                (push (mistty--move-str cursor old-end) replay-seqs)))

            ;; delete
            (when (> old-end beg)
              (push (mistty--repeat-string
                     (mistty--distance-on-term beg old-end) "\b")
                    replay-seqs))

            ;; insert
            (when (and (> end beg) (>= beg orig-beg))
              (push (mistty--maybe-bracketed-str
                     (substring content
                                (max 0 (- beg orig-beg))
                                (min (length content) (max 0 (- end orig-beg)))))
                    replay-seqs))

            ;; for the last modification, move cursor back to point
            (when (and (null modifications)
                       (>= (point) intervals-start)
                       (<= (point) intervals-end))
              (setq mistty-goto-cursor-next-time t)
              (push (mistty--move-str
                     end
                     (if lower-limit (max lower-limit (point))
                       (point)))
                    replay-seqs))

            ;; send the content of replay-seqs
            (let ((replay-str (mapconcat #'identity (nreverse replay-seqs) "")))
              (if (length> replay-str 0)
                  (iter-yield replay-str)))))

        ;; force refresh, even if nothing was sent, if only to revert what
        ;; couldn't be replayed.
        (setq mistty--need-refresh t)

        ;; Re-enable refresh.
        (setf (mistty--changeset-applied cs) t)
        )

    ;; unwind; always release the changeset at the end.
    (mistty--release-changeset cs)
    (mistty--refresh-after-changeset)))

(defun mistty--refresh-after-changeset ()
  "Refresh the work buffer again if there are not more changesets."
  (unless mistty--changesets
    (setq mistty--inhibit-refresh nil)
    (when mistty--need-refresh
      (mistty--refresh))))

(defun mistty--move-str (from to &optional will-wait)
  "Builds a terminal sequence to move from FROM to TO.

FROM and TO are positions in the work buffer.

If WILL-WAIT is non-nil, add a little flourish at the end to
avoid situation where movement not being possible results in the
terminal not answering anything, which forces MisTTY to rely on a
timeout to continue its operations."
  (or
   (when (= from to) "")
   (mistty--fish-multiline-move-str from to)
   (mistty--move-horizontally-str (mistty--distance-on-term from to) will-wait)))

(defun mistty--move-horizontally-str (direction &optional will-wait)
  "Return a key sequence to move horizontally.

The absolute value of DIRECTION specifies the number of character
 to move and the sign specifies whether to go right (positive) or
 left (negative).

If WILL-WAIT, add a useless right-left or left-right move at the
end, which forces the terminal to send something back in the case
where we jest can't move left/right anymore, thus avoiding a
timeout."
  (if (zerop direction)
      ""
    (let ((distance (abs direction))
          (towards-str
           (if (< direction 0) mistty-left-str mistty-right-str))
          (away-from-str
           (if (< direction 0) mistty-right-str mistty-left-str)))
      (concat
       (mistty--repeat-string distance towards-str)
       (if will-wait
           ;; Send a no-op right/left pair so that if, for example, it's
           ;; just not possible to go left anymore, the connected process
           ;; might still send *something* back and mistty--send-and-wait
           ;; won't have to time out.
           (concat away-from-str towards-str)
         "")))))

(defun mistty--move-vertically-str (direction)
  "Return a key sequence to move vertically.

The absolute value of DIRECTION specifies the number of character
 to move and the sign specifies whether to go down (positive) or
 up (negative)."
  (mistty--repeat-string
   (abs direction)
   (if (< direction 0) mistty-up-str mistty-down-str)))

(defun mistty--fish-multiline-move-str (from to)
  "Return a move sequence if currently in a fish multiline prompt.

Normally MisTTY avoids using up and down arrows, as these
typically move up and down history in a shell. However, in a fish
multiline prompt, the up and down arrows work normally and help
work around the problem of moving from line to line without
knowing which space on the screen are reachable with the cursor
and which spaces aren't.

If in a multiline fish prompt, return a string moving the cursor
from FROM to TO, otherwise return nil.

FROM and TO are positions in the work buffer."
  (let* ((prompt-end (marker-position mistty--cmd-start-marker))
         (prompt-length (if (> prompt-end mistty-sync-marker)
                            (- prompt-end
                               (mistty--bol prompt-end))
                          0)))
    (when (and (> prompt-length 0)
               (> from prompt-end)
               (> to prompt-end))
      (with-current-buffer mistty-term-buffer
        (mistty--fish-multiline-move-str-on-term
         (mistty--safe-pos (mistty--from-work-pos from))
         (mistty--safe-pos (mistty--from-work-pos to))
         (mistty--safe-pos (mistty--from-work-pos prompt-end))
         prompt-length)))))

(defun mistty--fish-multiline-move-str-on-term
    (from to prompt-start prompt-length)
  "Internal helper for `mistty--fish-multiline'."
  (let ((beg (mistty--bol-skipping-fakes (min from to)))
        (end (max from to))
        (calling-buffer (current-buffer)))
    (when (> (count-lines beg end) 1)
      (with-temp-buffer
        (insert (with-current-buffer calling-buffer
                  (buffer-substring beg end)))
        (let ((from (copy-marker (- from beg)))
              (to (copy-marker (- to beg)))
              lines min-col from-col to-col move-col)

          ;; delete fake newlines
          (goto-char (point-min))
          (while-let ((fake-nl (text-property-any (point) (point-max) 'term-line-wrap t)))
            (goto-char fake-nl)
            (delete-char 1))

          ;; all lines must start with spaces except the one with the
          ;; prompt.
          (when (and
                 (>= (mistty--col from) prompt-length)
                 (>= (mistty--col to) prompt-length)
                 (length> (setq lines (mistty--lines)) 1)
                 (seq-every-p
                  (lambda (line-start)
                    (progn
                      (goto-char line-start)
                      (looking-at-p (make-string prompt-length ?\ ))))
                  (if (> prompt-start beg)
                      (cdr lines)
                    lines)))
            (dolist (line-start lines)
              (delete-region line-start (+ prompt-length line-start)))

            ;; Get from and to column before deleting trailing ws, in
            ;; case from is on a trailing ws, we know it exits and in
            ;; case to is on a trailing ws, we optimistically assume
            ;; it exists.
            (setq from-col (mistty--col from))
            (setq to-col (mistty--col to))

            ;; Find a column that's not on a trailing ws to move down.
            (delete-trailing-whitespace (point-min) (point-max))
            (setq min-col (max 0 (1- (seq-min (mapcar #'mistty--line-length lines)))))
            (setq move-col (min min-col to-col from-col))

            (concat
             ;; go left-right from from-col to move-col
             (mistty--move-horizontally-str (- move-col from-col))
             ;; go up/down from from-line to to-line
             (mistty--move-vertically-str (- (mistty--line to) (mistty--line from)))
             ;; go left-right from move-col to to-col
             (mistty--move-horizontally-str (- to-col move-col)))))))))

(defun mistty--distance-on-term (beg end)
  "Compute the number of cursor moves necessary to get from BEG to END.

This function skips over the `term-line-wrap' newlines introduced
by term as if they were not here.

While it takes BEG and END as work buffer positions, it looks in
the term buffer to figure out, so it's important for the BEG and
END section to be valid in the term buffer."
  (with-current-buffer mistty-term-buffer
    (let ((beg (mistty--safe-pos (mistty--from-work-pos (min beg end))))
          (end (mistty--safe-pos (mistty--from-work-pos (max beg end))))
          (sign (if (< end beg) -1 1)))
      (let ((pos beg) (nlcount 0))
        (while (and (< pos end) (setq pos (text-property-any pos end 'term-line-wrap t)))
          (setq pos (1+ pos))
          (setq nlcount (1+ nlcount)))
        (* sign (- (- end beg) nlcount))))))

(defun mistty-next-prompt (n)
  "Move the point to the Nth next prompt in the buffer."
  (interactive "p")
  (let ((pos (point))
        found)
    ;; skip current prompt
    (when (and (eq 'prompt (get-text-property pos 'mistty))
               (not (get-text-property pos 'field)))
      (setq pos (next-single-property-change pos 'mistty-prompt-id nil (point-max))))
    ;; go to next prompt(s)
    (dotimes (_ n)
      (if (and (setq found (text-property-any pos (point-max) 'mistty 'prompt))
               (setq pos (next-single-property-change found 'mistty-prompt-id nil (point-max))))
          (if (get-text-property found 'field)
              (goto-char (next-single-property-change found 'field nil pos))
            (goto-char found))

        (if (mistty--maybe-realize-possible-prompt (mistty-cursor))
            (mistty-next-prompt 1)
        (error "No next prompt"))))))

(defun mistty-previous-prompt (n)
  "Move the point to the Nth previous prompt in the buffer."
  (interactive "p")
  (let ((not-current nil)
        (pos (point)))
    (when (eq 'mistty-prompt
              (or (get-text-property (point) 'field)
                  (get-text-property (1- (point)) 'field)))
      (setq not-current t)
      (setq pos (1- (point))))
    (dotimes (_ n)
      (let ((match (save-excursion
                     (goto-char pos)
                     (text-property-search-backward 'mistty-prompt-id nil nil not-current))))
        (unless match
          (error "No previous prompt"))
        (goto-char (prop-match-beginning match)))
      (when (get-text-property (point) 'field)
        (goto-char (next-single-property-change (point) 'field nil (point-max))))
      (setq not-current t)
      (setq pos (point)))))

(defun mistty--pre-command ()
  "Function called from the `pre-command-hook' in `mistty-mode' buffers."
  (setq mistty--old-point (point)))

(defun mistty--post-command ()
  "Function called from the `post-command-hook' in `mistty-mode' buffers."
  ;; Show cursor again if the command moved the point.
  (let ((point-moved (and mistty--old-point (/= (point) mistty--old-point))))
    (when point-moved
      (setq cursor-type t))

    (run-with-idle-timer 0 nil #'mistty--post-command-1
                         mistty-work-buffer point-moved)))

(defun mistty--post-command-1 (buf point-moved)
  "Function called from `mistty--post-command'.

This is the body of `mistty--post-command', which replays any
modifications or cursor movement executed during the command. It
is run in an idle timer to avoid failures inside of the
post-command hook."
  (mistty--with-live-buffer buf
    (save-restriction
      (widen)
    (when (and (process-live-p mistty-proc)
               (buffer-live-p mistty-term-buffer))
      (let* ((cs (mistty--active-changeset))
             shift replay)
        (cond
         ;; nothing to do
         ((not (mistty--changeset-p cs)))

         ;; modifications are part of the current prompt; replay them
         ((mistty-on-prompt-p (mistty-cursor))
          (setq replay t))

         ;; modifications are part of a possible prompt; realize it, keep the modifications before the
         ;; new prompt and replay the modifications after the new prompt.
         ((and (mistty--possible-prompt-p)
               (setq shift (mistty--changeset-restrict
                            cs (nth 0 mistty--possible-prompt))))
          (mistty--realize-possible-prompt shift)
          (setq replay t))

         ;; leave all modifications if there's enough of an unmodified
         ;; section at the end. moving the sync mark is only possible
         ;; as long as the term and work buffers haven't diverged.
         ((and (< (mistty--changeset-end cs)
                  ;; modifiable limit
                  (mistty--bol (point-max) -5))
               (not mistty--need-refresh))
          (mistty--set-sync-mark-from-end
           (mistty--bol (mistty--changeset-end cs) 3)))

         (t ;; revert everything

          ;; The following forces a call to refresh, in time, even if
          ;; the process sent nothing new.
          (setq mistty--need-refresh t)))

        (when replay
          (mistty--enqueue mistty--queue (mistty--replay-generator cs)))

        ;; Abandon changesets that haven't been picked up for replay.
        (when (and (not replay) (mistty--changeset-p cs))
          (mistty--release-changeset cs)
          (mistty--refresh-after-changeset))

        (when (and (not replay) point-moved)
          (mistty--enqueue mistty--queue (mistty--cursor-to-point-generator))))))))

(iter-defun mistty--cursor-to-point-generator ()
  "A generator that tries to move the terminal cursor to the point."
  (when (mistty-on-prompt-p (point))
    (iter-yield (mistty--move-str (mistty-cursor) (point)))))

(defun mistty--window-size-change (_win)
  "Update the process terminal size, reacting to _WIN changing size."
  (when (process-live-p mistty-proc)
    (let* ((adjust-func (or (process-get mistty-proc 'adjust-window-size-function)
                            window-adjust-process-window-size-function))
           (size (funcall adjust-func mistty-proc
                          (get-buffer-window-list mistty-work-buffer nil t))))
      (when size
        (mistty--set-process-window-size (car size) (- (cdr size) left-margin-width)))))
  (dolist (win (get-buffer-window-list mistty-work-buffer nil t))
    (mistty--recenter win)))

(defun mistty--set-process-window-size (width height)
  "Set the process terminal size to WIDTH x HEIGHT."
  (mistty--with-live-buffer mistty-term-buffer
    (set-process-window-size mistty-proc height width)
    (term-reset-size height width)))

(defun mistty--enter-fullscreen (proc terminal-sequence)
  "Enter fullscreen mode for PROC.

TERMINAL-SEQUENCE is processed in fullscreen mode."
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (mistty--detach 'keep-sync-markers)
    (setq mistty-fullscreen t)
    (mistty--with-live-buffer mistty-term-buffer
      (setq mistty-fullscreen t))

    (let ((msg
           "Fullscreen mode ON. C-c C-j switches between the tty and scrollback buffer."))
      (save-excursion
        (goto-char (point-max))
        (insert msg)
      (message msg)))

    (let ((bufname (buffer-name)))
      (rename-buffer (generate-new-buffer-name (concat bufname " scrollback")))
      (with-current-buffer mistty-term-buffer
        (rename-buffer bufname)
        (turn-on-font-lock)))
    (mistty--swap-buffer-in-windows mistty-work-buffer mistty-term-buffer)


    (set-process-filter proc #'mistty--fs-process-filter)
    (set-process-sentinel proc #'mistty--fs-process-sentinel)

    (mistty--update-mode-lines proc)
    (when (length> terminal-sequence 0)
      (funcall (process-filter proc) proc terminal-sequence))))

(defun mistty--leave-fullscreen (proc terminal-sequence)
  "Leave fullscreen mode for PROC.

TERMINAL-SEQUENCE is processed in fullscreen mode."
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (save-restriction
      (widen)
    (setq mistty-fullscreen nil)
    (mistty--with-live-buffer mistty-term-buffer
      (setq mistty-fullscreen nil))

    (mistty--attach (process-buffer proc))

    (let ((bufname (buffer-name mistty-term-buffer)))
      (with-current-buffer mistty-term-buffer
        (rename-buffer (generate-new-buffer-name (concat " mistty tty " bufname))))
      (rename-buffer bufname))

    (mistty--swap-buffer-in-windows mistty-term-buffer mistty-work-buffer)
    (with-current-buffer mistty-term-buffer
      (font-lock-mode -1))

    (mistty--update-mode-lines proc)
    (when (length> terminal-sequence 0)
      (funcall (process-filter proc) proc terminal-sequence)))))

(defun mistty--update-mode-lines (&optional proc)
  "Update the mode lines of the work and term buffers of PROC.

If PROC is not specified, use the value of `mistty-work-buffer'
and `mistty-term-buffer' to find the buffers.

Ignores buffers that don't exist."
  (mistty--with-live-buffer
      (or mistty-work-buffer (and proc (process-get proc 'mistty-work-buffer)))
    (cond
     (mistty-fullscreen
      (setq mode-line-process
            (propertize "scrollback"
                        'help-echo "mouse-1: Go to Term buffer"
                        'mouse-face 'mode-line-highlight
                        'local-map '(keymap
                                     (mode-line
                                      keymap
                                      (down-mouse-1 . mistty-switch-to-fullscreen-buffer))))))
     (mistty-proc
      (setq mode-line-process (format ":%s" (process-status mistty-proc))))
     (t
      (setq mode-line-process ":no process"))))
  (mistty--with-live-buffer
      (or mistty-term-buffer (and proc (process-buffer proc)))
    (cond
     (mistty-fullscreen
      (setq mode-line-process
            (concat
             (propertize "misTTY"
                         'help-echo "mouse-1: Go to scrollback buffer"
                         'mouse-face 'mode-line-highlight
                         'local-map '(keymap
                                      (mode-line
                                       keymap
                                       (down-mouse-1 . mistty-switch-to-scrollback-buffer)))) ":%s")))
     (t
      (setq mode-line-process "misTTY:%s"))))
  (force-mode-line-update))

(defun mistty--swap-buffer-in-windows (a b)
  "Swap buffers A and B in windows and window previous buffers."
  (walk-windows
   (lambda (win)
     (cond
      ((eq (window-buffer win) a)
       (set-window-buffer win b))
      ((eq (window-buffer win) b)
       (set-window-buffer win a)))
     (let ((prev-buffers (window-prev-buffers win))
           (modified nil))
       (dolist (entry prev-buffers)
         (cond
          ((eq (car entry) a)
           (setcar entry b)
           (setq modified t))
          ((eq (car entry) b)
           (setcar entry a)
           (setq modified t))))
       (when modified
         (set-window-prev-buffers win prev-buffers))))))

(defun mistty-switch-to-fullscreen-buffer ()
  "Switch to the fullscreen buffer, from the scrollback buffer."
  (interactive)
  (if (and mistty-fullscreen (buffer-live-p mistty-term-buffer))
      (switch-to-buffer mistty-term-buffer)
    (error "No fullscreen buffer available")))

(defun mistty-switch-to-scrollback-buffer ()
  "Switch to the scrollback buffer, from the fullscreen buffer."
  (interactive)
  (if (and (buffer-live-p mistty-work-buffer)
           (buffer-local-value 'mistty-fullscreen mistty-work-buffer))
      (switch-to-buffer mistty-work-buffer)
    (error "No scrollback buffer available")))

(defun mistty-on-prompt-p (pos)
  "Return non-nil if POS is on a prompt."
  (and (>= pos mistty-sync-marker)
       (or mistty-bracketed-paste
           (and
            (>= pos mistty--cmd-start-marker)
            (> mistty--cmd-start-marker mistty-sync-marker)
            (>= pos mistty--cmd-start-marker)
            (<= pos (mistty--eol mistty--cmd-start-marker))))))

(defun mistty-before-positional ()
  "Prepare the state for executing a positional command.

This:
 - creates any possible prompts that were detected
 - move the cursor to the point, so the command executes
   at the right position in the buffer."
  (let ((cursor (mistty-cursor)))
    (when (and (not (= cursor (point)))
               (mistty--maybe-realize-possible-prompt))
      (mistty-send-raw-string (mistty--move-str cursor (point))))))

(defun mistty--maybe-realize-possible-prompt (&optional pos)
  "If a possible prompt was detected at POS, create it now."
  (let ((pos (or pos (point))))
    (when (and (not (mistty-on-prompt-p pos))
               (mistty--possible-prompt-p)
               (mistty--possible-prompt-contains pos))
      (mistty--realize-possible-prompt)
      t)))

(defun mistty--realize-possible-prompt (&optional shift)
  "Create the prompt in `mistty--possible-prompt'.

If SHIFT is non-nil, it specifies a position difference between
the sync markers in the work and term buffer at the beginning of
the prompt."
  (pcase-let ((`(,start ,end ,_ ) mistty--possible-prompt))
    (if shift
        (mistty--move-sync-mark-with-shift start end shift)
      (mistty--set-sync-mark-from-end start end)))
  (setq mistty--possible-prompt nil))

(defun mistty--possible-prompt-p ()
  "Return non-nil if `mistty--possible-prompt' is usable."
  (when mistty--possible-prompt
    (pcase-let ((`(,start ,end ,content) mistty--possible-prompt))
      (let ((cursor (mistty-cursor)))
        (and (>= end mistty--cmd-start-marker)
             (>= cursor end)
             (or (> cursor (point-max))
                 (<= cursor (mistty--bol start 2)))
             (string= content (mistty--safe-bufstring start end)))))))

(defun mistty--possible-prompt-contains (pos)
  "Return non-nil if POS is on `mistty--possible-prompt'."
  (when mistty--possible-prompt
    (pcase-let ((`(,start ,line-start ,_) mistty--possible-prompt))
      (and (>= pos line-start) (<= pos (mistty--eol start))))))

(provide 'mistty)

;;; mistty.el ends here
