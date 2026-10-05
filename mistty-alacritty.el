;;; mistty-alacritty.el --- Raw alacritty-based terminal -*- lexical-binding: t -*-

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
;; This file defines a mode that provides direct access to an
;; alacritty-based terminal, with none of the extra features and
;; overhead of MisTTY. The result is similar to term.el in raw mode.

(require 'mistty-util)
(require 'mistty-kbd)
(require 'mistty-log)
(require 'mistty-scrolline)
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'ansi-osc) ; links use ansi-osc-hyperlink
(defvar explicit-shell-file-name) ;; defined in shell

;; Loading the module is optional here, so this file can be safely
;; loaded and compiled. Availability check should be done dynamically
;; dynamically using (mistty-alacritty-available-p)

(defvar mistty-alacritty-version "2.1.0"
  "Mistty version name or \"dev\" for local development version.

This is used to download and load the correct version of the module.")

(defvar mistty-alacritty-release nil
  "Mistty release name.

Defaults to `mistty-alacritty-version'. It is available in case the
release and version differ.")

(defvar mistty-alacritty-arch
  (car (string-split system-configuration "-"))
  "Processor architecture.

This is used to download and load the correct version of the module.")

(defun mistty-alacritty-modulename ()
  "Name of the module that should be loaded.

The module version should match the version of the elisp code and should
provide the feature `mistty-alacritty-vt'."
  (format "mistty-alacritty-vt-%s-%s%s"
          mistty-alacritty-version
          mistty-alacritty-arch
          module-file-suffix))

(defun mistty-alacritty-load ()
  "Attempt to load the module.

Return non-nil if the module is found and could be loaded, this function
returns. Return nil if the module is not found.

This function might fail if the module is found, but cannot be loaded
for some reason."
  (cond
   ((featurep 'mistty-alacritty-vt) t)
   ((load (mistty-alacritty-modulename) 'noerror 'nomessage)
    (unless (featurep 'mistty-alacritty-vt)
      (error "Module doesn't define feature mistty-alacritty-vt"))

    t)
   ;; not found
   (t nil)))

(mistty-alacritty-load)

;; These declarations allow compiling without loading the module.
(eval-when-compile
  (declare-function mistty-alacritty-vt-alt-screen-p nil (term))
  (declare-function mistty-alacritty-vt-cleanup-prompt-sp nil (term pos))
  (declare-function mistty-alacritty-vt-clear-to-eol nil (term line col))
  (declare-function mistty-alacritty-vt-clear-scrollback nil (term))
  (declare-function mistty-alacritty-vt-cursor nil (term))
  (declare-function mistty-alacritty-vt-enable-scrollback nil (term))
  (declare-function mistty-alacritty-vt-make-vterm nil (w h))
  (declare-function mistty-alacritty-vt-process-bytes nil (term bytes))
  (declare-function mistty-alacritty-vt-render nil (term cursor))
  (declare-function mistty-alacritty-vt-render-screen nil (term cursor))
  (declare-function mistty-alacritty-vt-resize nil (term w h))
  (declare-function mistty-alacritty-vt-scrollback-line-count nil (term))
  (declare-function mistty-alacritty-vt-write-scrollback nil (term)))

(defcustom mistty-alacritty-osc52 'only-copy
  "Allow sharing data through clipboard.

This option controls the ability of applications running on the terminal
to clipboard to share text (copy) or to request text (paste).

This is mapped to the kill ring. Text sent by terminals using
OSC52 (copy) is added to the kill ring and made available to `yank' and
`yank-pop'. Text requested by terminal using OSC52 (paste) is the same
as what a `yank' operation would return, that is, usually, text form
either the Emacs kill ring or the system clipboard.

By default the terminal can share text (copy) to be added to the Emacs
kill ring, but requesting text (paste) is disabled.

Valid values for this option are:
- \\='only-copy only allows copying text (the default)
- \\='only-paste allows pasting text, but not copying
- \\='copy-paste allows both copying and pasting text
- nil or anything else turns off osc52 support entirely

Note that only OSC 52 clipboard (c) sharing is supported; requests for
the any other target (p, q, s, 0-7) are ignored.

Setting this option doesn't affect running terminals. It only changes
terminals created after the option was changed.

This option only works on alacritty terminals. It has no effect on eterm
terminals."
  :group 'mistty
  :type '(choice (const :tag "Only Copy" only-copy)
                 (const :tag "Only Paste" only-paste)
                 (const :tag "Copy and Paste" copy-paste)
                 (const :tag "Disabled" nil)))

(defcustom mistty-alacritty-term-name nil
  "Value for the TERM env variable for alacritty virtual terminals.

If the alacritty terminal definition is installed, set it to alacritty
or alacritty-direct to get full 24bit color support.

See https://github.com/alacritty/alacritty/blob/master/INSTALL.md#terminfo

For backward compatibility or if you often log into other hosts that
don't have alacritty installed, you may want set it to xterm-256color or
even xterm.

If this is nil, MisTTY checks whether the alacritty terminfo is present
on the system and automatically falls back to xterm-256color.

Setting this option doesn't affect running terminals. It only changes
terminals created after the option was changed.

This option only works on alacritty terminals. It has no effect on eterm
terminals."
  :group 'mistty
  :type '(choice (const :tag "(auto)" nil)
                 (const "alacritty")
                 (const "xterm-256color")
                 (const "xterm")
                 string))

(defvar mistty-alacritty--inhibit-render nil
  "Tell the process filter not to update buffer content.

This is meant to be set temporarily, within a let form that calls the
filter.

The virtual terminal state is still updated when this variable is set.")

(defvar-local mistty-alacritty--vterm nil
  "Virtual terminal tied to the buffer, from mistty-alacritty-vt.")

(defvar-local mistty-alacritty--cursor nil
  "Marker that tracks the cursor position.

This marker is set by the last rendering operation.")

(defvar-local mistty-alacritty--home nil
  "Marker that tracks the position of the top of the screen.

This immediately follows the scrollback lines.")

(defvar-local mistty-alacritty-columns nil
  "Width of the terminal, in columns. Set by `mistty-alacritty-resize'.")

(defvar-local mistty-alacritty-lines nil
  "Height of the terminal, in lines. Set by `mistty-alacritty-resize'.")

(defvar mistty-alacritty-mode-map
  (let ((map (make-sparse-keymap))
        (esc-map (make-sparse-keymap)))
    ;; This builds a map very similar to term raw-keymap.
    (dotimes (c 128)
      (unless (memq c '(?\C-c ?\C-x))
        (define-key map (make-string 1 c) 'mistty-send-key)))
    (define-key map "\e" esc-map)
    (dotimes (c 128)
      (unless (memq c '(?O ?\[))
        (define-key esc-map (make-string 1 c) 'mistty-send-key)))

    ;; C-q <any key> sends that key to the terminal unmodified
    (define-key map "\C-q" '(keymap (t . mistty-send-last-key)))

    (dolist (key '([mouse-2] [up] [down] [right] [left] [C-up] [C-down]
                   [C-right] [C-left] [delete] [deletechar] [backspace]
                   [home] [end] [insert] [S-prior] [S-next] [S-insert]
                   [prior] [next] [?\C-/] [?\C- ] [?\C-\M-/] [?\C-\M- ]))
      (define-key map key 'mistty-send-key))

    ;; Mirror keybindings from mistty-mode-map, for consistency.
    (keymap-set map "C-c C-c" #'mistty-send-last-key)
    (keymap-set map "C-c C-z" #'mistty-send-last-key)
    (keymap-set map "C-c C-\\" #'mistty-send-last-key)
    (keymap-set map "C-c C-g" #'mistty-send-last-key)
    (keymap-set map "C-c C-q" #'mistty-capture-keyboard)
    ;; TODO: support xterm-paste?

  map)
  "Keymap of major mode MisTTY Alacritty.

This intercepts all major key bindings and sends them to the terminal.
Overwrite to recover key bindings.")

(defvar mistty-alacritty--key-map
  (let ((map (make-sparse-keymap)))

    (define-key map (kbd "<return>") "\r")
    (define-key map (kbd "C-<return>") "\r")
    (define-key map (kbd "M-<return>") "\e\r")
    (define-key map (kbd "S-<return>") "\r")
    (define-key map (kbd "C-S-<return>") "\r")
    (define-key map (kbd "M-S-<return>") "\e\r")
    (define-key map (kbd "C-M-<return>") "\e\r")

    (define-key map (kbd "<escape>") "\e")
    (define-key map (kbd "C-<escape>") "\e")
    (define-key map (kbd "M-<escape>") "\e\e")
    (define-key map (kbd "S-<escape>") "\e")
    (define-key map (kbd "C-S-<escape>") "\e")
    (define-key map (kbd "M-S-<escape>") "\e\e")
    (define-key map (kbd "C-M-<escape>") "\e\e")

    (define-key map (kbd "<backspace>") "\C-?") ; kbs
    (define-key map (kbd "C-<backspace>") "\x08")
    (define-key map (kbd "M-<backspace>") "\e\C-?")
    (define-key map (kbd "S-<backspace>") "\C-?")
    (define-key map (kbd "C-S-<backspace>") "\x08")
    (define-key map (kbd "M-S-<backspace>") "\e\C-?")
    (define-key map (kbd "C-M-<backspace>") "\e\x08")

    (define-key map (kbd "<tab>") "\t")
    (define-key map (kbd "C-<tab>") "\t")
    (define-key map (kbd "M-<tab>") "\e\t")
    (define-key map (kbd "S-<tab>") "\e[Z") ; kcbt
    (define-key map (kbd "C-S-<tab>") "\e[Z")
    (define-key map (kbd "M-S-<tab>") "\e\e[Z")
    (define-key map (kbd "C-M-<tab>") "\e\t")

    (define-key map (kbd "SPC") " ")
    (define-key map (kbd "C-SPC") "\x00")
    (define-key map (kbd "M-SPC") "\e ")
    (define-key map (kbd "S-SPC") " ")
    (define-key map (kbd "C-S-SPC") "\x00")
    (define-key map (kbd "M-S-SPC") "\e ")
    (define-key map (kbd "C-M-SPC") "\e\x00")

    (define-key map (kbd "<clear>") "\eOE") ; kb2
    (define-key map (kbd "<delete>") "\e[3~") ; kdch1
    (define-key map (kbd "<down>") "\eOB") ; kcud1
    (define-key map (kbd "<end>") "\eOF") ; kend
    (define-key map (kbd "<f10>") "\e[21~") ; kf10
    (define-key map (kbd "<f11>") "\e[23~") ; kf11
    (define-key map (kbd "<f12>") "\e[24~") ; kf12
    (define-key map (kbd "<f13>") "\e[1;2P") ; kf13
    (define-key map (kbd "<f14>") "\e[1;2Q") ; kf14
    (define-key map (kbd "<f15>") "\e[1;2R") ; kf15
    (define-key map (kbd "<f16>") "\e[1;2S") ; kf16
    (define-key map (kbd "<f17>") "\e[15;2~") ; kf17
    (define-key map (kbd "<f18>") "\e[17;2~") ; kf18
    (define-key map (kbd "<f19>") "\e[18;2~") ; kf19
    (define-key map (kbd "<f1>") "\eOP") ; kf1
    (define-key map (kbd "<f20>") "\e[19;2~") ; kf20
    (define-key map (kbd "<f21>") "\e[20;2~") ; kf21
    (define-key map (kbd "<f22>") "\e[21;2~") ; kf22
    (define-key map (kbd "<f23>") "\e[23;2~") ; kf23
    (define-key map (kbd "<f24>") "\e[24;2~") ; kf24
    (define-key map (kbd "<f25>") "\e[1;5P") ; kf25
    (define-key map (kbd "<f26>") "\e[1;5Q") ; kf26
    (define-key map (kbd "<f27>") "\e[1;5R") ; kf27
    (define-key map (kbd "<f28>") "\e[1;5S") ; kf28
    (define-key map (kbd "<f29>") "\e[15;5~") ; kf29
    (define-key map (kbd "<f2>") "\eOQ") ; kf2
    (define-key map (kbd "<f30>") "\e[17;5~") ; kf30
    (define-key map (kbd "<f31>") "\e[18;5~") ; kf31
    (define-key map (kbd "<f32>") "\e[19;5~") ; kf32
    (define-key map (kbd "<f33>") "\e[20;5~") ; kf33
    (define-key map (kbd "<f34>") "\e[21;5~") ; kf34
    (define-key map (kbd "<f35>") "\e[23;5~") ; kf35
    (define-key map (kbd "<f36>") "\e[24;5~") ; kf36
    (define-key map (kbd "<f37>") "\e[1;6P") ; kf37
    (define-key map (kbd "<f38>") "\e[1;6Q") ; kf38
    (define-key map (kbd "<f39>") "\e[1;6R") ; kf39
    (define-key map (kbd "<f3>") "\eOR") ; kf3
    (define-key map (kbd "<f40>") "\e[1;6S") ; kf40
    (define-key map (kbd "<f41>") "\e[15;6~") ; kf41
    (define-key map (kbd "<f42>") "\e[17;6~") ; kf42
    (define-key map (kbd "<f43>") "\e[18;6~") ; kf43
    (define-key map (kbd "<f44>") "\e[19;6~") ; kf44
    (define-key map (kbd "<f45>") "\e[20;6~") ; kf45
    (define-key map (kbd "<f46>") "\e[21;6~") ; kf46
    (define-key map (kbd "<f47>") "\e[23;6~") ; kf47
    (define-key map (kbd "<f48>") "\e[24;6~") ; kf48
    (define-key map (kbd "<f49>") "\e[1;3P") ; kf49
    (define-key map (kbd "<f4>") "\eOS") ; kf4
    (define-key map (kbd "<f50>") "\e[1;3Q") ; kf50
    (define-key map (kbd "<f51>") "\e[1;3R") ; kf51
    (define-key map (kbd "<f52>") "\e[1;3S") ; kf52
    (define-key map (kbd "<f53>") "\e[15;3~") ; kf53
    (define-key map (kbd "<f54>") "\e[17;3~") ; kf54
    (define-key map (kbd "<f55>") "\e[18;3~") ; kf55
    (define-key map (kbd "<f56>") "\e[19;3~") ; kf56
    (define-key map (kbd "<f57>") "\e[20;3~") ; kf57
    (define-key map (kbd "<f58>") "\e[21;3~") ; kf58
    (define-key map (kbd "<f59>") "\e[23;3~") ; kf59
    (define-key map (kbd "<f5>") "\e[15~") ; kf5
    (define-key map (kbd "<f60>") "\e[24;3~") ; kf60
    (define-key map (kbd "<f61>") "\e[1;4P") ; kf61
    (define-key map (kbd "<f62>") "\e[1;4Q") ; kf62
    (define-key map (kbd "<f63>") "\e[1;4R") ; kf63
    (define-key map (kbd "<f6>") "\e[17~") ; kf6
    (define-key map (kbd "<f7>") "\e[18~") ; kf7
    (define-key map (kbd "<f8>") "\e[19~") ; kf8
    (define-key map (kbd "<f9>") "\e[20~") ; kf9
    (define-key map (kbd "<home>") "\eOH") ; khome
    (define-key map (kbd "<insert>") "\e[2~") ; kich1
    (define-key map (kbd "<kp-enter>") "\eOM") ; kent
    (define-key map (kbd "<left>") "\eOD") ; kcub1
    (define-key map (kbd "<menu>") "\e[1;2S") ; kf16
    (define-key map (kbd "<next>") "\e[6~") ; knp
    (define-key map (kbd "<prior>") "\e[5~") ; kpp
    (define-key map (kbd "<right>") "\eOC") ; kcuf1
    (define-key map (kbd "<up>") "\eOA") ; kcuu1
    (define-key map (kbd "S-<delete>") "\e[3;2~") ; kDC
    (define-key map (kbd "S-<down>") "\e[1;2B") ; kind
    (define-key map (kbd "S-<end>") "\e[1;2F") ; kEND
    (define-key map (kbd "S-<home>") "\e[1;2H") ; kHOM
    (define-key map (kbd "S-<insert>") "\e[2;2~") ; kIC
    (define-key map (kbd "S-<left>") "\e[1;2D") ; kLFT
    (define-key map (kbd "S-<next>") "\e[6;2~") ; kNXT
    (define-key map (kbd "S-<prior>") "\e[5;2~") ; kPRV
    (define-key map (kbd "S-<right>") "\e[1;2C") ; kRIT
    (define-key map (kbd "S-<up>") "\e[1;2A") ; kri

    map)
  "Translate Emacs keys into byte sequences for the terminal.

The key passed to this map are pre-translated using
`mistty-translation-keymap', mapping ESC, RET TAB and DEL to their
symbol equivalent, and M-<char> is always used in preference to ESC
<char>.

The keys is this table are those defined in alacritty terminfo with some
additions from
https://sw.kovidgoyal.net/kitty/keyboard-protocol/#legacy-functional-keys
for the RET ESC DEL TAB and SPC.")

(define-derived-mode mistty-alacritty-mode fundamental-mode "MisTTY/FS"
  "Major mode for Mistty Fullscreen.

This mode provides a raw terminal tied to a subprocess based on the
alacritty library.

Call `mistty-alacritty-exec' to create the virtual terminal and start the
process."
  ;; Face is set manually; disable font-lock mode
  (font-lock-mode -1)
  (jit-lock-mode nil)

  (use-local-map mistty-alacritty-mode-map)
  (setq mistty--translate-key-function #'mistty-alacritty--translate-key))

(defun mistty-alacritty-available-p ()
  "Check whether the module is available.

Calling any other function when this one returns nil will fail."
  (featurep 'mistty-alacritty-vt))

(defun mistty-alacritty-exec (name program args width height)
  "Execute a command inside of an alacritty terminal.

This creates a process NAME that runs PROGRAM with ARGS inside of a
terminal with the given WIDTH and HEIGHT and displays the result in the
current buffer. The created process is set as the current buffer's
process."
  (unless (mistty-alacritty-available-p)
    (error "Alacritty terminal is unavailable; module '%s' not found"
           (mistty-alacritty-modulename)))
  (unless (eq major-mode 'mistty-alacritty-mode)
    (error "Must be called from a mistty-alacritty-mode buffer"))
  (when (get-buffer-process (current-buffer))
    (error "A process is already attached to the buffer"))
  (mistty-log "LAUNCH %s %s" program args)
  (let ((width (or width 80))
        (height (or height 24))
        (process-environment
         (nconc
          (list (concat "TERM=" (mistty-alacritty--TERM))
                (concat "INSIDE_EMACS=" emacs-version))
          process-environment))
        (process-connection-type t)
	(inhibit-eol-conversion t)
	(coding-system-for-read 'binary))
    (jit-lock-mode nil) ;; in case this was turned on by a hook
    (setq mistty-alacritty--cursor (copy-marker (point-min)))
    (setq mistty-alacritty--home (copy-marker (point-min)))
    (set-marker-insertion-type mistty-alacritty--home nil)
    (mistty--init-scrolline mistty-alacritty--home 0)
    (setq mistty-alacritty-columns width)
    (setq mistty-alacritty-lines height)
    (mistty-log "MAKE VTERM %s lines, %s colums" height width)
    (setq mistty-alacritty--vterm (mistty-alacritty-vt-make-vterm width height))
    (mistty-alacritty-vt-enable-scrollback mistty-alacritty--vterm)
    (let ((proc (apply #'start-file-process name (current-buffer)
                       ;; On Android, /bin doesn't exist, and the default shell is
                       ;; found as /system/bin/sh.
	               (if (eq system-type 'android)
                           "/system/bin/sh"
                         "/bin/sh")
                       "-c"
	               (format "stty -nl echo rows %d columns %d sane erase %s 2>%s;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
		               height width
                               (pcase mistty-del
                                       ("\C-h" "^H")
                                       ("\d" "^?"))
                               null-device)
	               ".."
	               program args)))
      ;; Window size must be adjusted manually with mistty-alacritty--resize
      (process-put proc 'adjust-window-size-function nil)

      ;; start-file-process doesn't always respect
      ;; coding-system-for-read. Force it.
      (set-process-coding-system proc 'binary (cdr (process-coding-system proc)))

      (goto-char (point-min))
      (mistty-alacritty-vt-render mistty-alacritty--vterm mistty-alacritty--cursor)
      (goto-char mistty-alacritty--cursor)
      (set-marker (process-mark proc) mistty-alacritty--cursor)
      (set-process-sentinel proc #'mistty-alacritty--sentinel)
      (set-process-filter proc #'mistty-alacritty--process-filter))))

(defun mistty-alacritty-auto-resize (enabled)
  "Track window size and automatically resize the terminal.

Enabling auto-resize might trigger an immediate resize if the terminal
doesn't match the desired window size.

Set ENABLED to non-nil to enable automatic resize to nil to disable it."
  (when-let* ((proc (get-buffer-process (current-buffer))))
    (if enabled
        (progn
          (process-put proc 'adjust-window-size-function #'mistty-alacritty--resize-from-window)
          (when-let* ((wins (get-buffer-window-list)))
            (mistty-alacritty--resize-from-window proc wins)))
      (process-put proc 'adjust-window-size-function nil))))

(defun mistty-alacritty--resize-from-window (proc win)
  "Choose window size and apply it to the virtual terminal.

This is meant to be used as adjust-process-window-size function on the
process. PROC is the process, WIN the set of windows displaying the
process buffer. The current buffer is the process buffer.

This function updates the virtual terminal size and returns the new
size.

Calls `window-adjust-process-window-size' to choose the appropriate size
given the set of windows."
  (when-let* ((size (funcall window-adjust-process-window-size-function proc win)))
    (mistty-alacritty-resize (car size) (cdr size))
    size))

(defun mistty-alacritty-resize (width height)
  "Resize the terminal and pty to WIDTH x HEIGHT."
  (if (or (/= mistty-alacritty-columns width) (/= mistty-alacritty-lines height))
      (when-let* ((vterm mistty-alacritty--vterm)
                  (proc (get-buffer-process (current-buffer))))
        (mistty-log "RESIZE: %s lines %s columns" height width)
        (mistty-alacritty-vt-resize vterm width height)
        (setq mistty-alacritty-columns width)
        (setq mistty-alacritty-lines height)
        (set-process-window-size proc height width))))

(defun mistty-alacritty--alt-screen-p ()
  "Check whether we're displaying the alt screen buffer.

This function returns non-nil when the alt screen buffer is displayed.
This mode is called fullscreen in the rest of the code."
  (mistty-alacritty-vt-alt-screen-p mistty-alacritty--vterm))

(defun mistty-alacritty--cursor-linecol ()
  "Return cursor terminal line and column number.

The return value is a (cons line column)."
  (mistty-alacritty-vt-cursor mistty-alacritty--vterm))

(defun mistty-alacritty--cursor-column ()
  "Return cursor terminal column number."
  (cdr (mistty-alacritty-vt-cursor mistty-alacritty--vterm)))

(defun mistty-alacritty--cursor-chars ()
  "Return char index of the cursor within its line.

Do not confuse it with `mistty-alacritty--cursor-column'"
  (- mistty-alacritty--cursor (save-excursion
                          (goto-char mistty-alacritty--cursor)
                          (pos-bol))))

(defun mistty-alacritty--cursor-line ()
  "Return cursor terminal line number."
  (car (mistty-alacritty-vt-cursor mistty-alacritty--vterm)))

(defun mistty-alacritty--process-filter (proc str)
  "Update the terminal state and render the result.

This is meant to be used as process filter so takes the usual argument
PROC, for the process and STR for the data to send to the terminal."
  (mistty-log "RECV %S" str)
  (mistty--with-live-buffer (process-buffer proc)
    (mistty-alacritty--process-bytes str)
    (unless mistty-alacritty--inhibit-render
      (mistty-alacritty--render))))

(defun mistty-alacritty--process-bytes (str)
  "Send bytes from STR to the virtual terminal to be processed.

The current buffer must have a virtual terminal associated."
  (when-let* ((vterm mistty-alacritty--vterm)
              (proc (get-buffer-process (current-buffer))))
    (dolist (ev (mistty-alacritty-vt-process-bytes vterm (vconcat str)))
      (pcase ev
        (`(pty-write ,data)
         (mistty-log "REPLY %S" data)
         (process-send-string proc data))
        (`(title ,title)
         (mistty-log "TITLE %S" title)
         (setq ansi-osc-window-title title))))))

(defun mistty-alacritty--render ()
  "Render the virtual terminal on the current buffer.

The current buffer must have a virtual terminal associated."
  (when-let* ((vterm mistty-alacritty--vterm))
    (save-excursion
      (goto-char mistty-alacritty--home)
      (pcase-let ((`(,screen-top . ,scrollback-lines)
                   (mistty-alacritty-vt-render vterm mistty-alacritty--cursor)))
        (mistty-alacritty-vt-clear-scrollback vterm)
        (cl-incf mistty--scrolline-home-num scrollback-lines)
        (mistty-log "RENDER @%s (+%s)"
                    mistty--scrolline-home-num scrollback-lines)
        (set-marker mistty-alacritty--home screen-top))
      (when-let* ((proc (get-buffer-process (current-buffer))))
        (when (process-live-p proc)
          (set-marker (process-mark proc) mistty-alacritty--cursor))))
    (goto-char mistty-alacritty--cursor)))

(defun mistty-alacritty--sentinel (proc msg)
  "Update buffer when PROC has exited.

MSG is displayed at the end of the buffer, to let the user know the
process is dead."
  (when (memq (process-status proc) '(signal exit))
    (mistty--with-live-buffer (process-buffer proc)
      (save-excursion
        (goto-char (point-max))
        (insert "\nProcess " msg)))
    (set-process-buffer proc nil)
    (delete-process proc)))

(defun mistty-alacritty--TERM ()
  "Choose a value for the TERM env variable.

This is controlled by the custom variable `mistty-alacritty-term-name'"
  (cond
   (mistty-alacritty-term-name mistty-alacritty-term-name)
   ((shell-command-to-string "infocmp alacritty") "alacritty")
   (t "xterm-256color")))

(defun mistty-alacritty--clear-to-eol (pos)
  "Mark spaces from POS to the end of the line as clear."
  (when-let* ((vterm mistty-alacritty--vterm))
    (when (> pos mistty-alacritty--home)
      (mistty-alacritty-vt-clear-to-eol vterm
                               (mistty--count-lines mistty-alacritty--home pos)
                               (- pos (mistty--bol pos))))))

(defun mistty-alacritty--cleanup-prompt-sp (pos)
  "Cleanup after the shell using the prompt-sp hack.

POS should be the position where the CR is called in the prompt-sp
sequence."
  (when-let* ((vterm mistty-alacritty--vterm))
    (when (> pos mistty-alacritty--home)
      (mistty-alacritty-vt-cleanup-prompt-sp
       vterm
       (mistty--count-lines mistty-alacritty--home pos)))))

(defun mistty-alacritty--translate-key (key n)
  "Return the byte sequence for KEY N times appropriate for the terminal."
  (mistty--translate-key-default key n mistty-alacritty--key-map))

(provide 'mistty-alacritty)

;;; mistty-alacritty.el ends here
