;;; mistty-alacritty.el --- Use the alacritty library to run the terminall -*- lexical-binding: t -*-

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
;; This file defines a terminal backend as described in
;; mistty-term.el that is based on the alacritty-vt module.

(require 'cl-lib)
(require 'seq)
(require 'mistty-term)
(require 'mistty-custom)
(require 'mistty-prompt)
(require 'mistty-accum)
(require 'mistty-kbd)
(require 'mistty-scrolline)
(require 'mistty-sync)
(require 'mistty-util)
(eval-when-compile
  (require 'mistty-accum-macros))

(require 'ansi-osc) ; links in module use ansi-osc-hyperlink
(defvar explicit-shell-file-name) ;; defined in shell

;;; Code:


;; Loading the module is optional here, so this file can be safely
;; loaded and compiled. Availability check should be done dynamically
;; dynamically using (mistty-alacritty-available-p)

(defconst mistty-alacritty-version "2.3.2-alpha"
  "Mistty module version name, matching the version in Cargo.toml.

This is used to download and load the correct version of the module.")

(defvar mistty-development-mode t
  "Turn on development mode, switching module version to dev.

With this turned on, `mistty-alacritty-version' is ignored and the
module is loaded from the development version. Turn it on at your on
risks.")

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
          (if mistty-development-mode "dev" mistty-alacritty-version)
          mistty-alacritty-arch
          module-file-suffix))

(defvar mistty--alacritty-loaded nil
  "Name of the module that was loaded.

`mistty-alacritty-modulename' returns the module that should be loaded.
This variable keeps track of the module that was actually loaded.")

(defun mistty-alacritty-check-version (&optional noerror)
  "Check that right version of the module is loaded.

This function raises an error unless the right version of the module is
loaded, unless NOERROR is non-nil, in which case this function returns a
boolean."
  (interactive)
  (let (module-version)
    (cond
     ((and
       (featurep 'mistty-alacritty-vt)
       (string= mistty-alacritty-version
                (setq module-version
                      (condition-case nil
                          (mistty-alacritty-vt-version)
                        (void-function "unspecified")))))

        t)
     (noerror nil)
     ((not module-version)
      (error "Module %s not loaded yet" (mistty-alacritty-modulename)))
     (t
      (error "Restart Emacs to load mistty-alacritty-vt %s (Loaded %s, from %s)"
             mistty-alacritty-version
             module-version
             (or mistty--alacritty-loaded "unknown file"))))))

(defun mistty-alacritty-load (&optional noerror)
  "Attempt to load the module.

Return non-nil if the module is found and could be loaded, this function
returns. Return nil if the module is not found.

This function might fail if the module is found, but cannot be loaded
for some reason unless NOERROR is non-nil."
  (let ((modulename))
    (cond
     ((featurep 'mistty-alacritty-vt)
      (mistty-alacritty-check-version noerror))

     ((load (setq modulename (mistty-alacritty-modulename)) 'noerror 'nomessage)
      (setq mistty--alacritty-loaded modulename)
      (when mistty-development-mode
        (message "Loaded module %s (MisTTY Development mode on)" modulename))
      (if (and (not noerror) (not (featurep 'mistty-alacritty-vt)))
        (error "Module doesn't define feature mistty-alacritty-vt: %s" modulename))

      (mistty-alacritty-check-version noerror))

     ;; not found
     (t nil))))

;; These declarations allow compiling without loading the module.
(declare-function mistty-alacritty-vt-alt-screen-p nil (term))
(declare-function mistty-alacritty-vt-version nil ())
(declare-function mistty-alacritty-vt-cleanup-prompt-sp nil (term pos))
(declare-function mistty-alacritty-vt-detect-dead-spaces-after-insert nil (term content scrolline pos-in-line))
(declare-function mistty-alacritty-vt-collect-changes nil (term))
(declare-function mistty-alacritty-vt-cursor nil (term))
(declare-function mistty-alacritty-vt-enable-scrollback nil (term))
(declare-function mistty-alacritty-vt-end-regexp-prompt nil (term))
(declare-function mistty-alacritty-vt-flush-scrollback nil (term))
(declare-function mistty-alacritty-vt-kitty-key-seq nil (term key mods basic-type))
(declare-function mistty-alacritty-vt-make-vterm nil (w h options-plist))
(declare-function mistty-alacritty-vt-mode-alist nil (term))
(declare-function mistty-alacritty-vt-mouse-seq nil (term action button mods line column))
(declare-function mistty-alacritty-vt-process-bytes nil (term bytes))
(declare-function mistty-alacritty-vt-render nil (term cursor skip_above))
(declare-function mistty-alacritty-vt-render-screen nil (term cursor))
(declare-function mistty-alacritty-vt-resize nil (term w h))
(declare-function mistty-alacritty-vt-scrollback-line-count nil (term))
(declare-function mistty-alacritty-vt-scrolline-at-screen-top nil (term))
(declare-function mistty-alacritty-vt-start-regexp-prompt nil (term start))
(declare-function mistty-alacritty-vt-terminal-line-for-scrolline nil (term scrolline))
(declare-function mistty-alacritty-vt-update-options nil (term options-plist))

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
    (define-key map (kbd "<down>") "\e[B")
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
    (define-key map (kbd "<left>") "\e[D")
    (define-key map (kbd "<menu>") "\e[1;2S") ; kf16
    (define-key map (kbd "<next>") "\e[6~") ; knp
    (define-key map (kbd "<prior>") "\e[5~") ; kpp
    (define-key map (kbd "<right>") "\e[C")
    (define-key map (kbd "<up>") "\e[A")
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

(defvar mistty-alacritty--app-cursor-key-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "<up>") "\eOA")
      (define-key map (kbd "<down>") "\eOB")
      (define-key map (kbd "<right>") "\eOC")
      (define-key map (kbd "<left>") "\eOD")

      map)
    "Map used when Cursor Key Application Mode (DECCKM) is enabled.")

(defun mistty-alacritty-available-p ()
  "Check whether the module is available.

Calling any other function when this one returns nil will fail."
  (featurep 'mistty-alacritty-vt))

(defun mistty-alacritty--make-vterm (width height)
  "Create a terminal with the given WIDTH and HEIGHT."
  (unless (mistty-alacritty-load)
    (error "Alacritty terminal is unavailable; module '%s' not found"
           (mistty-alacritty-modulename)))

  (let ((vterm (mistty-alacritty-vt-make-vterm
                width height
                `(:osc52 ,mistty-alacritty-osc52
                  :allow-clearing-scrollback ,mistty-allow-clearing-scrollback))))
    (mistty-log "MAKE VTERM %s lines, %s colums" height width)
    (mistty-alacritty-vt-enable-scrollback vterm)

    vterm))

(defun mistty-alacritty--exec (name program args width height)
  "Execute a command inside and tie it to the current buffer.

This creates a process NAME that runs PROGRAM with ARGS. The created
process is set as the current buffer's process. A filter should be set
after making this call as this function sets the filter to t.

The process window size is declared to be WIDTH x HEIGHT.

Returns the process."
  (when (get-buffer-process (current-buffer))
    (error "A process is already attached to the buffer"))
  (mistty-log "LAUNCH %s %s" program args)
  (let ((process-connection-type t)
	(inhibit-eol-conversion t)
	(coding-system-for-read 'binary))
    (let ((proc (apply #'start-file-process name (current-buffer)
                       ;; On Android, /bin doesn't exist, and the default shell is
                       ;; found as /system/bin/sh.
	               (if (eq system-type 'android)
                           "/system/bin/sh"
                         "/bin/sh")
                       "-c"
	               (format "stty -nl echo rows %d columns %d sane iutf8 erase ^? 2>%s;\
if [ $1 = .. ]; then shift; fi; exec \"$@\""
		               height width
                               null-device)
	               ".."
	               program args)))
      (set-process-query-on-exit-flag proc nil)

      ;; Window size must be adjusted manually with mistty-alacritty--resize
      (process-put proc 'adjust-window-size-function #'ignore)

      ;; start-file-process doesn't always respect
      ;; coding-system-for-read. Force it.
      (set-process-coding-system proc 'binary (cdr (process-coding-system proc)))
      (set-process-sentinel proc nil)
      (set-process-filter proc t)

      proc)))

(defun mistty-alacritty--TERM ()
  "Choose a value for the TERM env variable.

This is controlled by the custom variable `mistty-alacritty-term-name'"
  (cond
   (mistty-alacritty-term-name mistty-alacritty-term-name)
   ((shell-command-to-string "infocmp alacritty") "alacritty")
   (t "xterm-256color")))

(defconst mistty--term-alacritty-buf-name " **mistty render**")

(cl-defstruct (mistty--term-alacritty
               (:constructor mistty--make-term-alacritty)
               (:copier nil))
  ;; associated process
  proc

  ;; intermediate buffer used when rendering
  buf

  ;; virtual terminal, from mistty-alacritty-vt module
  vterm

  ;; current terminal width
  columns

  ;; current terminal height
  lines

  ;; if non-nil, the kitty keyboard protocol is enabled
  kkp

  ;; if non-nil, Cursor Key Application Mode (DECCKM) is enabled
  app-cursor

  ;; if non-nil, we are in fullscreen mode
  fs

  ;; mouse tracking state; the value sent to the last call to
  ;; request-report-mouse
  mouse

  ;; struct of type mistty--prompt-cell used to track prompts
  prompt-cell

  ;; callback to send changes to what mouse events should be reported
  request-report-mouse

  ;; function to call when entering fullscreen
  enter-fullscreen

  ;; function to call when leaving fullscreen
  leave-fullscreen

  ;; function to call after clearing the screen
  after-clear-screen

  ;; function to call after resetting the terminal
  after-reset

  ;; function to call to show or hide cursor
  show-cursor

  ;; function to call when reporting focus in/out events is requested
  ;; or turned off.
  report-focus

  ;; status message, set after the process died
  process-status)

(cl-defmethod mistty--create-term
  ((_type (eql 'alacritty)) name command
   &key width height
   prompt-cell
   enter-fullscreen
   leave-fullscreen
   after-clear-screen
   after-reset
   clear-scrollback
   sync-scrolline
   show-cursor
   request-report-mouse
   report-focus)
  "Create an alacritty-type terminal with the given NAME and COMMAND.

If WIDTH and HEIGHT are specified, they'll be used as terminal line and
column count. The default is 80x24.

PROMPT-CELL specifies a mistty--prompt-cell struct used to track
prompts.

ENTER-FULLSCREEN and LEAVE-FULLSCREEN handle fullscreen transitions.
AFTER-CLEAR-SCREEN is called after the screen is cleared.
AFTER-RESET is called after the terminal was reset.
CLEAR-SCROLLBACK clears scrollback in the work buffer.
SYNC-SCROLLINE returns the current sync scrolline.
SHOW-CURSOR shows (non-nil argument) or hides (nil argument) the cursor
REQUEST-REPORT-MOUSE controls mouse reporting.
REPORT-FOCUS control reporting of focus in/out events."
  (let* ((width (or width 80))
         (height (or height 24))
         ;; Take terminal options from possibly buffer-local values
         ;; from calling buffer, the work buffer.
         (vterm (mistty-alacritty--make-vterm width height))
         (program (car command))
         (args (cdr command))
         (process-environment
          (append
           (with-connection-local-variables mistty-process-env)
           (list (concat "TERM=" (mistty-alacritty--TERM))
                 (concat "INSIDE_EMACS=" emacs-version))
           (if (with-connection-local-variables mistty-set-EMACS)
               (cons (format "EMACS=%s" emacs-version)
                     process-environment)
             process-environment)))
         (proc (mistty-alacritty--exec name program args width height))
         (term (mistty--make-term-alacritty
                :proc proc
                :vterm vterm
                :lines height
                :columns width
                :prompt-cell prompt-cell
                :request-report-mouse request-report-mouse))
         (accum (mistty--make-accumulator
                 (mistty--term-alacritty-filter term)
                 'immediate-preproc)))
    (set-process-filter proc accum)
    (process-put proc 'mistty-term term)

    (unless enter-fullscreen (error ":enter-fullscreen required"))
    (setf (mistty--term-alacritty-enter-fullscreen term) enter-fullscreen)

    (unless leave-fullscreen (error ":leave-fullscreen required"))
    (setf (mistty--term-alacritty-leave-fullscreen term) leave-fullscreen)

    (unless after-clear-screen (error ":after-clear-screen required"))
    (setf (mistty--term-alacritty-after-clear-screen term) after-clear-screen)

    (unless after-reset (error ":after-reset required"))
    (setf (mistty--term-alacritty-after-reset term) after-reset)

    (unless show-cursor (error ":show-cursor required"))
    (setf (mistty--term-alacritty-show-cursor term) show-cursor)

    (unless report-focus (error ":report-focus required"))
    (setf (mistty--term-alacritty-report-focus term) report-focus)

    (ignore sync-scrolline clear-scrollback)

    term))

(cl-defmethod mistty--term-handle-process-death ((term mistty--term-alacritty) msg)
  "Deal with the process of TERM having died with message MSG."
  (let ((proc (mistty--term-alacritty-proc term)))
    (mistty-log "PROCESS %s" msg)
    (setf (mistty--term-alacritty-process-status term) msg)
    (set-process-buffer proc nil)))

(cl-defmethod mistty--term-delete ((term mistty--term-alacritty))
  "Delete process and cleanup resources for TERM.

MSG may be nil or may be a message passed to the process sentinel.

The process and/or its associated buffer might have already been killed
when this function is called."
  (when (process-live-p (mistty--term-alacritty-proc term))
    (delete-process (mistty--term-alacritty-proc term)))
  (let ((buf (mistty--term-alacritty-buf term)))
    (when (and buf (buffer-live-p buf))
      (kill-buffer buf))))

(cl-defmethod mistty--term-update-options ((term mistty--term-alacritty) &rest plist)
  "Set TERM options.

PLIST specifies the options to set. Possible keys:

- :osc52 nil|\\='only-copy|\\='only-paste|\\='copy-paste
- :allow-clearing-scrollback boolean

Unknown options are ignored."
  (mistty-alacritty-vt-update-options
   (mistty--term-alacritty-vterm term) plist))

(cl-defmethod mistty--term-proc ((term mistty--term-alacritty))
  "Return TERM's associated process."
  (mistty--term-alacritty-proc term))

(cl-defmethod mistty--term-scrolline-at-screen-top ((term mistty--term-alacritty))
  "Return the scrolline number of the first line of TERM's terminal."
  (mistty-alacritty-vt-scrolline-at-screen-top (mistty--term-alacritty-vterm term)))

(cl-defmethod mistty--term-lines ((term mistty--term-alacritty))
  "Return the number of lines in TERM (its height)."
  (mistty--term-alacritty-lines term))

(cl-defmethod mistty--term-columns ((term mistty--term-alacritty))
  "Return the number of columns in TERM (its width)."
  (mistty--term-alacritty-columns term))

(cl-defmethod mistty--term-resize ((term mistty--term-alacritty) width height)
  "Resize TERM to WIDTH columns and HEIGHT lines."
  (if (or (/= (mistty--term-alacritty-columns term) width)
          (/= (mistty--term-alacritty-lines term) height))
      (when-let* ((vterm (mistty--term-alacritty-vterm term))
                  (proc (mistty--term-alacritty-proc term)))
        (mistty-log "RESIZE: %s lines %s columns" height width)
        (mistty-alacritty-vt-resize vterm width height)
        (setf (mistty--term-alacritty-columns term) width)
        (setf (mistty--term-alacritty-lines term) height)
        (set-process-window-size proc height width))))

(cl-defmethod mistty--term-sync
  ((term mistty--term-alacritty) dest-buffer sync-pos sync-scrolline keep-markers
   cursor-marker)
  "Update DEST-BUFFER to match the terminal TERM.

The buffer from SYNC-POS to EOB is rewritten to match TERM's content below
SYNC-SCROLLINE.  If KEEP-MARKERS is non-nil, keep markers and overlays.
CURSOR-MARKER is updated to the cursor position."
  ;; First, collect changes and update state
  (let ((vterm (mistty--term-alacritty-vterm term))
        (sync-pos (mistty--with-live-buffer dest-buffer
                    (copy-marker sync-pos)))
        (forwarded-events nil))
    (unwind-protect
        (progn
          (dolist (ev (mistty-alacritty-vt-collect-changes vterm))
            (mistty-log "EV %S" ev)
            (pcase ev
              ;; terminal changes

              (`(title . ,title)
               (mistty--with-live-buffer dest-buffer
                 (setq ansi-osc-window-title title)))
              (`(cwd . ,url)
               (when url
                 (mistty--with-live-buffer dest-buffer
                   (mistty-osc7 "7" url))))
              (`(cleared-screen)
               (funcall (mistty--term-alacritty-after-clear-screen term)))
              (`(reset)
               (funcall (mistty--term-alacritty-after-reset term)))
              (`(cleared-scrollback)
               (mistty--with-live-buffer dest-buffer
                 (delete-region (point-min) sync-pos)))
              (`(kkp . ,val)
               (setf (mistty--term-alacritty-kkp term) val))
              (`(mouse . ,val)
               (setf (mistty--term-alacritty-mouse term) val)
               (funcall (mistty--term-alacritty-request-report-mouse term) val))
              (`(cursor . ,val)
               (funcall (mistty--term-alacritty-show-cursor term) val))
              (`(app-cursor . ,val)
               (setf (mistty--term-alacritty-app-cursor term) val))
              (`(report-focus . ,val)
               (funcall (mistty--term-alacritty-report-focus term) val))
              (`(bracketed-paste . ,val)
               (mistty--with-live-buffer dest-buffer
                 (setq mistty-bracketed-paste val)))
              (`(fullscreen . ,val)
               (setf (mistty--term-alacritty-fs term) val)
               (if val
                   (funcall (mistty--term-alacritty-enter-fullscreen term))
                 (funcall (mistty--term-alacritty-leave-fullscreen term))))
              (`(modified . ,val)
               (when (< val sync-scrolline)
                 (mistty--with-live-buffer dest-buffer
                   (setq sync-scrolline val)
                   (set-marker sync-pos
                               (or (car (mistty--find-marked-scrolline val))
                                   (point-min)))
                   (mistty-log "REALIGN to %s@%s"
                               sync-scrolline (marker-position sync-pos)))))

              ;; prompt changes
              (`(start-prompt ,source ,start ,replace)
               (cl-block nil
                 (let* ((cell (mistty--term-alacritty-prompt-cell term))
                        (prompt (mistty--get-prompt cell)))
                   (when (and prompt replace)
                     (setf (mistty--prompt-source prompt) source)
                     (setf (mistty--prompt-start prompt) start)
                     (setf (mistty--prompt-end prompt) nil)
                     (mistty-log "Updated %s prompt #%s [%s-]"
                                 (mistty--prompt-source prompt)
                                 (mistty--prompt-input-id prompt)
                                 (mistty--prompt-start prompt))
                     (cl-return))

                   (when (and prompt (null (mistty--prompt-end prompt)))
                     (setf (mistty--prompt-end prompt) start)
                     (mistty-log "Ended %s prompt (unexpected) #%s [%s-%s]"
                                 (mistty--prompt-source prompt)
                                 (mistty--prompt-input-id prompt)
                                 (mistty--prompt-start prompt)
                                 start))

                   (setq prompt (mistty--make-prompt :source source :start start))
                   (mistty--set-prompt cell prompt)
                   (mistty-log "Detected %s prompt #%s [%s-]"
                               (mistty--prompt-source prompt)
                               (mistty--prompt-input-id prompt)
                               (mistty--prompt-start prompt)))))
              (`(end-prompt ,end)
               (when-let* ((prompt (mistty--get-prompt
                                    (mistty--term-alacritty-prompt-cell term))))
                 (setf (mistty--prompt-end prompt) end)
                 (mistty-log "Ended %s prompt #%s [%s-%s]"
                             (mistty--prompt-source prompt)
                             (mistty--prompt-input-id prompt)
                             (mistty--prompt-start prompt)
                             end)))
              (`(update-prompt . ,args-plist)
               (when-let* ((prompt (mistty--get-prompt
                                    (mistty--term-alacritty-prompt-cell term))))
                 (when-let* ((val (plist-get args-plist :user-input-start)))
                   (setf (mistty--prompt-user-input-start prompt) val)
                   (mistty-log "Updated %s prompt #%s [%s-%s] :user-input-start %s"
                               (mistty--prompt-source prompt)
                               (mistty--prompt-input-id prompt)
                               (mistty--prompt-start prompt)
                               (mistty--prompt-end prompt)
                               val))
                 (when-let* ((cmdline (plist-get args-plist :cmdline)))
                   (setf (mistty--prompt-cmdline prompt) cmdline)
                   (mistty-log "Updated %s prompt #%s [%s-%s] :cmdline %S"
                               (mistty--prompt-source prompt)
                               (mistty--prompt-input-id prompt)
                               (mistty--prompt-start prompt)
                               (mistty--prompt-end prompt)
                               cmdline))
                 (when-let* ((val (plist-get args-plist :click-events)))
                   (setf (mistty--prompt-click-events prompt) val)
                   (mistty-log "Updated %s prompt #%s [%s-%s] :click-events %S"
                               (mistty--prompt-source prompt)
                               (mistty--prompt-input-id prompt)
                               (mistty--prompt-start prompt)
                               (mistty--prompt-end prompt)
                               val))))

              (`(command-finished . ,_)
               (push ev forwarded-events))

              (_ (error "Unhandled event: %S" ev))))

          ;; Sync or render
          (cond
           ;; direct mode, in fullscreen and outside of a prompt
           ((or (mistty--term-alacritty-fs term) (not keep-markers))
            (mistty--with-live-buffer dest-buffer
              (let ((old-point (when (< (point) sync-pos (point-max))
                                 (point))))
                (goto-char sync-pos)
                (pcase-let ((`(,new-start . ,new-scrolline)
                             (mistty-alacritty-vt-render vterm cursor-marker sync-scrolline)))
                  (mistty-log "DIRECT RENDER [%s,..) POS %s (+%s)"
                              sync-scrolline
                              (marker-position sync-pos)
                              (- new-scrolline sync-scrolline))
                  (set-marker sync-pos new-start)
                  (setq sync-scrolline new-scrolline))
                (when (mistty--term-alacritty-fs term)
                  (put-text-property sync-pos (point-max) 'read-only t))
                (if old-point
                    (goto-char old-point)
                  (goto-char cursor-marker)))))

           ;; On a prompt, rendering first to term-buf then syncing,
           ;; to keep markers.
           (t
            (let ((render-buf (mistty--term-alacritty-get-or-create-buf term)))
              (pcase-let ((`(,new-start . ,new-scrolline)
                           (with-current-buffer render-buf
                             (goto-char 1)
                             (mistty-alacritty-vt-render vterm cursor-marker sync-scrolline))))
                (mistty--sync-buffers render-buf 1 dest-buffer sync-pos)
                (set-marker cursor-marker
                            (+ sync-pos (1- cursor-marker))
                            dest-buffer)
                (set-marker sync-pos
                            (+ sync-pos (1- new-start))
                            dest-buffer)
                (setq sync-scrolline new-scrolline)

                (mistty-alacritty-vt-flush-scrollback vterm)
                (when (> new-start 1)
                  (mistty--with-live-buffer render-buf
                    ;; Clear scrollback to allow partial rendering next
                    ;; time. This must be done after computing the
                    ;; positions to not mess with the cursor marker.
                    (delete-region (point-min) new-start)))))))

          (mistty--term-alacritty-mark-final-newline sync-pos)

          ;; Report process status, if necessary
          (when-let* ((msg (mistty--term-alacritty-process-status term)))
            (save-excursion
              (goto-char (point-max))
              (insert "Process " msg)))

          (list (marker-position sync-pos)
                sync-scrolline
                (nreverse forwarded-events)))

      ;; unwind-protect cleanup
      (set-marker sync-pos nil))))


(cl-defmethod mistty--term-sync-backstage ((term mistty--term-alacritty) sync-scrolline cursor)
  "Render TERM into the current buffer from SYNC-SCROLLINE.

The entire current buffer is rewritten to match the content of the
terminal starting at SYNC-SCROLLINE in a way that keeps marker positions
as much as possible.

The marker CURSOR is set at the cursor position, as long as the cursor is at or
below SYNC-SCROLLINE, otherwise the cursor is moved to the beginning of
the buffer after sync."
  (let* ((vterm (mistty--term-alacritty-vterm term))
         (render-buf (mistty--term-alacritty-get-or-create-buf term))
         (mode-alist (mistty-alacritty-vt-mode-alist vterm)))
    (when (alist-get 'fullscreen mode-alist)
      (error "Terminal entered fullscreen mode; replay not possible"))
    (setq mistty-bracketed-paste (alist-get 'bracketed-paste mode-alist))

    ;; replace-region-contents updates markers automatically
    (with-current-buffer render-buf
      (set-marker cursor (point-min))
      (goto-char (point-min))
      (mistty-alacritty-vt-render vterm cursor sync-scrolline))
    (mistty--sync-buffers render-buf 1 (current-buffer) 1)
    (mistty--term-alacritty-mark-final-newline (point-min))

    ;; even though the cursor is for render-buf, right now, the position
    ;; is the same.
    (set-marker cursor (marker-position cursor))
    (goto-char cursor)))

(cl-defmethod mistty--term-detect-dead-spaces-after-insert
  ((term mistty--term-alacritty) content scrolline pos-in-line)
  "Mark dead trailing spaces left in TERM after inserting CONTENT.

When inserting a newline in an existing line, the terminal often just
overwrites the existing characters with space instead of re-creating the
line properly. The result are spaces that should be skipped.

SCROLLINE is the scrolline at which CONTENT was inserted in the terminal
buffer and POS-IN-LINE is the character position within that scrolline where it
was inserted."
  (mistty-alacritty-vt-detect-dead-spaces-after-insert
   (mistty--term-alacritty-vterm term)
   content
   scrolline
   pos-in-line))

(cl-defmethod mistty--term-translate-key ((term mistty--term-alacritty) key &optional n)
  "Generate the key byte sequence for TERM.

KEY is an Emacs key event and N the number of repetition for that event.

The function returns the byte sequence appropriate for sending that key
to the terminal."
  (let ((n (or n 1)))
    (if (mistty--term-alacritty-kkp term)
        ;; follow the kitty keyboard protocol
        (let ((seq (if (and (length= key 1)
                            (memq (aref key 0) '(?\C-c ?\C-f ?\C-d)))
                       ;; Keep these keys as-is, to get out of issues
                       ;; in case things go wrong. They're handled
                       ;; specially be the terminal.
                       key
                    (mapconcat
                     (lambda (ev)
                       (mistty-alacritty-vt-kitty-key-seq
                        (mistty--term-alacritty-vterm term)
                        ev
                        (vconcat (event-modifiers ev))
                        (event-basic-type ev)))
                     key
                     ""))))
          (if (length= seq 0)
              nil
            (mistty--repeat-string n seq)))

      ;; legacy translation
      (mistty--translate-key-default
       key n
       (when (mistty--term-alacritty-app-cursor term)
         mistty-alacritty--app-cursor-key-map)
       mistty-alacritty--key-map))))

(cl-defmethod mistty--term-list-special-keys ((_type (eql 'alacritty)))
  "List the basic type of special event types (special keys)."
  (mistty--list-basic-types-from-map mistty-alacritty--key-map))

(defun mistty--term-alacritty-filter (term)
  "Build a filter function for TERM.

The filter updates the vterm and optionally renders to the terminal
buffer."
  (lambda (proc str)
    (mistty-log "RECV %S" str)
    (let ((vterm (mistty--term-alacritty-vterm term)))
      (when-let* ((reply (mistty-alacritty-vt-process-bytes vterm str)))
        (mistty-log "REPLY %S" reply)
        (process-send-string proc reply)))))

(cl-defmethod mistty--term-translate-mouse ((term mistty--term-alacritty) action button mods start-posn move-posn sync-pos)
  "Translate a mouse event for TERM into an escape sequence or nil.

ACTION is \\='press \\='release or \\='motion

BUTTON is the number of the pressed button or nil.

MODS are the event modifiers, as reported by `event-modifiers'.

START-POS is the start position reported by the event.

SYNC-POS is the position at which the terminal is synced. It's only

If MOVE-POSN is non-nil, it is the position of the mouse moved after EV
happened. This is used to report drag events.

SYNC-POS is the position at which the terminal is synced. It's only
guaranteed to correspond to the terminal screen top in fullscreen mode."
  (when-let* ((params (mistty--alacritty-mouse-event-params
                       term
                       action
                       button
                       mods
                       start-posn
                       move-posn
                       sync-pos)))
    (mistty-log "MOUSE EVENT PARAMS: %s" params)
    (apply #'mistty-alacritty-vt-mouse-seq
           (mistty--term-alacritty-vterm term) params)))

(cl-defmethod mistty--term-click-on ((term mistty--term-alacritty) target sync-pos)
  "Generate a click for TERM on buffer position TARGET.

TARGET is a position in the terminal zone of current buffer, which
starts at SYNC-POS. Scrollines must be set correctly on the buffer.

SYNC-POS is the position in the current buffer at which the terminal is
synced. It's only guaranteed to correspond to the terminal screen top in
fullscreen mode."
  (ignore term target sync-pos)

  (let ((vterm (mistty--term-alacritty-vterm term))
        (y (mistty--alacritty-terminal-line-at-pos
            term target sync-pos))
        (x (save-excursion
             (goto-char target)
             (current-column))))
    (mistty-log "CLICK on %s, %s" x y)
    (mistty--string-empty-to-nil
      (concat
       (mistty-alacritty-vt-mouse-seq
        vterm 'press 1 [] x y)
       (mistty-alacritty-vt-mouse-seq
        vterm 'release 1 [] x y)))))

(cl-defmethod mistty--term-start-regexp-prompt ((term mistty--term-alacritty) start)
  "Start a regexp prompt for TERM at START and make it current."
  (mistty-alacritty-vt-start-regexp-prompt (mistty--term-alacritty-vterm term) start))

(cl-defmethod mistty--term-end-regexp-prompt ((term mistty--term-alacritty))
  "End the current regexp prompt for TERM, if any."
    (mistty-alacritty-vt-end-regexp-prompt (mistty--term-alacritty-vterm term)))

(cl-defmethod mistty--term-mouse-reporting ((term mistty--term-alacritty))
  "Return the current mouse reporting level for TERM."
  (mistty--term-alacritty-mouse term))

(defun mistty--alacritty-mouse-event-params
    (term action button mods start-posn move-posn sync-pos)
  "Convert TERM mouse event for `mistty-alacritty-vt-translate-mouse'.

The current buffer is the butter that's going to be used to generate the
event position. It must contain a subset of the terminal content, with
scrollines matching the terminal's.

The arguments ACTION BUTTON MODS START-POSN MOVE-POSN and SYNC-POS as
the same as those of `mistty--term-translate-mouse', which see.

Return a terminal sequence or nil if conversion fails or if mouse
reporting for this event is not enabled in the terminal.."
  (cl-block nil
    (let* ((win (posn-window start-posn))
           (posn (or move-posn start-posn))
           (other-win (posn-window posn))
           term-x term-y)
      (unless (and (null (posn-area start-posn)) ; window body
                   (window-valid-p win)
                   (or button (eq action 'motion))
                   action)
        (cl-return nil))

      ;; Compute the corresponding position in the terminal to fill in
      ;; term-y and term-y, from the window and buffer positions
      ;; reported by posn. This is done in a way that avoids relying
      ;; on the lines reported by Emacs, as these don't always account
      ;; for varying line heights, relying on buffer-pos as much as
      ;; possible..
      (let* ((same-window (eq win other-win))
             (same-window-point (when same-window (posn-point posn)))

             ;; Build win-x-y to relative to win, even if the
             ;; coordinates are outside the window.
             (win-x-y (if same-window
                          (posn-x-y posn)
                        (pcase-let* ((`(,start-x ,start-y . _)
                                     (window-inside-pixel-edges win))
                                    (`(,other-x ,other-y . _)
                                     (if (framep other-win)
                                         '(0 0 0 0)
                                       (window-inside-pixel-edges other-win)))
                                    (`(,x . ,y) (posn-x-y posn)))
                          (cons (+ x other-x (- start-x))
                                (+ y other-y (- start-y))))))

             ;; Buffer-pos is a buffer position inside the terminal
             ;; zone that's visible on win.
             (buffer-pos (or same-window-point
                             (max sync-pos (window-start win))))

             ;; If non-nil, use the y position of buffer-pos.
             ;; If nil compute y coordinates relative to buffer-pos.
             (at-buffer-pos-line
              (when same-window-point
                (save-excursion
                  (goto-char same-window-point)
                  ;; not at buffer end
                  (not (looking-at-p "\n*\\'"))))))

        (setq term-x (/ (car win-x-y) (default-font-width)))
        (setq term-y (mistty--alacritty-terminal-line-at-pos
                      term buffer-pos sync-pos))
        (unless at-buffer-pos-line
          (cl-incf term-y
                   (/ (- (cdr win-x-y)
                         (cdr (posn-x-y
                               (posn-at-point buffer-pos win))))
                      (default-line-height)))))

      (list action button (vconcat mods) term-x term-y))))

(defun mistty--alacritty-terminal-line-at-pos (term buffer-pos sync-pos)
  "Return the terminal line in TERM that correspond to BUFFER-POS.

SYNC-POS marks the beginning of the terminal zone."
  (if (mistty--term-alacritty-fs term)
      ;; Fullscreen: Conversion is straightforward, since the
      ;; full terminal is shown, starting at sync-pos. It
      ;; includes empty lines at the end.
      (mistty--count-lines sync-pos buffer-pos)

    ;; Prompt: Find a scrolline, transform it to terminal line, then
    ;; account for the difference.
    (save-excursion
      (goto-char sync-pos)
      (when-let* ((line (mistty-alacritty-vt-terminal-line-for-scrolline
                         (mistty--term-alacritty-vterm term)
                         (mistty--scrolline-at-point))))
        (+ line (mistty--count-lines sync-pos buffer-pos))))))

(defun mistty--term-alacritty-mark-final-newline (sync-pos)
  "Mark the final newline rendered by alacritty as \\='empty-lines-at-eob.

Newline is only marked if after SYNC-POS."
  (let ((last-newline (1- (point-max))))
    (when (and (> last-newline sync-pos)
               (eq ?\n (char-after last-newline)))
      (add-text-properties
       last-newline (point-max)
       '(mistty-skip empty-lines-at-eob
                     yank-handler (nil "" nil nil))))))

(defun mistty--term-alacritty-get-or-create-buf (term)
  "Get TERM's render buffer or create a new one."
  (let ((buf (mistty--term-alacritty-buf term)))
    (if (and buf (buffer-live-p buf))
        buf
      (setq buf (generate-new-buffer
                 mistty--term-alacritty-buf-name 'inhibit-buffer-hooks))
      (setf (mistty--term-alacritty-buf term) buf)
      (with-current-buffer buf
        (jit-lock-mode nil))

      buf)))


(defun mistty--find-marked-scrolline (scrolline)
  "Find a scrolline using the \\='mistty-scrollback mark.

This function is meant to find SCROLLINE in the scrollback buffer zone,
which might have been modified. If SCROLLINE <
`mistty--scrolline-home-num', this function returns nil.

Return nil or (cons POS ACTUAL) with ACTUAL possibly being smaller than
SCROLLINE."
  (let ((prop (save-excursion
                (goto-char mistty--scrolline-home-mark)
                (text-property-search-backward
                 'mistty-scrolline
                 scrolline ;; goal
                 (lambda (goal val)
                   (and val (<= val goal)))))))
    (cond
     ;; Found exact scrolline, align there or below if rows match
     ((and prop (= scrolline (prop-match-value prop)))
      (cons (prop-match-beginning prop) scrolline))

     ;; Found actual < scrolline, return the position at the end of
     ;; actual, which we report as actual+1.
     (prop
      (cons
       (min (mistty--bol (prop-match-beginning prop) 2)
            mistty--scrolline-home-mark)
       (1+ (prop-match-value prop))))

     (t nil))))

(provide 'mistty-alacritty)

;;; mistty-alacritty.el ends here
