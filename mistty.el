;;; mistty.el --- Shell/Comint alternative based on term.el -*- lexical-binding: t -*-

;; Copyright (C) 2023-2025 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Version: 1.5.1snapshot
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, unix
;; URL: http://github.com/szermatt/mistty

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
;; This package defines a major mode for Emacs 29.1 and up that runs a
;; shell inside of a buffer, similarly to comint mode, started with
;; M-x mistty or M-x mistty-create. It is built on top of term.el.
;;
;; For details, see the documentation, at
;; https://mistty.readthedocs.io/en/latest/
;;

(require 'term)
(require 'seq)
(require 'subr-x)
(require 'pcase)
(require 'text-property-search)
(require 'fringe)
(require 'cl-lib)
(require 'imenu)
(eval-when-compile
  (require 'files-x) ; with-connection-local-variables
  (require 'minibuffer))

(require 'mistty-changeset)
(require 'mistty-accum)
(eval-when-compile
  (require 'mistty-accum-macros))
(require 'mistty-term)
(require 'mistty-util)
(require 'mistty-log)
(require 'mistty-queue)
(require 'mistty-undo)

;;; Code:

;; Customization:
(defgroup mistty nil
  "Shell/Comint alternative with a real terminal."
  :group 'shell)

(defcustom mistty-shell-command nil
  "The command MisTTY should use to start a shell.

This should either be a single string, containing the name of an
executable, or a list of string containing the name of the
executable followed by any command-line arguments.

This is used by `mistty' and `mistty-create', when not given any
command argument.

If unset, `mistty-create' and `mistty' try to use, in order:
 - `explicit-shell-file-name', without arguments
 - `shell-file-name'
 - the value of the ESHELL env variable
 - the value of the SHELL env variable.

When using TRAMP, it is possible to specify different values for
this variable for different hosts by setting mistty-shell-command
as a connection-local variable."
  :type '(repeat string)
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
differently from modifications made inside of the synced region.

This can also be turned on and off on a per-buffer basis using
`mistty-fringe-mode'."
  :type '(boolean)
  :group 'mistty
  :set (lambda (sym val)
         (set sym val)
         (dolist (buf (buffer-list))
           (with-current-buffer buf
             (when (derived-mode-p 'mistty-mode)
               (mistty-fringe-mode (if val nil -1)))))))

(defcustom mistty-skip-empty-spaces t
  "If non-nil the cursor skips over empty spaces like the shell does.

With this option set, MisTTY attempts to reproduce the jumps the
shell does when moving around a prompt that contains empty
spaces, such as the indentation spaces fish adds.

It also prevents the buffer from entering the right prompt or empty
lines at the end of the buffer."
  :type '(boolean)
  :group 'mistty)

(defcustom mistty-buffer-name '("mistty" mistty-buffer-name-user mistty-buffer-name-host)
  "Name for MisTTY buffers.

Elements of this list should be either strings or functions
returning strings. `mistty-create' evaluates each one in order to
create the full buffer name, then adds * around and possible <1>
<2> to make them unique.

By default, for non-TRAMP buffers, this the default is usually
just \"*mistty*\". If connected to another host, it could become
\"*mistty@myhost.example\". If using the sudo method, it could
become \"*mistty-root*\".

The functions on this list should look at the current
environment, primarily, `default-directory' and
`mistty-shell-command', set to the current command by
`mistty-create', to make their choice. At the point this function
is called, the buffer is not a mistty buffer yet and just has a
temporary name."
  :type '(repeat (choice string
                         (function-item mistty-buffer-name-shell)
                         (function-item mistty-buffer-name-user)
                         (function-item mistty-buffer-name-host)
                         function))
  :group 'mistty)

(defface mistty-fringe-face '((t (:foreground "#1b345a")))
  "Color of the left fringe or margin that highlights the synced region.

On the terminal, a margin is created instead of a fringe. This
face is used as well in such case.

You can turn this off completely by setting
`mistty-fringe-enabled'."
  :group 'mistty)

(defcustom mistty-buffer-maximum-size 8192
  "The maximum size in lines for MisTTY buffers.

Buffers that grow larger than the given size might be truncated.
Set to 0 to disable truncation."
  :group 'mistty
  :type 'natnum)

(defcustom mistty-move-vertically-regexps
  '("^In \\[[0-9]+\\]: " ; ipython
    )
  "Regexp that signals availability of vertical moves.

MisTTY normally avoids moving vertically, because in many shells
the up/down arrows navigate through history instead of moving the
cursor.

Vertical moves are turned on when a regexp on this list matches
the beginning of the terminal zone."
  :group 'mistty
  :type '(list regexp))

(defvar mistty--can-move-vertically nil
  "If non-nil, vertical moves are allowed.")

(defcustom mistty-forbid-edit-regexps
  '( ;; fish:
    "^search: "
    ;; bash:
    "^(reverse-i-search)"
    ;; zsh:
    ".*\n\\(failing \\)?bck-i-search:.*_ *$"
    ;; ipython
    "^I-search\\( backward\\)?: ")
  "Regexps that turn off replaying of Emacs modifications.

These regexps are meant to detect modes in which shells turn off
line editing in favor of direct interactions. The shell's reverse
history search are typically such a mode. The regexps apply to
the region starting at the beginning of the line containing the
point. They usually start with ^ to detect a specialized prompt.

While the forbid edit mode is active, the status mode line shows
\"FE:run\" instead of just \":run\".

`mistty-forbid-edit-map' is the active map in the synced region
of the buffer as long as one of these regexps matches. By
default, this means that arrow keys are sent directly to the
terminal."
  :group 'mistty
  :type '(list regexp))

(defcustom mistty-detect-foreign-overlays t
  "Treat some overlays as a sign of a long-running command.

When this option is on, Mistty tracks overlays added to its
buffer and enter long-running command mode when it sees an
overlay with a property listed in `mistty-foreign-overlay-properties'.

This allows interactive editing feature to work
such as filling-in templates, completion UIs, and CUA-style
rectangle selection.

Turn this option on if you'd like to use such a package or if
auto-detection of long-running commands doesn't work for you.

You might have to turn MisTTY on and off manually with
 `mistty-report-long-running-command'."
  :group 'mistty
  :type '(boolean))

(defcustom mistty-foreign-overlay-properties
  '(mistty-long-running-command
    cua-rectangle
    tempel--field tempel--form tempel--range
    yas--snippet)
  "Set of properties to look for in overlays.

MisTTY turns itself off while it sees an overlay with one of
these properties set and `mistty-detect-foreign-overlays' is
non-nil. This allows interactive editing features to work that
span more than one command.

This covers known interactive editing features. If the package or
feature you're using is not on this list, you might have to add
it: look for an overlay with `overlays-in', then call
`overlay-properties' and choose a custom property or face symbol
on that list.

If you find yourself extending this list, please add an issue to
https://github.com/szermatt/mistty/issues/new so it can be
integrated into the next version.

If you're writing an interactive editing feature and would like
it to be detected by MisTTY out of the boy, you might set the
overlay property \\='mistty-long-runnning-command t. This avoids
having to call `mistty-long-runnning-command'."
  :type '(list symbol)
  :group 'mistty)

(defcustom mistty-wrap-capf-functions t
  "Make `completion-at-point' functions work nice with autosuggestions.

Shell autosuggestions and `completion-at-point' don't work well
together: as the typical `completion-at-point' function looks
around the point for the object to complete, it'll see the
shell's autosuggestions and think that this is what the user
typed. So if the used typed \"com\" and the shell autosuggestion
is \"complete\", what's shown on the buffer \"com<point>plete\"
and naturally `completion-at-point' auto-completes \"complete\"
not \"com\".

To work around that, MisTTY wraps functions on
`completion-at-point-functions' so that they don't see what's
after the point, while on the terminal region. In the example
above, these wrapped functions only see \"com\" and complete
that.

This does mean that completion doesn't work exactly the same way
as it normally does: it only see what's before the point - but
the resulting behavior is saner.

Turn if off if you don't use a shell with autosuggestions and are
annoyed by this difference."
  :group 'mistty
  :type '(boolean))

(defcustom mistty-simulate-self-insert-command nil
  "Try to make auto-completion work.

This option turns on a hack that attempts to make auto-completion
work in the terminal region of MiSTTY buffers. This is known to
work at least with company and corfu at this time.

Packages that offer auto-completion often insert a function into
`post-command-hook' and run a function a while after a string of
`self-insert-command' has run.

This doesn't work in MisTTY terminal region, as keystrokes are
sent directly to the terminal and `self-insert-command' is just
not used.

MisTTY offers the hook `mistty-interactive-insert-hook' that
allows implementing auto-completion in the terminal region.

This option turns on the simulation of `self-insert-command' and
their pre and post hooks being called whenever that hook is
called. This is implemented by the function
`mistty-simulate-self-insert-command'."
  :group 'mistty
  :type '(boolean))

(defcustom mistty-at-end 'kill-buffer-and-window
  "Configures what to do when the process end.

If the value is \\='kill-buffer, kill the buffer, but only if the process
exited successfully and there was a successful interaction (the user
sent a RET or EOF) in the buffer.

If the value is \\='kill-buffer-and-window, also kill the containing
window, unless it is the single frame of the window. This is the
default.

If the value is nil or anything else, do nothing.

For more control, you can also add a function to
`mistty-after-process-start-hook'."
  :group 'mistty
  :type '(choice (const :tag "Do nothing" nil)
                 (const :tag "Kill buffer" kill-buffer)
                 (const :tag "Kill buffer and window" kill-buffer-and-window)))

(defcustom mistty-force-reuse-buffer-name t
  "Kill buffers with dead processes and reuse their names.

When this option is non-nil, the command `mistty' attempts to avoid
buffer number to rise unnecessarily by reusing buffers with the name it
wants to use whose process is dead."
  :group 'mistty
  :type 'boolean)

(defcustom mistty-allow-clearing-scrollback nil
  "Let terminal commands clear the work buffer.

In normal operations, once buffer content has left the terminal area,
they cannot be modified by the application tied to the terminal.

With this option set to non-nil, terminal applications can clear the
whole buffer content, including history kept in the scrollback area, by
issuing reset (ESC C).

In practice, this means that with this option set, the reset and clear
command or a printf \"\\ec\" will clear the whole buffer."
  :group 'mistty
  :type 'boolean)

(defcustom mistty-newline-replacement "; "
  "What to replace newlines with when extracting commands.

When extracting commands for imenu or for creating buffer names, MisTTY
needs to turn multi-line commands into one line. This string is what it
replaces newlines with.

A semicolon, the default, looks good for many shells but might look out
of place for some languages."
  :group 'mistty
  :type 'string)

(defcustom mistty-default-terminal-size nil
  "Sets the size of the terminal backing MisTTY.

By default, MisTTY sets the terminal to a size that fits the windows the
buffer is currently displayed in.

If you prefer, you can instead give the terminal a fixed dimension, by
setting this option to (cons WIDTH HEIGHT).

This might be useful if you're running program that don't check the size
of the terminal before displaying. On the other hand, this might be
confusing if you run programs that do check the size of the terminal,
such as more.

Note that this setting doesn't apply to the fullscreen mode. Terminal
size always matches window size when in fullscreen mode.

You can also change the terminal size of existing MisTTY buffers with
the commands `mistty-set-terminal-size' and
`mistty-terminal-size-tracks-window'."
  :group 'mistty
  :type '(choice (const :tag "Track Windows" nil)
                 (cons :tag "Fixed Size"
                       (natnum :tag "Width")
                       (natnum :tag "Height"))))

(defvar-keymap mistty-mode-map
  :doc "Keymap of `mistty-mode'.

This map is active whenever the current buffer is in MisTTY mode."
  "C-c C-n" #'mistty-next-output
  "C-c C-p" #'mistty-previous-output
  "C-c C-l" #'mistty-clear
  "C-c C-r" #'mistty-create-buffer-with-output
  "C-c C-o" #'mistty-select-output
  "C-c C-j" #'mistty-toggle-buffers
  "C-c C-q" #'mistty-send-key-sequence
  "C-c C-s" #'mistty-sudo
  "C-e" #'mistty-end-of-line-or-goto-cursor

  ;; mistty-send-last-key makes here some globally useful keys
  ;; available in mistty-mode buffers. More specific keys can be
  ;; input using C-q while mistty-prompt-map is active.
  "C-c C-c" #'mistty-send-last-key
  "C-c C-z" #'mistty-send-last-key
  "C-c C-\\" #'mistty-send-last-key
  "C-c C-g" #'mistty-send-last-key
  ;;   when adding a new entry above, update mistty-fullscreen-map, too.

  ;; Bind history search backward, previous history and next history
  ;; to meta keys, like comint does.
  "M-r" #'mistty-send-C-r
  "M-p" #'mistty-send-C-p
  "M-n" #'mistty-send-C-n
  "M-<up>" #'mistty-send-C-p
  "M-<down>" #'mistty-send-C-n
  "M-." #'mistty-send-key)

(defvar mistty-send-last-key-map '(keymap (t . mistty-send-last-key))
  "Keymap that sends everything to the terminal using `mistty-send-last-key'.")

(defvar-keymap mistty-prompt-map
  :parent mistty-mode-map
  :doc "Keymap active on the part of `mistty-mode' synced with the terminal.

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
terminal."

  "RET" #'mistty-send-command
  "TAB" #'mistty-tab-command
  "DEL" #'mistty-backward-delete-char
  "C-d" #'mistty-delete-char
  "C-a" #'mistty-beginning-of-line

  "S-<return>" #'mistty-newline

  ;; While on the prompt, "quoted-char" turns into "send the next
  ;; key directly to the terminal".
  "C-q" mistty-send-last-key-map

  ;; Don't bother capturing single key-stroke modifications and
  ;; replaying them; just send them to the terminal. This works even
  ;; when the terminal doesn't accept editing.
  "<remap> <self-insert-command>" #'mistty-self-insert)

(defvar mistty-send-last-key-map '(keymap (t . mistty-send-last-key))
  "Keymap that forwards everything to`mistty-send-last-key'.")

(defvar-keymap mistty-forbid-edit-map
  :parent mistty-prompt-map
  :doc "Keymap active when line editing is off.

This map is active on the part of `mistty-mode' synced with the
terminal when of of `mistty-forbid-edit-regexps' has been
detected and replay is limited to insert and delete.

In practice, `mistty-forbid-edit-regexps' is used to detect shell
search mode, and in such a mode, it's convenient if arrow keys
are sent directly to the terminal."
  "<up>" #'mistty-send-last-key
  "<down>" #'mistty-send-last-key
  "<left>" #'mistty-send-last-key
  "<right>" #'mistty-send-last-key)

(defvar-keymap mistty-fullscreen-map
  :parent term-raw-map
  :doc "Keymap active while in fullscreen mode.

While in fullscreen mode, the buffer is a `term-mode' with its
own keymaps (`term-mod-map' and `term-raw-map')

This map is applied in addition to these as a way of making key
mapping somewhat consistent between fullscreen and normal mode."

    "C-q" mistty-send-last-key-map
    "C-c C-q" #'mistty-send-key-sequence

    ;; Mirror keybindings from mistty-mode-map, for consistency.
    "C-c C-c" #'mistty-send-last-key
    "C-c C-z" #'mistty-send-last-key
    "C-c C-\\" #'mistty-send-last-key
    "C-c C-g" #'mistty-send-last-key

    ;; Overwrite mapping from term-raw-map so they can be remapped
    ;; with mistty-term-key-map, if necessary.
    "<up>" #'mistty-send-key
    "<down>" #'mistty-send-key
    "<right>" #'mistty-send-key
    "<left>" #'mistty-send-key
    "C-<up>" #'mistty-send-key
    "C-<down>" #'mistty-send-key
    "C-<right>" #'mistty-send-key
    "C-<left>" #'mistty-send-key
    "<delete>" #'mistty-send-key
    "<deletechar>" #'mistty-send-key
    "<backspace>" #'mistty-send-key
    "<home>" #'mistty-send-key
    "<end>" #'mistty-send-key
    "<insert>" #'mistty-send-key
    "<prior>" #'mistty-send-key
    "<next>" #'mistty-send-key

    ;; This only applies if term-bind-function-keys is non-nil.
    "<remap> <term-send-function-key>" #'mistty-send-key

    ;; Disable the "Terminal" menu; nothing that it contains should be
    ;; used on Term buffers used by MisTTY.
    "<menu-bar> <terminal>" nil

    ;; switching the term buffer to line mode would cause issues.
    "<remap> <term-line-mode>" #'mistty-toggle-buffers)

;; Variables:

(defvar mistty-interactive-insert-hook
  (list #'mistty-simulate-self-insert-command)
  "Report interactively inserted text.

This hook is called whenever text that was inserted by
`mistty-self-insert' has been echoed back. It might report a
single character or a large number of characters, depending on
the speed of the terminal and how fast the user types.

This hook is called from the work buffer with the point right
after the inserted text.

This hook is not guaranteed to be called for all interactively
inserted text. If the user is too fast or the shell too slow and
a command gets run that's not `mistty-self-insert' before it was
reported, it just never will be called.

This is meant to be used to allow interactive completion or
suggestions to work in MisTTY, with a little work, as a
replacement of the usual approach, which is to track
`self-insert-command' in post-command hooks. Such an approach
doesn't work in the terminal region as typed characters usually
get echoed back to buffer outside of any command.

By default, this calls the function
`mistty-simulate-self-insert-command', which does nothing unless
the option variable `mistty-simulate-self-insert-command' is
non-nil.")

(defvar mistty-after-process-start-hook nil
  "Report that the MistTTY process started.

This hook is called from `mistty-work-buffer` just after the
shell process has been started and attached to the MisTTY window.
The process is available in `mistty-proc` and the terminal buffer
in `mistty-term-buffer`. Note that the process might not have
output anything yet; the buffer is likely empty.")

(defvar mistty-after-process-end-hook (list #'mistty--at-end)
  "Report that the MisTTY process ended.

This hook is called with PROC as an argument from
`mistty-work-buffer` just after the shell process ended. PROC is
guaranteed to be a dead process. Its status can be checked with
`process-status`.

At the point this hook is called `mistty-proc` is unset and
`mistty-term-buffer` has already been killed.

Note that this hook might never be called, in particular when the
MisTTY buffer is killed while the process is still running.

This hook can be used to kill the buffer after the shell ended
successfully. See `mistty-kill-buffer` and
`mistty-kill-buffer-and-window`.")

(defvar mistty-entered-fullscreen-hook nil
  "Report that MisTTY just entered fullscreen mode.

At the point this hook is called, `mistty-fullscreen` is non-nil.")

(defvar mistty-left-fullscreen-hook nil
  "Report that MisTTY just left fullscreen mode.

At the point this hook is called, `mistty-fullscreen` is nil.")

(defvar-local mistty-work-buffer nil
  "The main `mistty-mode' buffer.

This buffer keeps a modifiable history of all commands at the top
and a view of the terminal modified by the current command at the
bottom.

In normal mode, this is the buffer that's displayed to the user.
In fullscreen mode, this buffer is kept as historical scrollback
buffer that can be independently switched to with
`mistty-toggle-buffers'.

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
  "Controls whether the point should be moved to the cursor.

If t, this variable tells `mistty--refresh' that it should move
the point to the cursor next time it copies the state of the
terminal to the work buffer.

If \\='off, this variable tells `mistty--refresh' not to move the
point to the cursor, even if it would normally do it.

If nil, this variable lets `mistty-refresh' do what it think is
best.

This variable is available in the work buffer.")

(defvar-local mistty--queue nil
  "A queue of data to send to the process; a mistty--queue struct.

See mistty-queue.el.")

(defvar-local mistty-sync-marker nil
  "A marker that links `mistty-term-buffer' to `mistty-work-buffer'.

The region of the terminal that's copied to the work buffer by
`mistty--sync-buffer' starts at `mistty-sync-marker' and ends
at `(point-max)' on both buffers. The two markers must always be
kept in sync.

This variable is available in both the work buffer and the term
buffer.")

(defvar-local mistty--sync-marker-scrolline 0
  "Define the scrolline assigned to `mistty-sync-marker'.

This is updated at the same time as the marker, on both buffers.")

(defvar-local mistty--active-prompt nil
  "A `mistty--prompt' struct of the active prompt.")

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

(defvar-local mistty--old-point nil
  "The position of the point captured in `pre-command-hook'.

It is used in the `post-command-hook'.

This variable is available in the work buffer.")

(defvar-local mistty--end-prompt nil
  "End the prompt after the next refresh.

When this variable is non-nil, it contains a position in the work buffer
that's on the current prompt. The line after that is going to be a
process output or a new prompt.")

(defvar-local mistty--cursor-after-last-refresh nil
  "A marker on the cursor at the end of `mistty--refresh'.

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

(defvar-local mistty--truncate-timer nil
  "An idle timer that'll truncate the buffer.

Truncation is configured by `mistty-buffer-maximum-size'.")

(defvar-local mistty--forbid-edit nil
  "Non-nil when normal editing is not available.

This is controlled by `mistty-forbid-edit-regexp'.

When this is set, MisTTY assumes that typing and deletion work,
but moving the cursor doesn't. This allows replaying some simple
editing commands, such as a `yank' or a `backward-kill-word'.")

(defvar-local mistty--inhibit nil
  "When non-nil, inhibit communication with the terminal.

Normally, MisTTY allows normal Emacs operations during a command,
then replays any changes in the post-command hook. With this
variable set, MisTTY allows normal Emacs operations during a
chain of commands. Replay, refresh and even `mistty-prompt-map'
are suppressed.

This is used to allow longer-running, interactive commands such
as filling templates to run without interference from the shell
until the end.

This is a list of symbols, that represent the reason why normal
operations are inhibited. This allows dealing with more than one
running command at a time.")

(defvar mistty--inhibit-fake-nl-cleanup nil
  "Inhibit deletion of fake newline when moving the sync marker.

Normally, fake newlines are removed by mistty--set-sync-mark,
when the terminal zone is moved, so they don't confuse cursor
movements.

Setting this variable to non-nil inhibit this behavior, which
might be useful in tests, to avoid tests being confused when
old positions are invalidated")

(defvar-local mistty--ignored-overlays nil
  "Foreign overlays that should just be ignored.

Normally, overlays are seen as a sign that there is a
long-running command and MisTTY lets it run until without
replaying the change. The overlays on this list must not be taken
as a sign that there is a long-running command.")

(defvar-local mistty--self-insert-line nil
  "Capture of the last string of self-inserted characters.

The first element is the distance from bol to the current point.
The second element is a list to which characters are pushed (this
means that most recent characters appear first.)

Whenever a character is inserted by `mistty-self-insert', it is added
to this. When a character or a series of such self-inserted character
is echoed back, call `mistty-interactive-insert-hook'.")

(defvar mistty-chain-start nil
  "The buffer that was current at the start of the `mistty' chain.

The `mistty' command is meant to be called multiple times, to
iterate through multiple buffers and possibly eventually creating
a new one. In such a scenario, it's useful to keep track of the
buffer `mistty' was called for the first time, so inherit the
`default-directory' of that buffer and possibly use TRAMP to
connect to the same host.")

(defvar-local mistty--interacted nil
  "Non-nil if there was a successful interaction in this session.

This is used by `mistty-kill-buffer' and `mistty-kill-buffer-and-window'
to decide whether it's OK to kill the buffer.")

(defvar-local mistty--terminal-size nil
  "If non-nil, a (cons WIDTH HEIGHT) that specify the terminal size.

When non-nil, tracking window size change is disabled outside of
fullscreen mode.")

(eval-when-compile
  ;; defined in term.el
  (defvar term-home-marker))

(defconst mistty-min-terminal-width 8
  "Minimum terminal width.

Width smaller than that will not be accepted as terminal size and will
be ignored if coming from window size.")

(defconst mistty-min-terminal-height 4
  "Minimum terminal height.

Height smaller than that will not be accepted as terminal size and will
be ignored if coming from window size.")

(define-derived-mode mistty-mode fundamental-mode "misTTY" "Line-based TTY."
  :interactive nil
  (setq buffer-read-only nil)
  (setq mistty-work-buffer (current-buffer))

  ;; scroll down only when needed. This typically keeps the point at
  ;; the end of the window. This seems to be more in-line with what
  ;; commands such as more expect than the default Emacs behavior.
  (setq-local scroll-conservatively 1024)
  (setq-local window-point-insertion-type t)
  (setq-local scroll-margin 0)

  ;; Mistty hides the fake newlines term added and relies on large
  ;; lines being wrapped for display and proper resizing.
  (setq-local truncate-lines nil)

  (add-hook 'pre-redisplay-functions #'mistty--cursor-skip nil t)
  (add-hook 'completion-in-region-mode-hook #'mistty--detect-completion-in-region nil t)

  (setq mistty-sync-marker (point-max-marker))
  (setq mistty--sync-ov (make-overlay mistty-sync-marker (point-max) nil nil 'rear-advance))
  (setq mistty--ignored-overlays (list mistty--sync-ov))
  (setq-local beginning-of-defun-function #'mistty-beginning-of-defun)
  (setq-local end-of-defun-function #'mistty-end-of-defun)
  (setq-local imenu-create-index-function #'mistty-imenu-create-index)
  (setq-local imenu-sort-function nil) ;; keep imenu entries in order

  (overlay-put mistty--sync-ov 'local-map (mistty--active-prompt-map))

  (when mistty-fringe-enabled
    (mistty-fringe-mode 'on)))
(put 'mistty-mode 'mode-class 'special)

(define-minor-mode mistty-fringe-mode
  "Highlight the synced region with a left fringe or margin.

This makes the synced region visible, which is useful as
modifications made inside of the synced region are treated
differently from modifications made inside of the synced region."
  :init-value nil
  (unless (derived-mode-p 'mistty-mode)
    (error "Mistty-fringe-mode only meaningful on MisTTY buffers"))
  (if mistty-fringe-mode
      ;; turn on
      (if (window-system)
          ;; implemented as a left fringe
          (progn
            (unless (fringe-bitmap-p 'mistty-bar)
              (define-fringe-bitmap
                'mistty-bar (make-vector 40 7) nil 3 'center))
            (overlay-put
             mistty--sync-ov
             'line-prefix
             (propertize " " 'display
                         '(left-fringe mistty-bar mistty-fringe-face))))
        ;; implemented as a left margin
        (progn
          (setq left-margin-width 1)
          (overlay-put
           mistty--sync-ov
           'line-prefix
           (propertize " " 'display
                       `((margin left-margin)
                         ,(propertize "┃" 'face 'mistty-fringe-face))))))
    ;; turn off
    (unless (window-system)
      (setq left-margin-width (default-value 'left-margin-width)))
    (overlay-put mistty--sync-ov 'line-prefix nil)))

(defsubst mistty--require-work-buffer ()
  "Asserts that the current buffer is the work buffer."
  (unless (eq mistty-work-buffer (current-buffer))
    (error "Work buffer required, got %s" (buffer-name))))

(defsubst mistty--require-term-buffer ()
  "Asserts that the current buffer is the term buffer."
  (unless (eq mistty-term-buffer (current-buffer)) (error "Term buffer required")))

(defsubst mistty--require-proc ()
  "Asserts that the current buffer has a live process."
  (unless (process-live-p mistty-proc) (error "No running process")))

(cl-defun mistty-exec (program &key width height)
  "Execute PROGRAM in the current buffer.

Buffer must be a `mistty-mode' buffer. If it is already running a
program, it is replaced with the new one.

PROGRAM can be either a string or a list. If it is a string, it
should be the name of an executable to run, without arguments. If
it is a list, it should be a list of executable and its
arguments.

WIDTH and HEIGHT, if specified, are used as the initial dimensions of
the terminal. When not specified and `mistty-default-terminal-size' is
nil, the dimension of the terminal is taken from the window showing the
current buffer or, if the buffer isn't displayed yet, the selected
window."
  (unless (derived-mode-p 'mistty-mode)
    (error "Not a mistty-mode buffer"))

  (mistty--kill-term-buffer)
  (erase-buffer)

  (let ((command (if (consp program) (car program) program))
        (args (if (consp program) (cdr program) nil)))
    (when (and mistty-default-terminal-size
               (null width)
               (null height))
      (setq width (car mistty-default-terminal-size))
      (setq height (cdr mistty-default-terminal-size)))

    (if (or width height)
        (progn
          (mistty--check-terminal-size width height)
          (setq-local mistty--terminal-size (cons width height)))

      (setq-local mistty--terminal-size nil)
      (let ((win (or (get-buffer-window (current-buffer))
                     (selected-window))))
        (setq width (window-max-chars-per-line win))
        (setq height (floor (with-selected-window win
                                   (window-screen-lines))))))
    (mistty--attach
     (mistty--create-term
      (concat " mistty tty " (buffer-name)) command args
      ;; local-map
      mistty-fullscreen-map
      width height)))
  (mistty--wrap-capf-functions)
  (mistty--update-mode-lines)
  (run-hooks 'mistty-after-process-start-hook))

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
    (setq mistty--prompt-cell (buffer-local-value 'mistty--prompt-cell term-buffer))
    (setq mistty--queue (mistty--make-queue proc))
    (mistty--needs-refresh)

    (with-current-buffer term-buffer
      (setq mistty-proc proc)
      (setq mistty-work-buffer work-buffer)
      (setq mistty-term-buffer term-buffer)
      (unless mistty-sync-marker
        (setq mistty-sync-marker (copy-marker term-home-marker))))

    (when proc
      (let ((accum (process-filter proc)))
        (mistty--accum-reset accum)
        (mistty--add-prompt-detection accum work-buffer)
        (mistty--add-osc-detection accum)
        (mistty--add-skip-unsupported accum)

        ;; Sync work and term buffers
        (mistty--accum-add-arround-process-filter
         accum
         (lambda (func)
           (funcall func)
           (mistty--with-live-buffer work-buffer
             (mistty--needs-refresh))))
        (mistty--accum-add-arround-process-filter
         accum
         (mistty--detect-write-before-sync-mark term-buffer))
        (mistty--accum-add-post-processor
         accum #'mistty--postprocessor)

        ;; Handle show/hide cursor
        (mistty--accum-add-processor
         accum
         '(seq CSI "?25h")
         (lambda (_ _)
           (mistty--with-live-buffer work-buffer
             (mistty--show-cursor))))
        (mistty--accum-add-processor
         accum
         '(seq CSI "?25l")
         (lambda (_ _)
           (mistty--with-live-buffer work-buffer
             (mistty--hide-cursor))))

        ;; Switch to fullscreen mode
        (mistty--accum-add-processor
         accum
         '(seq CSI (or "47" "?47" "?1047" "?1049") ?h)
         (lambda (ctx str)
           (mistty--accum-ctx-flush ctx)
           (mistty--enter-fullscreen proc)
           (mistty--accum-ctx-push-down ctx str)))

        ;; Handle clear and reset
        (mistty--accum-add-processor
         accum
         '(or (seq ESC ?c)
              (seq CSI ?H CSI (? ?0) ?J)
              (seq CSI ?2 ?J))
         (lambda (ctx str)
           (mistty--accum-ctx-flush ctx)
           (mistty--reset)
           (mistty--accum-ctx-push-down ctx str))))
      (set-process-sentinel proc #'mistty--process-sentinel))

    (add-hook 'kill-buffer-hook #'mistty--kill-term-buffer nil t)
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)
    (add-hook 'pre-command-hook #'mistty--pre-command nil t)
    (add-hook 'post-command-hook #'mistty--post-command nil t)

    (if-let ((size mistty--terminal-size))
        (mistty--set-process-window-size (car size) (cdr size))
      (mistty--set-process-window-size-from-windows)
      (add-hook 'window-size-change-functions #'mistty--window-size-change nil t))))

(defun mistty--create-or-reuse-marker (m initial-pos)
  "Create the marker M set to INITIAL-POS or move it to that position.

Returns M or a new marker."
  (if (not (markerp m))
      (copy-marker initial-pos)
    (when (= 1 (marker-position m))
      (move-marker m initial-pos))
    m))

(defun mistty--detach ()
  "Detach the current `mistty-mode' buffer from its process."
  (mistty--require-work-buffer)

  (remove-hook 'kill-buffer-hook #'mistty--kill-term-buffer t)
  (remove-hook 'after-change-functions #'mistty--after-change-on-work t)
  (remove-hook 'pre-command-hook #'mistty--pre-command t)
  (remove-hook 'post-command-hook #'mistty--post-command t)
  (remove-hook 'window-size-change-functions #'mistty--window-size-change t)

  (when mistty--queue
    (mistty--cancel-queue mistty--queue)
    (setq mistty--queue nil))
  (when mistty-proc
    (let ((accum (process-filter mistty-proc)))
      (mistty--accum-reset accum))
    (set-process-sentinel mistty-proc #'term-sentinel)
    (setq mistty-proc nil)))

(defun mistty--kill-term-buffer ()
  "Kill-buffer-hook handler for `mistty-term-buffer'."
  (let ((term-buffer mistty-term-buffer))
    (when (buffer-live-p mistty-work-buffer) ;; might be nil
      (mistty--detach))
    (mistty--update-mode-lines)
    (when (buffer-live-p term-buffer)
      (when-let ((proc (get-buffer-process term-buffer)))
        (when (process-live-p proc)
          (delete-process proc)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer term-buffer)))))

(defun mistty-buffer-p (buffer)
  "Return the BUFFER if the buffer is a MisTTY buffer.

The buffer might be a `mistty-mode' buffer in non-fullscreen mode or a
`term-mode' buffer in fullscreen mode."
  (and
   (buffer-live-p buffer)
   (pcase (buffer-local-value 'major-mode buffer)
     ('mistty-mode (not (buffer-local-value 'mistty-fullscreen buffer)))
     ('term-mode (buffer-local-value 'mistty-fullscreen buffer)))

   ;; returns
   buffer))

(defun mistty-live-buffer-p (buffer)
  "Return the BUFFER if the buffer is a MisTTY buffer.

The process attached to the buffer must be live.

When in fullscreen mode, the main MisTTY buffer is actually a
`term-mode' buffer, not the scrollback buffer."
  (and
   (mistty-buffer-p buffer)
   (buffer-local-value 'mistty-proc buffer)
   (process-live-p (buffer-local-value 'mistty-proc buffer))

   ;; return
   buffer))

(defun mistty-list-live-buffers (&optional accept-buffer)
  "List of live MisTTY buffers, sorted.

ACCEPT-BUFFER, if specified, must be a function that takes in a
buffer and return non-nil if that buffer should be taken into
account by this command."
  (let ((cond #'mistty-live-buffer-p))
    (when accept-buffer
      (setq cond (lambda (buf) (when (and (mistty-live-buffer-p buf)
                                          (funcall accept-buffer buf))
                                 buf))))
    (sort (delq nil (mapcar cond (buffer-list)))
          (lambda (a b)
            (string< (buffer-name a) (buffer-name b))))))

;;;###autoload
(defun mistty (&optional other-window accept-buffer)
  "Go to the next MisTTY buffer, or create a new one.

The first time this command is called, it creates a new MisTTY
buffer. Afterwards, this command goes to a MisTTY buffer. If
already on a MisTTY buffer, go to the next one or create another
one.

If called with an argument, create a new buffer unconditionally,
so you can call `mistty' multiple times to visit existing MisTTY
buffers, and then, if it turns out to be necessary, call it a
final time with an argument to create a new one.

When creating a new buffer, the `default-directory' of that
buffer is taken from the buffer from which the chain of calls
`mistty' was started.

If OTHER-WINDOW is nil, execute the default action configured by
`display-comint-buffer-action'. If OTHER-WINDOW is a function, it is
passed to `pop-to-buffer` to be used as a `display-buffer' action.
Otherwise, display the buffer in another window.

    You might prefer configuring `display-buffer-alist' for
    comint category buffers to get the exact behavior you want instead
    of passing OTHER-WINDOW.

Passing ACCEPT-BUFFER is deprecated. Call `mistty-cycle-or-create'
instead. See that function for details on that argument.

    This function is for interactive use only. When building similar
    commands, call `mistty-cycle-or-create' instead."
  (interactive)
  (when (not (eq this-command last-command))
    (setq mistty-chain-start (current-buffer)))

  (mistty-cycle-or-create
   accept-buffer
   (lambda (other-window)
     (if mistty-chain-start
       (with-current-buffer mistty-chain-start
         (mistty-create nil other-window))
       (mistty-create nil other-window)))
   other-window))

(defun mistty-cycle-or-create (accept-buffer create-buffer other-window)
  "Cycle through existing buffers, creating one if necessary.

This function implements the behavior of `mistty' in a configurable way.

The first time this command is called, it creates a new MisTTY
buffer. Afterwards, this command goes to a MisTTY buffer. If
already on a MisTTY buffer, go to the next one or create another
one.

ACCEPT-BUFFER, if specified, must be a function that takes in a live
MisTTY buffer and return non-nil if that buffer should be taken into
account as existing buffer by this command. This can be used to manage
distinct group of buffers, such as buffers belonging to different
projects.

CREATE-BUFFER must be a function that creates a new buffer. It takes a
single argument, OTHER-WINDOW. It must return the newly created buffer.

If OTHER-WINDOW is nil, execute the default action configured by
`display-comint-buffer-action' to pop to the existing or newly-created
buffer. If OTHER-WINDOW is a function, it is passed to `pop-to-buffer`
to be used as a `display-buffer' action. Otherwise, display the buffer
in another window.

    You might prefer configuring `display-buffer-alist' for
    comint category buffers to get the exact behavior you want
    instead of passing OTHER-WINDOW."
  (let ((existing (mistty-list-live-buffers accept-buffer)))
    (if (or current-prefix-arg         ; command prefix was given
            (null existing)            ; there are no mistty buffers
            (and (null (cdr existing)) ; the current buffer is the only mistty buffer
                 (eq (current-buffer) (car existing))))
        (funcall create-buffer other-window)
      (mistty--goto-next existing other-window))))

;;;###autoload
(defun mistty-other-window ()
  "Go to the next MisTTY buffer in another window.

    You might prefer configuring `display-buffer-alist' for
    comint category buffers and calling `mistty' directly to get
    the exact behavior you want instead of using
    `mistty-other-window'.

    This function is for interactive use only. When building similar
    commands, consider calling `mistty-cycle-or-create' instead."
  (interactive)
  (mistty 'other-window))

(defun mistty--goto-next (existing &optional other-window)
  "Go to the next buffer in EXISTING, skipping the current one.

If OTHER-WINDOW is non-nil, put the buffer into another window."
  (let ((existing-tail (or (cdr (member (current-buffer) existing))
                           existing)))
    (if existing-tail
        (mistty--pop-to-buffer (car existing-tail) other-window)
      (error "No next mistty buffer"))))

(defun mistty--pop-to-buffer (buf other-window)
  "Display BUF.

If OTHER-WINDOW is nil, execute the default action configured by
`display-comint-buffer-action'. If OTHER-WINDOW is a function, it
is passed to `pop-to-buffer` to be used as a `display-buffer'
action. Otherwise, display the buffer in another window."
  (pop-to-buffer
   buf
   (cond
    ((functionp other-window) other-window)
    (other-window #'display-buffer-pop-up-window)
    (t (bound-and-true-p display-comint-buffer-action)))))

;;;###autoload
(defun mistty-create (&optional command other-window)
  "Create a new MisTTY buffer, running a shell.

The shell that is run can be configured by setting
`mistty-shell-command', `explicit-shell-file-name',
`shell-file-name' or come implicitly from the ESHELL or SHELL
environment variables.

Set COMMAND to specify instead the command to run for the current
call. COMMAND can be either a string or a list. If it is a
string, it should be the name of an executable to run, without
arguments. If it is a string, it should be a list of executable
and its arguments.

If this command is called with no prefix arg, it is executed in
the `default-directory' of the current buffer. If that directory
is a remote file, and if the TRAMP method supports it, the
command is executed on the remote host. This also works for
same-host methods, such as sudo. See Info node `(mistty)Remote
Shells with TRAMP' for details.

If this command is called with a prefix arg, it asks for the
value of `default-directory'. This is useful if you want start a
shell to a remote host without opening a file first.

If OTHER-WINDOW is nil, execute the default action configured by
`display-comint-buffer-action'. If OTHER-WINDOW is a function, it
is passed to `pop-to-buffer` to be used as a `display-buffer'
action. Otherwise, display the buffer in another window.

Upon success, the function returns the newly-created buffer."
  (interactive)
  (let* ((default-directory
          (or (and (memq this-command '(mistty-create mistty-create-other-window))
                   current-prefix-arg
                   (mistty--read-default-directory))
              default-directory))

         (buf (generate-new-buffer " *mistty-new"))
         (command (or command
                      (with-current-buffer buf
                        (with-connection-local-variables
                         (or
                          mistty-shell-command
                          explicit-shell-file-name
                          shell-file-name
                          (getenv "ESHELL")
                          (getenv "SHELL")))))))
    (with-current-buffer buf
      (setq-local mistty-shell-command command)
      (rename-buffer (mistty--generate-new-buffer-name (mistty-new-buffer-name)))
      (mistty-mode))
    ;; Note that it's important to attach the buffer to a window
    ;; before executing the command, so that the shell known the size
    ;; of the terminal from the very beginning.
    (mistty--pop-to-buffer buf other-window)
    (with-current-buffer buf (mistty-exec command))
    buf))

(defun mistty--generate-new-buffer-name (bufname)
  "Generate a unique buffer name based on BUFNAME.

If `mistty-force-reuse-buffer-name' is non-nil, this function might kill
dead MisTTY buffers to reuse their name."
  (if (or (not mistty-force-reuse-buffer-name)
          (string-prefix-p " " bufname))
      (generate-new-buffer-name bufname)

    (let ((index 0)
          (full-bufname bufname))
      (while
          (let (buf)
            (cl-incf index)
            (when (> index 1)
              (setq full-bufname (format "%s<%d>" bufname index)))

            (when (and (setq buf (get-buffer full-bufname))
                       (mistty-buffer-p buf)
                       (not (mistty-live-buffer-p buf)))
              (let ((kill-buffer-query-functions nil))
                (kill-buffer buf))
              (setq buf nil))

            buf))

      full-bufname)))

;;;###autoload
(defun mistty-create-other-window (&optional command)
  "Create a new MisTTY buffer, running a shell, in another window.

If this command is called with no prefix arg, it is executed in
the `default-directory' of the current buffer. If that directory
is a remote file, and if the TRAMP method supports it, the
command is executed on the remote host. This also works for
same-host methods, such as sudo. See Info node `(mistty)Remote
Shells with TRAMP' for details.

If this command is called with a prefix arg, it asks for the
value of `default-directory'. This is useful if you want start a
shell to a remote host without opening a file first.

COMMAND, if specified, is the command to execute instead of the
shell.

See the documentation of `mistty-create' for details."
  (interactive)
  (mistty-create command 'other-window))

(defun mistty--read-default-directory ()
  "Get the default directory."
  (read-directory-name "Default directory: " default-directory default-directory t nil))

(defun mistty--process-sentinel (proc msg)
  "Process sentinel for MisTTY shell processes.

PROC is the process, which might not be live anymore, and MSG is
a special string describing the new process state."
  (mistty--update-mode-lines proc)

  (let ((work-buffer (process-get proc 'mistty-work-buffer))
        (term-buffer (process-buffer proc)))
    (mistty--with-live-buffer work-buffer
      (when (memq (process-status proc) '(signal exit))
        (while (accept-process-output proc 0 0 t))))

    (cond
     ((and (buffer-live-p term-buffer)
           (buffer-live-p work-buffer))
      (term-sentinel proc msg)
      (mistty--with-live-buffer work-buffer
        (mistty--needs-refresh) ;; term-buffer was modified by term-sentinel
        (save-restriction
          (widen)
          (mistty--refresh)
          (when (and (processp mistty-proc)
                     (>= (point) (mistty-cursor)))
            (goto-char (point-max)))
          (mistty--detach)))
      (kill-buffer term-buffer))

     ((buffer-live-p work-buffer)
      (with-current-buffer work-buffer
        (mistty--detach)
        (insert-before-markers "\n\nTerminal killed.\n")))

     ((buffer-live-p term-buffer)
      (kill-buffer term-buffer)))
    (mistty--run-after-process-end-hooks work-buffer proc)))

(defun mistty--run-after-process-end-hooks (buf proc)
  "Run hooks on `mistty-after-process-end-hook'.

The hooks are called from inside BUF after PROC has died. Since these
hooks tend to kill their buffer, make sure that running stops once the
buffer is killed."
  (run-hook-wrapped
   'mistty-after-process-end-hook
   (lambda (fun)
     (mistty--with-live-buffer buf
       (funcall fun proc)

       ;; continue
       t))))

(defun mistty--fs-process-sentinel (proc msg)
  "Process sentinel for MisTTY shell processes in fullscreen mode.

PROC is the process, which might not be live anymore, and MSG is a
special string describing the new process state."
  (mistty--update-mode-lines proc)

  (let ((process-dead (memq (process-status proc) '(signal exit)))
        (term-buffer (process-get proc 'mistty-term-buffer))
        (work-buffer (process-get proc 'mistty-work-buffer)))
    (cond
     ((and process-dead (buffer-live-p term-buffer) (buffer-live-p work-buffer))
      (mistty--leave-fullscreen proc)
      (mistty--process-sentinel proc msg))
     ((and process-dead (not (buffer-live-p term-buffer)) (buffer-live-p work-buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (process-get proc 'mistty-work-buffer)))
      (term-sentinel proc msg)
      (mistty--run-after-process-end-hooks work-buffer proc))
     (t (term-sentinel proc msg)))))

(defun mistty--postprocessor ()
  "React to modifications on the term buffer.

This is meant to be added to the accumulator as post-processor."
  (mistty--with-live-buffer mistty-work-buffer
    (mistty--cancel-timeout mistty--queue)
    (mistty--refresh)
    (mistty--maybe-truncate-when-idle)
    (mistty--dequeue mistty--queue 'intermediate)
    (mistty--dequeue-with-timer mistty--queue 'stable)))

(defun mistty--reset ()
  "Reset the link between work and term buffer.

This should be called just before reseting the terminal."
  (let ((reset-scrolline nil)
        (home-pos nil))
    (mistty--with-live-buffer mistty-work-buffer
      (mistty--cancel-queue mistty--queue)
      (while-let ((cs (car mistty--changesets)))
        (mistty--release-changeset cs))
      (setq mistty--inhibit-refresh nil)
      (setq mistty--need-refresh t)
      (mistty--refresh)
      (mistty--prepare-end-for-reset)
      (setq reset-scrolline (mistty--scrolline (point-max)))
      (setq home-pos (point)))
    (mistty--with-live-buffer mistty-term-buffer
      (mistty--term-reset-scrolline reset-scrolline)
      (setq mistty-bracketed-paste nil))
    (mistty--with-live-buffer mistty-work-buffer
      (setq mistty-bracketed-paste nil)
      (mistty--set-sync-mark (point-max) reset-scrolline)
      (mistty-log "RESET terminal at scrolline %s" reset-scrolline))

    (if mistty-allow-clearing-scrollback
        (mistty--clear-scrollback)
      ;; Scroll the main window so the region that was cleared is
      ;; not visible anymore. This way, it looks like the buffer
      ;; was cleared even though history is kept.
      (mistty--with-live-buffer mistty-work-buffer
        (when-let (win (get-buffer-window mistty-work-buffer))
          (with-selected-window win
            (set-window-start win (mistty--bol home-pos) 'noforce)))))))

(defun mistty--prepare-end-for-reset ()
  "Prepare work buffer to put terminal region at (point-max).

This function might erase whitespaces at the end of the buffer."
  (mistty--require-work-buffer)
  (save-excursion
    (goto-char (mistty--last-non-ws))
    (let ((inhibit-modification-hooks t))
      (insert "\n")
      (delete-region (point) (point-max)))))

(defun mistty--clear-scrollback ()
  "Clear the work buffer above `mistty-sync-marker'."
  (mistty--with-live-buffer mistty-work-buffer
    (mistty-log "CLEAR SCROLLBACK")
    (when (< (point) mistty-sync-marker)
      (goto-char mistty-sync-marker)
      (setq mistty-goto-cursor-next-time 1))
    (when (> mistty-sync-marker 1)
      (let ((inhibit-modification-hooks t)
            (inhibit-read-only t))
        (delete-region 1 mistty-sync-marker)))))

(defun mistty--maybe-scroll-window-down ()
  "Make sure that newly inserted text is visible.

If there's something below the point in a prompt, the window down so
it's visible. Emacs won't do it on its own, since all it cares about is
the point being visible."
  (mistty--require-work-buffer)
  (when (and mistty-proc
             (equal (point) (mistty-cursor))
             (mistty-on-prompt-p (point)))
    (let* ((pos (point))
           (end (mistty--last-non-ws))
           (lines-after-point (count-lines (point) end)))
      (when (> lines-after-point 1)
        (dolist (win (get-buffer-window-list))
          (when (and (equal pos (window-point win))
                     (not (pos-visible-in-window-p end win)))
            (recenter (- lines-after-point))))))))

(defun mistty--detect-write-before-sync-mark (term-buffer)
  "Return a function for realigning work and term buffers as necessary.

TERM-BUFFER should be the terminal buffer."
  (lambda (func)
    (let ((old-sync-position (mistty--with-live-buffer term-buffer
                               (marker-position mistty-sync-marker))))
      ;; Reminder: call func with no buffer set, to avoid strange
      ;; breakages when the term buffer is killed.
      (funcall func)
      (mistty--with-live-buffer term-buffer
        (when (/= mistty-sync-marker old-sync-position)
          (mistty-log "Detected terminal change above sync mark, at scrolline %s"
                      mistty--sync-marker-scrolline)
          (mistty--realign-buffers))))))

(defun mistty-goto-cursor ()
  "Move the point to the terminal's cursor."
  (interactive)
  (mistty--require-proc)
  (let ((cursor (mistty--safe-pos (mistty-cursor))))
    (when (/= cursor (point))
      (goto-char cursor)
      (dolist (win (get-buffer-window-list mistty-work-buffer nil t))
        (when (= cursor (window-point win))
          (set-window-parameter win 'mistty--cursor-skip-state nil))))))

(defun mistty-cursor ()
  "Return the position of the terminal cursor in the MisTTY buffer.

Note that the position might not exist in `mistty-work-buffer',
not yet, if it the work buffer is out of sync with
`mistty-term-buffer'."
  ;; Not using mistty--require-proc as a non-live process is
  ;; acceptable here.
  (unless mistty-proc
    (error "No process"))
  (mistty--from-pos-of (process-mark mistty-proc) mistty-term-buffer))

(defun mistty--cursor-scrolline ()
  "Return the scrolline position of the cursor."
  (with-current-buffer mistty-term-buffer
    (mistty--term-scrolline)))

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

(defun mistty--scrolline (pos)
  "Return the scrolline that correspond to POS.

This works in both the sync and term buffers.

To get the position of the cursor `mistty--cursor-scrolline' is more
efficient that passing the result of `mistty-cursor' to this function."
  (save-restriction
    (+ mistty--sync-marker-scrolline (mistty--count-scrollines mistty-sync-marker pos))))

(defun mistty--scrolline-pos (scrolline)
  "Return the position of the beginning of SCROLLINE.

Return nil if SCROLLINE is not below the sync marker or outside the
region accessible from the current buffer.

Work in the work or term buffer."
  (save-restriction
    (save-excursion
      (when (>= scrolline mistty--sync-marker-scrolline)
        (goto-char mistty-sync-marker)
        (when (zerop (mistty--go-down-scrollines (- scrolline mistty--sync-marker-scrolline)))
          (point))))))

(defun mistty--needs-refresh ()
  "Let next call to `mistty--refresh' know there's something to refresh."
  (mistty--require-work-buffer)
  (setq mistty--need-refresh t))

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
  (when (and mistty--need-refresh (not mistty--inhibit-refresh) (not mistty--inhibit))
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (old-point-max (point-max))
          (point-was-at-cursor (or (null mistty--cursor-after-last-refresh)
                                   (= (point) mistty--cursor-after-last-refresh)))
          on-prompt)
      (mistty--copy-buffer-local-variables
       mistty-variables-to-copy mistty-term-buffer)
      (mistty--inhibit-undo
       (save-restriction
         (widen)
         (setq mistty--need-refresh nil)
         (setq on-prompt (and mistty-proc ;; doesn't need to be live
                              (buffer-live-p mistty-term-buffer)
                              (mistty-on-prompt-p (mistty-cursor))))

         (mistty-log "refresh (%s)" (if on-prompt "complete" "quick"))
         (mistty--sync-buffer mistty-term-buffer (not on-prompt))

         ;; Make fake newlines invisible. They're not really "visible"
         ;; to begin with, since they're at the end of the window, but
         ;; marking them invisible allows kill-line to go "through"
         ;; them, as it should.
         ;; TODO: ignore those that are not at the end of the window
         ;; using (window-max-chars-per-line)
         (save-excursion
           (goto-char mistty-sync-marker)
           (while-let ((prop-match
                        (text-property-search-forward 'term-line-wrap t t)))
             (add-text-properties (prop-match-beginning prop-match)
                                  (prop-match-end prop-match)
                                  '(invisible term-line-wrap yank-handler (nil "" nil nil)))))

         ;; Mark empty lines at EOB with mistty-skip.
         (let ((pos (point-max)))
           (while (and (> pos mistty-sync-marker)
                       (eq ?\n (char-before pos)))
             (cl-decf pos))
           (when (< pos (point-max))
             (add-text-properties pos (point-max)
                                  '(mistty-skip empty-lines-at-eob yank-handler (nil "" nil nil)))))

         ;; Right after a mistty-send-command, we're waiting for a line
         ;; after mistty--end-prompt that's not part of the old prompt.
         (when mistty--end-prompt
           (when-let* ((prompt mistty--active-prompt)
                       (scrolline (mistty--with-live-buffer mistty-term-buffer
                                    (mistty--term-scrolline)))
                       (end-scrolline (mistty--prompt-end prompt)))
             (when (>= scrolline end-scrolline)
               (when (mistty--maybe-move-sync-mark end-scrolline)
                 (mistty-log "Closing %s prompt #%s [%s-%s] (cursor at scrolline %s)"
                             (mistty--prompt-source prompt)
                             (mistty--prompt-input-id prompt)
                             (mistty--prompt-start prompt)
                             end-scrolline
                             scrolline)))))

         ;; detect prompt from bracketed-past region and use that to
         ;; restrict the sync region.
         (when-let ((prompt (mistty--prompt)))
           (when (and (not (mistty--prompt-realized prompt))
                      (memq (mistty--prompt-source prompt) '(bracketed-paste osc133))
                      (null (mistty--prompt-end prompt)))
             (when-let ((prompt-beg (mistty--scrolline-pos (mistty--prompt-start prompt)))
                        (cursor (when (process-live-p mistty-proc)
                                  (mistty-cursor))))
               (when (and (> cursor prompt-beg)
                          (or (eq 'osc133 (mistty--prompt-source prompt))
                              (string-match mistty--prompt-regexp
                                            (save-excursion
                                              (goto-char cursor)
                                              (mistty--scrolline-text-before-point
                                               'no-properties)))))
                 (mistty-log "Realized %s prompt #%s [%s-] @%s"
                             (mistty--prompt-source prompt)
                             (mistty--prompt-input-id prompt)
                             (mistty--prompt-start prompt)
                             prompt-beg)
                 (mistty--set-sync-mark prompt-beg (mistty--prompt-start prompt))
                 (setf (mistty--prompt-realized prompt) t)
                 (setq mistty--active-prompt prompt)))))

         (let ((v (and on-prompt (mistty--can-move-vertically-p))))
           (unless (eq v mistty--can-move-vertically)
             (mistty-log "Can move vertically: %s" v)
             (setq mistty--can-move-vertically v)))

         ;; Turn mistty-forbid-edit on or off
         (let ((forbid-edit (mistty--match-forbid-edit-regexp-p)))
           (cond
            ((and forbid-edit (not mistty--forbid-edit))
             (setq mistty--forbid-edit t)
             (overlay-put mistty--sync-ov 'keymap (mistty--active-prompt-map))
             (mistty--update-mode-lines)
             (mistty-log "FORBID EDIT on"))
            ((and (not forbid-edit) mistty--forbid-edit)
             (setq mistty--forbid-edit nil)
             (overlay-put mistty--sync-ov 'keymap (mistty--active-prompt-map))
             (mistty--update-mode-lines)
             (mistty-log "FORBID EDIT off"))))

         (unless mistty--active-prompt
           (mistty--with-live-buffer mistty-term-buffer
             ;; Next time, only sync the visible portion of the terminal.
             (when (< mistty-sync-marker term-home-marker)
               (let ((scrolline (mistty--term-scrolline-at-screen-start)))
                 (mistty--with-live-buffer mistty-work-buffer
                   (mistty--maybe-move-sync-mark scrolline))))

             ;; Truncate the term buffer, since scrolling back is available on
             ;; the work buffer anyways. This has to be done now, after syncing
             ;; the marker, and not in term-emulate-terminal, which is why
             ;; term-buffer-maximum-size is set to 0.
             (mistty--adjust-scrolline-base)
             (save-excursion
               (goto-char term-home-marker)
               (forward-line -5)
               (delete-region (point-min) (point)))))

         ;; Move the point to the cursor, if necessary.
         (when (process-live-p mistty-proc)
           (when (and (not (eq mistty-goto-cursor-next-time 'off))
                      (or mistty-goto-cursor-next-time point-was-at-cursor))
             (mistty-goto-cursor))
           (unless mistty--cursor-after-last-refresh
             (setq mistty--cursor-after-last-refresh (make-marker)))
           (move-marker mistty--cursor-after-last-refresh (mistty-cursor)))
         (setq mistty-goto-cursor-next-time nil)))
      (when (> (point-max) old-point-max)
        (mistty--maybe-scroll-window-down)))

    (mistty--report-self-inserted-text)))

(defun mistty--report-self-inserted-text ()
  "Report self-inserted text that was just echoed back.

This function compares the buffer with `mistty--self-insert-line'
to figure out a good time to call
`mistty-interactive-insert-hook'."
  (when (and mistty--self-insert-line
             (= (point) (mistty-cursor)))
    (let* ((str (apply #'concat (reverse (cdr mistty--self-insert-line))))
           (origin (- (point) (length str))))
      (when (and (string= str (mistty--safe-bufstring
                               origin (point)))
                 (eql (car mistty--self-insert-line)
                      (mistty--distance (mistty--bol (point)) origin)))
        (setq mistty--self-insert-line nil)
        (run-hook-wrapped 'mistty-interactive-insert-hook
                          #'mistty--run-hook-ignoring-errors)))))

(defun mistty--run-hook-ignoring-errors (func)
  "Run FUNC with ARGS ignoring any errors.

Errors are reported to `message' and ignored."
  (condition-case-unless-debug err
      (funcall func)
    (error
     (message "MisTTY: hook failed with %s. hook: %s" err func)))
  ;; call the next hooks
  nil)

(defun mistty-simulate-self-insert-command ()
  "Simulate the execution of `self-insert-command'.

This is meant to be called from `mistty-interactive-insert-hook'
to attempt to trigger auto-completion.

Does nothing unless the variable
`mistty-simulate-self-insert-command' is set to a non-nil
value (by default, it is nil)."
  (when mistty-simulate-self-insert-command
    (let ((last-command 'self-insert-command)
          (this-command 'self-insert-command))
      (run-hook-wrapped 'pre-command-hook #'mistty--wrap-pre-post-command)
      (run-hook-wrapped 'post-command-hook #'mistty--wrap-pre-post-command))))

(defun mistty--wrap-pre-post-command (func)
  "Run a pre or post command FUNC.

For `mistty-simulate-self-insert'."
  (unless (memq func (list #'mistty--pre-command #'mistty--post-command))
    (mistty--run-hook-ignoring-errors func))
  ;; call the next hooks
  nil)

(defun mistty--match-forbid-edit-regexp-p ()
  "Return t if `mistty-forbid-edit-regexp' matches, nil otherwise.

The region searched is from the line containing the cursor to end
of buffer. The match must start on the line containing the
cursor to be considered."
  (when (process-live-p mistty-proc)
    (let* ((pos (mistty-cursor))
           (bol (mistty--bol pos))
           (eol (mistty--eol pos))
           (regexps mistty-forbid-edit-regexps)
           (match nil))
      (while (and (not match) regexps)
        (save-excursion
          (goto-char bol)
          (when (and (search-forward-regexp (pop regexps) nil 'noerror)
                     (>= (match-beginning 0) bol)
                     (< (match-beginning 0) eol))
            (setq match t))))
      match)))

(defun mistty--can-move-vertically-p ()
  "Check whether vertical movements are allowed, return either nil or t.

Does not update `mistty--can-move-vertically'."
  (save-excursion
    (goto-char mistty-sync-marker)
    (catch 'mistty-end-loop
      (dolist (regexp mistty-move-vertically-regexps)
        (when (search-forward-regexp regexp (pos-eol) 'noerror)
          (throw 'mistty-end-loop t)))
      nil)))

(defun mistty--sync-buffer (source-buffer &optional quick)
  "Copy the sync region of SOURCE-BUFFER to the current buffer.

The region [mistty-sync-marker,(point-max)] is copied from PROC
buffer to the current buffer. Both buffers must have
`mistty-sync-marker' set.

The text and text properties of the destination buffer are
overwritten with the properties of SOURCE-BUFFER.

Unless QUICK evaluates to non-nil, markers, restrictions,
overlays and point of the destination buffer are moved as
relevant to the changes that happened on the process buffer since
the last update.

Does nothing if SOURCE-BUFFER is dead."
  (if quick
      ;; Quicker version of sync-buffer that doesn't bother with
      ;; markers.
      (save-restriction
        (widen)
        (let ((old-point (point))
              (at-eobp (eobp)))
          (goto-char mistty-sync-marker)
          (delete-region mistty-sync-marker (point-max))
          (insert-buffer-substring
           mistty-term-buffer
           (with-current-buffer mistty-term-buffer
             mistty-sync-marker))
          (unless at-eobp
            (goto-char old-point))))

    ;; Complete but expensive version of sync-buffer that conserves
    ;; markers.
    (let ((dest-buffer (current-buffer))
          (old-point (and (< (point) mistty-sync-marker) (point))))
      (mistty--with-live-buffer source-buffer
        (save-restriction
          (narrow-to-region mistty-sync-marker (point-max))
          (let ((properties (mistty--save-properties (point-min))))
            (with-current-buffer dest-buffer
              (save-restriction
                (narrow-to-region mistty-sync-marker (point-max))
                (replace-buffer-contents source-buffer 0.2)
                (mistty--restore-properties properties (point-min)))

              ;; If the point was outside the sync region, restore it,
              ;; as it has been moved by narrow-to-region . Otherwise,
              ;; trust replace-buffer-contents to do something
              ;; reasonable with it.
              (when old-point
                (goto-char old-point)))))))))

(defun mistty--copy-buffer-local-variables (variables source-buffer)
  "Copy the buffer-local values of VARIABLES between buffers.

Copies values from SOURCE-BUFFER to the current buffer."
  (dolist (var variables)
    (when-let (val (buffer-local-value var source-buffer))
      (make-local-variable var)
      (set var val))))

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
            (mistty-truncate cutoff)))))))

(defun mistty-truncate (cutoff)
  "Delete everything int the MisTTY buffer until CUTOFF.

Fails if CUTOFF is inside the synced region."
  (mistty--require-work-buffer)
  (save-restriction
    (widen)
    (when (> cutoff mistty-sync-marker)
      (error "Cutoff must be outside the synced region [%s-]"
             (marker-position mistty-sync-marker)))
    (delete-region (point-min) cutoff)))

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

(defun mistty--maybe-move-sync-mark (scrolline)
  "Move sync mark to SCROLLINE on both buffers, if possible.

This function checks whether SCROLLINE is displayed on the work buffer.
If it is, it sets the sync marker at that position and call
`mistty--set-sync-mark'.

Return nil if SCROLLINE is not accessible on the work buffer."
  (when-let ((pos (mistty--scrolline-pos scrolline)))
    (mistty--set-sync-mark pos scrolline)

    t))

(defun mistty--set-sync-mark (sync-pos scrolline)
  "Sync SYNC-POS with terminal at SCROLLINE.

SYNC-POS is a position on the work buffer that marks the start of the
terminal region.

SCROLLINE is a scrolline that's currently visible on the terminal."
  (mistty--require-work-buffer)
  (mistty--with-live-buffer mistty-term-buffer
    (let ((pos (mistty--term-scrolline-pos scrolline)))
      (unless pos
        (error "Scrolline %s not accessible in terminal buffer" scrolline))
      (set-marker mistty-sync-marker pos)))

  (unless (and (= sync-pos mistty-sync-marker)
               (= scrolline mistty--sync-marker-scrolline))
    (mistty-log "MOVE SYNC MARKER %s to %s at scrolline %s"
                (marker-position mistty-sync-marker)
                sync-pos
                scrolline)
    (setq mistty--active-prompt nil)
    (setq mistty--end-prompt nil)
    (mistty--process-archived-prompts sync-pos)
    (let ((old-marker-position (marker-position mistty-sync-marker)))
      (move-marker mistty-sync-marker sync-pos)
      (when (< old-marker-position sync-pos)
        (mistty--prepare-for-scrollback
         old-marker-position mistty--sync-marker-scrolline mistty-sync-marker)))
    (setq mistty--sync-marker-scrolline scrolline)
    (move-overlay mistty--sync-ov mistty-sync-marker (point-max))))

(defun mistty--update-sync-marker-scrolline ()
  "Update `mistty--sync-marker-scrolline' on the term and work buffers."
  (mistty--require-work-buffer)
  (let (scrolline)
    (mistty--with-live-buffer mistty-term-buffer
      (setq scrolline (mistty--term-scrolline-at mistty-sync-marker))
      (setq mistty--sync-marker-scrolline scrolline)
      (mistty-log "SYNC MARKER AT SCROLLINE %s ON SCREEN %s-"
                  scrolline
                  (mistty--term-scrolline-at-screen-start)))
    (when scrolline
      (setq mistty--sync-marker-scrolline scrolline))))

(defun mistty--process-archived-prompts (limit-pos)
  "Remove any archived prompt above END and mark their regions.

Only look up to LIMIT-POS.

This function cleans up `mistty--prompt-archive', removing prompts above END.

It also marks the prompt region with the text property
\\=`mistty-input-id so they can be detected by functions like
`mistty-next-output'."
  (let ((limit (mistty--scrolline limit-pos))
        (inhibit-modification-hooks t)
        (inhibit-read-only t))
    (when-let ((prompt (mistty--prompt)))
      (when (and (mistty--prompt-end prompt)
                 (<= (mistty--prompt-end prompt) limit))
        (setf (mistty--prompt) nil)))
    (dolist (prompt (mistty--prompt-archive))
      (when (mistty--prompt-realized prompt)
        (when-let ((prompt-beg (mistty--scrolline-pos
                                (mistty--prompt-start prompt)))
                   (prompt-end (when (mistty--prompt-end prompt)
                                 (mistty--scrolline-pos
                                  (mistty--prompt-end prompt)))))
          (when (> prompt-end prompt-beg)
            (mistty-log "End %s prompt #%s. Mark input range: [%s-%s]/[%s-%s]"
                        (mistty--prompt-source prompt)
                        (mistty--prompt-input-id prompt)
                        (mistty--prompt-start prompt)
                        (mistty--prompt-end prompt)
                        prompt-beg
                        prompt-end)
            (put-text-property
             prompt-beg prompt-end
             'mistty-input-id (mistty--prompt-input-id prompt))))))
    (setf (mistty--prompt-archive) nil)))

(defun mistty--prepare-for-scrollback (beg scrolline end)
  "Transition a region from the terminal to the scrollback zone.

This function modifies the region from BEG to END as appropriate
for a scrollback area that's not going to be refreshed from the
terminal anymore.

SCROLLINE is the scrolline at BEG.

It removes the fake newlines which are not useful anymore and
just tend to cause issues."
  (let ((inhibit-modification-hooks t)
        (inhibit-read-only t))
    (mistty--mark-scrollines beg scrolline end)
    (when (not mistty--inhibit-fake-nl-cleanup)
      (mistty--remove-fake-newlines beg end))))

(defun mistty--mark-scrollines (beg scrolline end)
  "Add text property \\='mistty-scrolline to scrollines from BEG to END.

SCROLLINE is the scrolline at BEG."
  (save-excursion
    (goto-char beg)
    (while (< (point) end)
      (let ((bos (mistty--beginning-of-scrolline-pos))
            (eos (mistty--end-of-scrolline-pos)))
        (when (eq ?\n (char-after eos))
          (cl-incf eos))
        (when (> eos bos)
          (put-text-property bos eos 'mistty-scrolline scrolline))
        (cl-incf scrolline)
        (mistty--go-down-scrollines 1)))))

(defun mistty-send-string (str)
  "Send STR to the process."
  (mistty--require-proc)
  (mistty--enqueue-str mistty--queue str))

(defun mistty-send-command ()
  "Send the current command to the shell.

This also deactivates the mark, as it looks strange otherwise to
have the command prompt and output marked."
  (interactive)
  (deactivate-mark)
  (mistty--require-proc)
  (mistty--enqueue
   mistty--queue
   (mistty--interact send-command (interact)
     (mistty-maybe-realize-possible-prompt)
     (setq mistty-goto-cursor-next-time t)
     (when (and mistty-proc
                (mistty-on-prompt-p (point))
                (mistty-on-prompt-p (mistty-cursor)))
       (setq mistty--end-prompt (mistty-cursor)))
     (mistty--interact-send interact "\C-m")
     (mistty--interact-return-then
      interact
      (lambda (&optional _)
        (setq mistty--interacted t)
        (mistty--interact-done))))))

(defun mistty-newline (&optional n)
  "Send one ore more newlines that won't submit the current command.

This only works when the program tied to the terminal supports bracketed
paste mode, as most recent version of shells do.

If N is a positive integer that many newlines."
  (interactive "p")
  (unless mistty-bracketed-paste
    (error "Newlines not supported in this context"))
  (let* ((nls (make-string (or n 1) ?\n))
         (nl-seq (concat "\e[200~" nls "\e[201~")))
    (cond
     ((and (buffer-live-p mistty-work-buffer)
           (not (buffer-local-value 'mistty-fullscreen mistty-work-buffer)))
      (with-current-buffer mistty-work-buffer
        (mistty-before-positional)
        (mistty--enqueue
         mistty--queue
         (mistty--interact newline (interact)
           (if (mistty-on-prompt-p (point))
               (progn
                 (setq mistty-goto-cursor-next-time t)
                 (mistty--interact-send interact nl-seq)
                 (mistty--interact-return-then
                  interact #'mistty--interact-done))
             (insert nls)
             (mistty--interact-done))))))

     ((process-live-p mistty-proc)
      (mistty--send-string mistty-proc nl-seq))

     (t (insert "\n")))))

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
  (or (mistty-self-insert-p key)
      (and (length= key 1)
           (characterp (aref key 0))
           (seq-contains-p mistty-positional-keys (aref key 0)))))

(defun mistty-self-insert (&optional n c)
  "Send a self-inserting character to the terminal.

If N is set, send the key that many times. It defaults to 1.

C is the character to send, a single character."
  (interactive "p")
  (if mistty--inhibit
      (self-insert-command n c)
    (mistty-send-key n (when c (make-string 1 c)) 'positional)))

(defun mistty-backward-delete-char (&optional n)
  "Send DEL N times to the terminal.

If N is unset, send DEL once. If N is negative, send Control d
that many times instead."
  (interactive "p")
  (if mistty--inhibit
      (call-interactively 'backward-delete-char)
    (if (and (numberp n) (< n 0))
        (mistty-send-key (abs n) "\C-d" 'positional)
      (mistty-send-key n mistty-del 'positional))))

(defun mistty-delete-char (&optional n)
"Send Control D N times to the terminal.

If N is unset, send Control d once. If N is negative, send DEL
that many times instead."
  (interactive "p")
  (if mistty--inhibit
      (delete-char n)
    (setq mistty--interacted t)
    (if (and (numberp n) (< n 0))
        (mistty-send-key (abs n) mistty-del 'positional)
      (mistty-send-key n "\C-d" 'positional))))

(defun mistty-tab-command (&optional n)
  "Send a TAB to the terminal.

If N is specified, do it N times."
  (interactive "p")
  (if mistty--inhibit
      (call-interactively 'indent-for-tab-command)
    (mistty-send-key n (kbd "TAB") 'positional)))

(defun mistty-send-key (&optional n key positional)
  "Send the current key sequence to the terminal.

This command sends N times the current key sequence, or KEY if it
is specified, directly to the terminal. If the key sequence is
positional or if POSITIONAL evaluates to true, MisTTY attempts to
move the terminal's cursor to the current point.

KEY must be a string or vector as would be returned by `kbd'.

This command is available in fullscreen mode."
  (interactive "p")
  (mistty--require-proc)
  (let* ((key (or key (this-command-keys-vector)))
         (translated-key (mistty-translate-key key n))
         (fire-and-forget (string-match "^[[:graph:]]+$" translated-key))
         (positional (or positional (mistty-positional-p key))))
    (cond
     ((and (buffer-live-p mistty-work-buffer)
           (not (buffer-local-value
                 'mistty-fullscreen mistty-work-buffer)))
      (with-current-buffer mistty-work-buffer
        (when (and positional
                   (not (and (eq this-command 'mistty-self-insert)
                             (eq last-command 'mistty-self-insert))))
          (mistty-before-positional))
        (mistty--enqueue
         mistty--queue
         (mistty--interact send-key (interact)
           (when (eq this-command 'mistty-self-insert)
             (unless mistty--self-insert-line
               (setq mistty--self-insert-line
                     (cons (mistty--distance (mistty--bol (point)) (point)) nil)))
             (push translated-key (cdr mistty--self-insert-line)))
           (setq mistty-goto-cursor-next-time t)
           (mistty--maybe-add-key-to-undo n key (mistty-cursor))
           (mistty--interact-send interact translated-key)
           (if fire-and-forget
               (mistty--interact-done)
             (mistty--interact-return-then
              interact #'mistty--interact-done))))))

     ((process-live-p mistty-proc)
      (mistty--send-string mistty-proc translated-key))

     (t (error "No live process")))))

(defun mistty-send-C-r (n)
  "Send Control r to the terminal, usually backward history search.

If an argument is specified, repeat it N time."
  (interactive "p")
  (mistty-send-key n (kbd "C-r")))

(defun mistty-send-C-p (n)
  "Send Control p to the terminal, usually move up or prev history.

If an argument is specified, repeat it N time."
  (interactive "p")
  (mistty-send-key n (kbd "C-p")))

(defun mistty-send-C-n (n)
  "Send Control n to the terminal, usually move down or next history.

If an argument is specified, repeat it N time."
  (interactive "p")
  (mistty-send-key n (kbd "C-n")))

(defun mistty-send-key-sequence ()
  "Send all keys to terminal until interrupted.

This function continuously read keys and sends them to the
terminal, just like `mistty-send-key', until it is interrupted
with \\[keyboard-quit] or until it is passed a key or event it
doesn't support, such as a mouse event.."
  (interactive)
  (mistty--require-proc)
  (let ((proc mistty-proc)
        key)
    (while
        (and
         (setq key
               (read-key "Sending all KEYS to terminal... Exit with C-g."
                         'inherit-input-method))
         (not (eq key ?\C-g)))
      (pcase key
        (`(xterm-paste ,str)
         (mistty--send-string proc (mistty--maybe-bracketed-str str)))
        (_ (mistty-send-key 1 (make-vector 1 key)))))))

(defun mistty-beginning-of-line (&optional n)
  "Go to the Nth beginning of line, possibly by sending Control a.

This command moves the point to the beginning of the line, either
by calling `beginning-of-line' or by sending Control a to the
shell, if on a prompt.

With an argument, this command just calls `beginning-of-line' and
forwards the argument to it."
  (interactive "p")
  (let ((n (or n 1)))
    (if (and (= n 1)
             (process-live-p mistty-proc)
             mistty--queue)
        (mistty--enqueue
         mistty--queue
         (mistty--interact bol (interact)
           ;; While C-a is not, strictly-speaking, a positional,
           ;; it's a good sign that we're on a prompt.
           (if (or (mistty-maybe-realize-possible-prompt (point))
                   (mistty-on-prompt-p (point)))
               (progn
                 (setq mistty-goto-cursor-next-time t)
                 (mistty--interact-send interact "\C-a")
                 (mistty--interact-return-then
                  interact #'mistty--interact-done))
             (beginning-of-line n)
             (mistty--interact-done))))
      (beginning-of-line n))))

(defun mistty-end-of-line-or-goto-cursor (&optional n)
  "Move the point to the end of the Nth line, then to the cursor.

The first time this command is called, it moves the point to the
end of the line, either using `end-of-line' or, if on a prompt,
by sending Control e to the shell.

The second time this command is called, it moves the point to the
cursor.

With an argument, this command just calls `end-of-line' and
forwards the argument to it."
  (interactive "p")
  (let ((n (or n 1)))
    (if (and (= 1 n)
             (eq last-command this-command)
             mistty-proc ;; doesn't have to be running
             (/= (point) (mistty-cursor)))
        (progn (mistty-log "goto cursor") (mistty-goto-cursor))
      (progn (mistty-log "goto eol %s" n) (mistty-end-of-line n)))))

(defun mistty-end-of-line (&optional n)
  "Move the point to the end of the Nth line.

This command moves the point to the end of the line, either using
`end-of-line' or, if on a prompt, by sending Control e to the
shell.

With an argument, this command just calls `end-of-line' and
forwards the argument to it."
  (interactive "p")
  (let ((n (or n 1)))
    (cond
     (mistty--inhibit (end-of-line n))
     ((and (= 1 n)
           (process-live-p mistty-proc)
           mistty--queue)
      (mistty--enqueue
       mistty--queue
       (mistty--interact eol (interact)
         ;; While C-e is not, strictly-speaking, a positional, it's
         ;; a good sign that we're on a prompt.
         (if (or (mistty-maybe-realize-possible-prompt (point))
                 (mistty-on-prompt-p (point)))
             (progn
               ;; If anything, move point to the cursor. This might
               ;; be the only visible effect if the cursor is
               ;; already at what the shell considers eol.
               (mistty-goto-cursor)
               (setq mistty-goto-cursor-next-time t)
               (mistty--interact-send interact "\C-e")
               (mistty--interact-return-then
                interact #'mistty--interact-done))
           (end-of-line n)
           (mistty--interact-done)))))
     (t
      (end-of-line n)))))

(defun mistty--after-change-on-work (beg end old-length)
  "Handler for modifications made to the work buffer.

BEG and END are the changed region and OLD-LENGTH the length of
that region before the change.

This is meant to be added to ==\'after-change-functions."
  (if (and mistty-sync-marker (>= end mistty-sync-marker))
      ;; In sync region
      (let ((inhibit-read-only t)
            (beg (max beg mistty-sync-marker))
            (end (max end mistty-sync-marker))
            (old-end (max (+ beg old-length) mistty-sync-marker))
            (cs (mistty--activate-changeset)))
        ;; Temporarily stop refreshing the work buffer while collecting
        ;; modifications.
        (setq mistty--inhibit-refresh t)

        (mistty--inhibit-undo
         (mistty--changeset-mark-region cs beg end old-end)))))

(defun mistty--replay-interaction (cs)
  "Build a `mistty--interact' of type \\='replay to replay CS.

If CS is empty, it is released immediately and this function returns nil
instead of a interaction.

Replay interactions are callable with `mistty--call-interact'.
They take a single argument, another changeset, and attempt to
append that changeset to the current one. If that works, the
changeset is released and the call returns t, otherwise the call
returns nil."
  (let ((interact (mistty--make-interact 'replay))

        ;; mistty--changeset-modification extracts modification from
        ;; the buffer. It must be called when the interaction is
        ;; created, not when it is run
        (modifications (mistty--changeset-modifications cs))
        (calling-buffer (current-buffer))
        (term-buffer mistty-term-buffer)
        (inhibit-moves mistty--forbid-edit)
        (beg (make-marker))
        (old-end (make-marker))

        trailing-ws-to-delete target
        backstage lower-limit upper-limit distance
        orig-beg content old-length waiting-for-last-change
        inserted-detector-regexp point-after-last-insert)

      ;; If the point is after the last insert, which is very common,
      ;; trust the cursor position instead of relying on
      ;; replace-buffer-contents to set the point properly. This is
      ;; cheaper and more reliable, as replace-buffer-contents has
      ;; trouble in some cases, especially when the inserted string is
      ;; too short or contains only spaces.
      (setq point-after-last-insert
            (when-let ((m (car (last modifications))))
              (when (length> (nth 1 m) 0)
                (equal (point) (+ (nth 0 m) (length (nth 1 m)))))))

      ;; Init interact
      (cl-labels
          ((start (&optional _) ;; Interact entry point
             (set-buffer calling-buffer)
             (setq backstage (mistty--create-backstage mistty-proc))
             (let ((work-sync-marker (marker-position mistty-sync-marker)))
               (set-buffer backstage)
               ;; Move modifications positions into the backstage buffer.
               ;; Rely on markers to keep the positions valid through
               ;; buffer modifications.
               (dolist (m modifications)
                 (setcar m (copy-marker (+ (car m) (- work-sync-marker) (point-min))))))
             (setq lower-limit (point-min-marker))
             (setq upper-limit (point-max-marker))
             (next-modification))

           (next-modification ()
             (if modifications
                 (handle-modification (pop modifications))
               (done-handling-modifications)))

           (handle-modification (m)
             (setq orig-beg (nth 0 m))
             (setq content (nth 1 m))
             (setq old-length (nth 2 m))

             (move-marker beg orig-beg)
             (if (< old-length 0)
                 (let ((end
                        ;; When looking for the end of the text to be
                        ;; deleted marked with old-length=-1, ignore the
                        ;; final \n or anything marked mistty-skip, as
                        ;; these cannot be deleted.
                        (save-excursion
                          (goto-char (point-max))
                          (when (eq (char-before) ?\n)
                            (goto-char (1- (point))))
                          (while (get-text-property (1- (point)) 'mistty-skip)
                            (goto-char (1- (point))))
                          (point))))
                   (setq old-length (if (> end orig-beg) (- end orig-beg) 0))
                   (move-marker old-end (max orig-beg end)))
               (move-marker old-end (+ orig-beg old-length)))

             ;; never delete the final \n that some shells add.
             (when (and (> old-length 0)
                        (= old-end (point-max))
                        (= ?\n (char-before old-end)))
               (move-marker old-end (1- old-end))
               (setq old-length (1- old-length)))

             ;; don't even try to move through trailing ws, as they may
             ;; not exist (Issue #34)
             (setq trailing-ws-to-delete 0)
             (when (memq (char-after old-end) '(nil ?\n))
               (let ((at-end (<= (mistty--last-non-ws) old-end )))
                 (while (and (> old-length 0)
                             (eq ?\  (char-before old-end)))
                   (move-marker old-end (1- old-end))
                   (when at-end
                     (cl-incf trailing-ws-to-delete))
                   (cl-decf old-length))))

             (mistty-log "replay: %s %s %s old-content: '%s' (limit: [%s-%s])"
                         (marker-position orig-beg)
                         content
                         old-length
                         (mistty--safe-bufstring beg old-end)
                         (marker-position lower-limit)
                         (marker-position upper-limit))
             (if (> old-length 0)
                 (setq target old-end)
               (setq target beg))
             (if (and (zerop old-length) (equal "" content))
                 ;; The modification is empty, move on to the next one.
                 ;; This can happen when the change specified "delete to
                 ;; the end of the buffer" and there was nothing to
                 ;; delete.
                 (next-modification)
               (move-to-target)))

           (move-to-target ()
             (cond
              ((> target upper-limit)
               (mistty-log "SKIP target=%s > upper-limit=%s"
                           (marker-position target)
                           (marker-position upper-limit))
                              (next-modification))

              ((and (< target lower-limit))
               (mistty-log "SKIP target=%s < lower-limit=%s"
                           (marker-position target)
                           (marker-position lower-limit))
               (next-modification))

              (t
               (when inhibit-moves
                 (mistty-log "INHIBITED: to target: %s -> %s" (point) target)
                 (after-move-to-target))

               (let* ((distance (mistty--vertical-distance (point) target))
                      (term-seq (mistty--move-vertically-str distance)))
                 (when (mistty--nonempty-str-p term-seq)
                   (mistty-log "to target: %s -> %s lines: %s (can-move-vertically=%s)"
                               (point) target distance mistty--can-move-vertically)
                   (mistty--interact-send interact term-seq)
                   (mistty--interact-return-then
                    interact #'move-horizontally
                    :pred (let ((comparison (cond (mistty--can-move-vertically '=)
                                                  ((< distance 0) '<=)
                                                  (t '>=))))
                            (lambda ()
                              (mistty--update-backstage)
                              (funcall comparison 0 (mistty--vertical-distance
                                                     (point) target))))
                    ;; after-move-to-target-f deals with the point not being
                    ;; where it should.
                    :on-timeout #'after-move-to-target))
                 (move-horizontally)))))

           (move-horizontally ()
             (setq distance (mistty--distance (point) target))
             (mistty-log "to target: %s -> %s distance: %s" (point) target distance)
             (let ((term-seq (mistty--move-horizontally-str distance)))
               (when (mistty--nonempty-str-p term-seq)
                 (mistty--interact-send interact term-seq)
                 (mistty--interact-return-then
                  interact #'after-move-to-target
                  :pred (lambda ()
                          (mistty--update-backstage)
                          (zerop (mistty--distance (point) target))))))
             (delete-lines))

           (after-move-to-target ()
             (mistty--update-backstage)
             (mistty-log "Got to %s" (point))
             (cond
              ((and (> (point) target)
                    (> (mistty--distance target (point)) 0))
               (mistty-log "LOWER LIMIT: %s (wanted %s)" (point) target)
               (move-marker lower-limit (point))
               (if (= old-length 0)
                   ;; insert anyways
                   (progn
                     (mistty-log "insert anyway, at %s instead of %s"
                                 (point) (marker-position target))
                     (move-marker target (point))
                     (insert-and-delete))
                 ;; skip delete or replace
                 (mistty-log "SKIP delete or replace; %s (wanted %s)"
                             (point) (marker-position target))
                 (next-modification)))

              ((and (> target (point))
                    (> (mistty--distance (point) target) 0))
               (mistty-log "UPPER LIMIT: %s (wanted %s)"
                           (point) (marker-position target))
               (move-marker upper-limit (point))
               (if (>= (point) beg)
                   (progn
                     (move-marker old-end (point))
                     (setq old-length (- old-end beg))
                     (insert-and-delete))
                 (next-modification)))

              (t
               (move-marker target (point))
               (delete-lines))))

           ;; For multi-line delete, delete line by line. This allows not
           ;; knowing where a line really ends (Issue #34).
           (delete-lines ()
             (let ((lines (mistty--vertical-distance beg old-end)))
               (when (> lines 0)
                 (let ((bol (save-excursion
                              (goto-char old-end)
                              (catch 'mistty-bol
                                (while (search-backward "\n" beg 'noerror)
                                  (unless (get-text-property (match-beginning 0) 'term-line-wrap)
                                    (throw 'mistty-bol (match-end 0))))))))
                   (when (and bol (<= beg bol old-end))
                     (mistty-log "delete line: [%s-%s] beg: %s"
                                 (1- bol)
                                 (marker-position old-end)
                                 (marker-position beg))
                     (mistty--interact-send
                      interact (mistty--repeat-string
                                (1+ (mistty--distance bol old-end)) "\b"))
                     (mistty--interact-return-then
                      interact
                      (lambda ()
                        (move-marker old-end (point))
                        (setq old-length (max 0 (- old-end beg)))

                        ;; Maybe delete another line
                        (delete-lines))
                      :pred (lambda ()
                              (mistty--update-backstage)
                              (< (mistty--vertical-distance
                                  beg (point)) lines))
                      ;; If we can't even delete lines, just give up and move
                      ;; on to the next modification.
                      :on-timeout #'after-insert-and-delete))))
               (insert-and-delete)))

           (insert-and-delete ()
             (mistty-log "insert and delete: point: %s beg: %s old-end: %s"
                         (point)
                         (marker-position beg)
                         (marker-position old-end))
             (let ((term-seq
                    (concat
                     ;; delete
                     (when (> old-length 0)
                       (let ((char-count (mistty--distance beg old-end)))
                         (mistty-log "DELETE %s chars (was %s)" char-count old-length)
                         (mistty--repeat-string char-count mistty-del)))

                     ;; delete trailing ws
                     (when (> trailing-ws-to-delete 0)
                       (mistty-log "DELETE %s trailing whitespaces with C-k" trailing-ws-to-delete)
                       (mistty--repeat-string 1 "\C-k"))

                     ;; insert
                     (when (length> content 0)
                       (mistty-log "INSERT: '%s'" content)
                       (mistty--maybe-bracketed-str content)))))
               (when (mistty--nonempty-str-p term-seq)

                 ;; ignore term-line-wrap and mistty-skip when
                 ;; building and running the detector.
                 (mistty--remove-text-with-property 'term-line-wrap)
                 (mistty--remove-text-with-property 'mistty-skip)
                 (setq inserted-detector-regexp
                       (concat
                        "^"
                        (regexp-quote (mistty--safe-bufstring
                                       (mistty--bol beg) beg))
                        (string-replace "\n" " *\n" (regexp-quote content))))
                 (mistty-log "RE /%s/" inserted-detector-regexp)
                 (unless modifications
                   (setq waiting-for-last-change t))
                 (mistty--interact-send interact term-seq)
                 (mistty--interact-return-then
                  interact #'after-insert-and-delete
                  :pred (lambda ()
                          (mistty--update-backstage)
                          (mistty--remove-text-with-property 'term-line-wrap)
                          (mistty--remove-text-with-property 'mistty-skip)
                          (looking-back inserted-detector-regexp (point-min))))))

             ;; Nothing to do, move on to the next modification, if any
             (next-modification))

           (after-insert-and-delete ()
             (setq waiting-for-last-change nil)
             (mistty--update-backstage)
             (mistty--with-live-buffer term-buffer
               (mistty--detect-dead-spaces-after-insert
                content (+ mistty-sync-marker (marker-position beg))))

             ;; Move right prompt just like the shell would, to avoid it
             ;; confusing the sync happening after applying all
             ;; modifications.
             (when-let ((content-nl (string-match "\n" content)))
               (with-current-buffer calling-buffer
                 (let* ((content-end (+ orig-beg (length content)))
                        (eol (mistty--eol content-end)))
                   (when-let ((right-prompt
                               (text-property-any content-end eol
                                                  'mistty-skip 'right-prompt)))
                     (save-excursion
                       (let ((inhibit-modification-hooks t)
                             (inhibit-read-only t)
                             (right-prompt-content (buffer-substring right-prompt eol)))
                         (goto-char (+ orig-beg content-nl))
                         (delete-region right-prompt eol)
                         (insert right-prompt-content)))))))

             (next-modification))

           (done-handling-modifications ()
             (set-buffer calling-buffer)

             ;; Force refresh, even if nothing was sent, if only to revert what
             ;; couldn't be replayed.
             (mistty--needs-refresh)

             (if (or inhibit-moves point-after-last-insert)
                 (setq mistty-goto-cursor-next-time t)

               ;; Move cursor back to point unless the next interact is a
               ;; replay, in which case we let the replay move the cursor.
               (setq mistty-goto-cursor-next-time 'off)
               (let ((next (car (mistty--queue-more-interacts mistty--queue))))
                 (when (or (null next)
                           (not (eq 'replay (mistty--interact-type next))))
                   (mistty--enqueue
                    mistty--queue
                    (mistty--cursor-to-point-interaction) 'prepend))))
             (mistty--interact-done))

           ;; Handles (mistty--call-interact)
           ;;
           ;; (mistty--call-interact interact 'replay OTHER-CS)
           ;; attempts to append modifications to an existing replay.
           ;;
           ;; OTHER-CS must be a changeset.
           ;;
           ;; If OTHER-CS can be appended to the current set of
           ;; modifications, this function takes ownership of OTHER-CS
           ;; and returns non-nil. Otherwise, this function returns
           ;; nil and the caller retains ownership of OTHER-CS.
           (handle-call-interact (other-cs)
             (when-let ((text-to-insert (mistty--changeset-single-insert other-cs)))
               (when (and
                      (eql (mistty--changeset-beg other-cs)
                           (mistty--changeset-end cs))
                      (cond
                       (modifications
                        (let* ((tail (last modifications))
                               (m (car tail))
                               (beg (nth 0 m))
                               (content (nth 1 m))
                               (old-length (nth 2 m)))
                          (setcar tail
                                  (list beg (concat content text-to-insert) old-length))
                          t))

                       ((and (null modifications) waiting-for-last-change)
                        (mistty--send-string mistty-proc text-to-insert)
                        (setq inserted-detector-regexp
                              (concat inserted-detector-regexp
                                      (regexp-quote text-to-insert)))
                        (mistty-log "updated RE /%s/" inserted-detector-regexp)
                        t)))
                 (setf (mistty--changeset-end cs)
                       (mistty--changeset-end other-cs))
                 (mistty--release-changeset other-cs)
                 t)))

           ;; Cleanup any open state. This is called when the interact
           ;; is closed, from (mistty--interact-close interact).
           ;;
           ;; The interact might have been run fully, not have been
           ;; run at all, or have been run partially. Cleanup can be
           ;; done in all cases.
           (cleanup ()
             (mistty--delete-backstage backstage)

             ;; Always release the changeset at the end and re-enable
             ;; refresh.
             (mistty--release-changeset cs)
             (mistty--refresh-after-changeset)))

        (setf (mistty--interact-cb interact) #'start)
        (setf (mistty--interact-call interact) #'handle-call-interact)
        (setf (mistty--interact-cleanup interact) #'cleanup))

      (if modifications
          interact

        ;; Nothing to do; clean things up right away
        (mistty--interact-close interact)
        nil)))

(defun mistty--refresh-after-changeset ()
  "Refresh the work buffer again if there are not more changesets."
  (unless mistty--changesets
    (setq mistty--inhibit-refresh nil)
    (mistty--refresh)))

(defun mistty--move-horizontally-str (direction)
  "Return a key sequence to move horizontally.

The absolute value of DIRECTION specifies the number of character
to move and the sign specifies whether to go right (positive) or
left (negative)."
  (if (zerop direction)
      ""
    (let ((distance (abs direction))
          (towards-str
           (if (< direction 0)
               mistty-left-str
             mistty-right-str)))
      (mistty--repeat-string distance towards-str))))

(defun mistty--move-vertically-str (direction)
  "Return a key sequence to move vertically.

The absolute value of DIRECTION specifies the number of lines
to move and the sign specifies whether to go down (positive) or
up (negative)."
  (if (zerop direction)
      ""
    (let ((distance (abs direction))
          (towards-str
           (if (< direction 0)
               (if mistty--can-move-vertically
                   mistty-up-str
                 (concat "\C-a" mistty-left-str))
             (if mistty--can-move-vertically
                 mistty-down-str
               (concat "\C-e" mistty-right-str)))))
      (mistty--repeat-string distance towards-str))))

(defun mistty-next-input (n)
  "Move the point to the Nth next input in the buffer."
  (interactive "p")
  (if-let ((prompt
            (mistty--forward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (< (point) (nth 0 prompt)) ; input must be below point
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (goto-char (mistty--user-input-start prompt))
    (error "No next input")))

(defun mistty-previous-input (n)
  "Move the point to the Nth previous input in the buffer."
  (interactive "p")
  (if-let ((prompt
            (mistty--backward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (and (>= (point) (nth 1 prompt))  ; input must be above point
                            (/= (point-max) (nth 2 prompt))) ;; ignore active prompt
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (goto-char (mistty--user-input-start prompt))
    (error "No previous input")))

(defun mistty-next-output (n)
  "Move the point to the beginning of the Nth next output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (if-let ((prompt
            (mistty--forward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (and
                        (< (point) (nth 1 prompt)) ; output must be above point
                        (> (nth 2 prompt) (nth 1 prompt))) ; ignore empty prompts
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (prog1 (cons (nth 1 prompt) (nth 2 prompt))
        (goto-char (nth 1 prompt)))
    (error "No next output")))

(defun mistty-previous-output (n)
  "Move the point to the beginning of the Nth previous output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (if-let ((prompt
            (mistty--backward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (and (> (point) (nth 2 prompt)) ; output must be above point
                            (> (nth 2 prompt) (nth 1 prompt))) ; ignore empty prompts
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (prog1 (cons (nth 1 prompt) (nth 2 prompt))
        (goto-char (nth 1 prompt)))
    (error "No previous output")))

(defun mistty-current-output-range ()
  "Return the range of the current output, if point is on an output.

This function returns nil, if the point is not inside an output.
Otherwise, it returns a cons containing the start and end
position of the output."
  (let ((prompt (mistty--prompt-ranges-around
                 (point) (mistty--active-or-potential-prompt-ranges))))
    (if (and prompt
             (and (<= (nth 1 prompt) (point))  ; point must be below input
                  (< (point) (nth 2 prompt)))) ; point must be above output end
        (cons (nth 1 prompt) (nth 2 prompt)))))

(defun mistty-clear (n)
  "Clear the MisTTY buffer until the end of the last output.

With an argument, clear from the end of the last Nth output."
  (interactive "p")
  (let ((range (save-excursion (mistty-previous-output (or n 1)))))
    (mistty-truncate (min mistty-sync-marker (cdr range)))))

(defun mistty-select-output (&optional n)
  "Select the current or Nth previous output range."
  (interactive "P")
  (let ((prompt-ranges (mistty--prompt-ranges-for-current-or-previous-output n)))
    (goto-char (nth 1 prompt-ranges))
    (set-mark (nth 2 prompt-ranges))))

(defun mistty-create-buffer-with-output (buffer-name &optional n)
  "Create the new buffer BUFFER-NAME with N'th previous output.

If called with no arguments and the point is on an output, create
a buffer with that output."
  (interactive
   (list (read-buffer
          "New buffer name: "
          ;; Generate a reasonable default new buffer name.
          (generate-new-buffer-name
           (mistty--truncate-string
            (or
             (condition-case nil
                 (when-let ((prompt (mistty--prompt-ranges-for-current-or-previous-output
                                     current-prefix-arg)))
                   (mistty--command-for-output prompt))
               (error nil))
             "command output")
            30)))
          current-prefix-arg))
  (let ((prompt (mistty--prompt-ranges-for-current-or-previous-output n))
        (buffer (generate-new-buffer buffer-name)))
    (copy-to-buffer buffer (nth 1 prompt) (nth 2 prompt))
    (with-current-buffer buffer
      (set-auto-mode 'keep-mode-if-sane))
    (pop-to-buffer buffer)
    buffer))

(defun mistty--user-input-start (prompt-ranges)
  "Find the position of the start of user input within PROMPT-RANGES."
  (let ((start (nth 0 prompt-ranges))
        (end (nth 1 prompt-ranges))
        pos)
    (save-excursion
      (cond
       ;; Prompt is marked.
       ((setq pos (text-property-any start end 'field 'prompt))
        (next-single-property-change pos 'field nil end))

       ;; Prompt is not marked. Look for something that looks like a prompt.
       ((progn
          (goto-char end)
          (search-backward-regexp "^[^#$%>\n]*[#$%>] +\\([[:alpha:]]+\\|$\\)" start 'noerror))
        (match-beginning 1))

       ;; Last line look like the end of a multi-line-prompt. Keep
       ;; everything in [start,end].
       ((progn
          (goto-char end)
          (while (and (>= (point) start)
                      (eq ?\n (char-before (point))))
            (goto-char (1- (point))))
          (goto-char (pos-bol))
          (looking-at "[[:blank:]]*\\(end\\|done\\|fi\\)"))
        start)

       ;; Keep only the last line within [start,end].
       (t (mistty--bol end))))))

(defun mistty--command-for-output (prompt-ranges)
  "Extract command for PROMPT-RANGES, if possible.

Note that this is very unreliable unless the shell issued OSS code B to
mark the prompt beginning.

Return nil if no command could be extracted."
  (let ((prompt-text (buffer-substring
                      (mistty--user-input-start prompt-ranges)
                      (nth 1 prompt-ranges))))
    (with-temp-buffer
      (insert prompt-text)

      ;; Remove anything marked mistty-skip.
      (let ((pos (point-min)))
        (while (and
                (> (point-max) pos)
                (setq pos (text-property-not-all
                           pos (point-max) 'mistty-skip nil)))
          (delete-region pos (next-single-property-change
                              pos 'mistty-skip nil (point-max)))))

      ;; Trim
      (goto-char (point-min))
      (while (search-forward-regexp "^[[:blank:]\n\r]+" nil 'noerror)
        (replace-match ""))
      (goto-char (point-min))
      (while (search-forward-regexp "[[:blank:]\n\r]+$" nil 'noerror)
        (replace-match ""))

      ;; Get rid of newlines.
      (goto-char (point-min))
      (while (search-forward-regexp
              "[[:blank:]]*\n[[:blank:]]*" nil 'noerror)
        (replace-match mistty-newline-replacement))

      ;; Return whatever is left
      (when (> (point-max) (point-min))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun mistty--prompt-ranges-for-current-or-previous-output (&optional n)
  "Return the promptrange of the current or the Nth output.

If N is a number, behave like `mistty-previous-output', otherwise
return either the current or te previous output

This function fails if there is no current or previous output."
  (let ((must-be-above-point (numberp n))
        (n (if (numberp n) n 1)))
    (if-let ((prompt
              (mistty--backward-prompt-ranges
               (let ((accepted-count 0))
                 (lambda (prompt)
                   (when (and
                          ;; Output must be above point (same as previous-output)
                          (or (not must-be-above-point)
                              (> (point) (nth 2 prompt)))
                          ;; Ignore empty prompts
                          (> (nth 2 prompt) (nth 1 prompt)))
                     (cl-incf accepted-count))

                   (>= accepted-count n))))))
        prompt
      (error "No current or previous output"))))

(defun mistty--prompt-ranges-around (pos active-prompt-ranges)
  "Return ranges of prompt around POS.

If a prompt if found around POS, return (list prompt-start output-start
output-end), otherwise return nil.

ACTIVE-PROMPT-RANGES must be the output of
`mistty--active-or-potential-prompt-ranges'."
  (catch 'mistty-return
    (when (and active-prompt-ranges (>= pos (nth 0 active-prompt-ranges)))
      (throw 'mistty-return active-prompt-ranges))
    (unless (get-text-property pos 'mistty-input-id)
      (setq pos (previous-single-property-change (min (1+ pos) (point-max)) 'mistty-input-id))
      (unless pos
        (throw 'mistty-return nil))
      (setq pos (1- pos)))
    ;; At pos, mistty-input-id is non-nil
    (setq pos (previous-single-property-change (min (1+ pos) (point-max)) 'mistty-input-id nil (point-min)))
    (mistty--prompt-ranges-at pos active-prompt-ranges)))

(defun mistty--prompt-ranges-at (start active-prompt-ranges)
  "Return ranges of a prompt starting at START.

If a prompt if found that starts at START, return (list START
output-start output-end), otherwise return nil.

ACTIVE-PROMPT-RANGES must be the output of
`mistty--active-or-potential-prompt-ranges'."
  (let ((limit (or (nth 0 active-prompt-ranges) (point-max))))
    (if (and (< start limit) (get-text-property start 'mistty-input-id))
        (let* ((prompt-end (next-single-property-change start 'mistty-input-id nil limit))
               (output-end (if (or (>= prompt-end limit)
                                   (get-text-property prompt-end 'mistty-input-id))
                               prompt-end
                             (next-single-property-change prompt-end 'mistty-input-id nil limit))))
          (list start prompt-end output-end))
      (if (equal start (nth 0 active-prompt-ranges))
          active-prompt-ranges
        (error "Invalid usage of mistty--prompt-ranges-at; no prompt starts at %s" start)))))

(defun mistty--active-or-potential-prompt-ranges ()
  "If a prompt is active, return (list prompt-start output-start output-end).

Also accepts inactive (potential) prompts."
  (when-let* ((prompt (or mistty--active-prompt (mistty--prompt)))
              (start-pos (mistty--scrolline-pos (mistty--prompt-start prompt)))
              (end-pos (or (when-let ((end (mistty--prompt-end prompt)))
                             (mistty--scrolline-pos end))
                           (point-max))))
    (list start-pos end-pos end-pos)))

(defun mistty--forward-prompt-ranges (accept)
  "Go through prompt ranges forward until ACCEPT return non-nil.

Return the prompt range that was accepted or nil."
  (let* ((active-prompt-ranges (mistty--active-or-potential-prompt-ranges))
         (prompt (mistty--prompt-ranges-around (point) active-prompt-ranges)))
    (catch 'mistty--accepted
      (unless prompt
        ;; If there's no prompt at point, look for a prompt after point.
        (when-let ((pos (next-single-property-change (point) 'mistty-input-id)))
            (setq prompt (mistty--prompt-ranges-around pos active-prompt-ranges))))
      (when (and prompt (funcall accept prompt))
        (throw 'mistty--accepted prompt))
      (while
          (setq prompt (when (and prompt (< (nth 2 prompt) (point-max)))
                         (mistty--prompt-ranges-at (nth 2 prompt) active-prompt-ranges)))
        (when (funcall accept prompt)
          (throw 'mistty--accepted prompt))))))

(defun mistty--backward-prompt-ranges (accept)
  "Go through prompt ranges backward until ACCEPT return non-nil.

Return the prompt range that was accepted or nil."
  (let* ((active-prompt-ranges (mistty--active-or-potential-prompt-ranges))
         (prompt (mistty--prompt-ranges-around (point) active-prompt-ranges)))
    (catch 'mistty--accepted
      (when (and prompt (funcall accept prompt))
        (throw 'mistty--accepted prompt))
      (while
          (setq prompt (when (and prompt (> (nth 0 prompt) (point-min)))
                         (mistty--prompt-ranges-around (1- (nth 0 prompt)) active-prompt-ranges)))
        (when (funcall accept prompt)
          (throw 'mistty--accepted prompt))))))

(defun mistty--pre-command ()
  "Function called from the `pre-command-hook' in `mistty-mode' buffers."
  (with-demoted-errors "mistty: pre-command error %S"
    (mistty--detect-foreign-overlays 'noschedule)
    (mistty--pre-command-for-undo)
    (when (and mistty--self-insert-line
               (not (eq this-command 'mistty-self-insert)))
      (setq mistty--self-insert-line nil))
    (setq mistty--old-point (point))))

(defun mistty--post-command ()
  "Function called from the `post-command-hook' in `mistty-mode' buffers."
  (with-demoted-errors "mistty: post-command error %S"
    (mistty--post-command-for-undo)

    (ignore-errors
      (when (and (or (eq this-command 'keyboard-quit)
                     (eq this-original-command 'keyboard-quit)))
        (when (not (mistty--queue-empty-p mistty--queue))
          (mistty-log "CANCEL")
          (message "MisTTY: Canceling replay")
          (mistty--cancel-queue mistty--queue))
        (mistty--ignore-foreign-overlays)
        (mistty--inhibit-clear 'noschedule)
        (when mistty--forbid-edit
          (mistty-send-key 1 "\C-g"))))

    (let ((point-moved (and mistty--old-point (/= (point) mistty--old-point))))
      ;; Show cursor again if the command moved the point.
      (when point-moved
        (mistty--show-cursor))

      (run-with-idle-timer
       0 nil #'mistty--post-command-1
       mistty-work-buffer point-moved))))

(defun mistty--inhibit-add (sym)
  "Add a source of inhibition with SYM as id.

This changes the value of `mistty--inhibit' and reacts to that
change."
  (let ((was-inhibited (if mistty--inhibit t nil)))
    (cl-pushnew sym mistty--inhibit)
    (unless was-inhibited
      (mistty-log "Long-running command ON %s" mistty--inhibit)
      (overlay-put mistty--sync-ov 'keymap nil)
      (mistty--update-mode-lines))))

(defun mistty--inhibit-remove (sym noschedule)
  "Remove a source of inhibition with SYM as id.

This changes the value of `mistty--inhibit' and reacts to that
change, unless NOSCHEDULE evaluates to true."
  (when mistty--inhibit
    (setq mistty--inhibit (delq sym mistty--inhibit))
    (unless mistty--inhibit
      (mistty-log "Long-running command OFF")
      (overlay-put mistty--sync-ov 'keymap (mistty--active-prompt-map))
      (mistty--update-mode-lines)
      (unless noschedule
        (run-with-idle-timer
         0 nil #'mistty--post-command-1
         mistty-work-buffer 'point-moved)))))

(defun mistty--inhibit-set (sym val &optional noschedule)
  "Add or remove a source of inhibition with SYM as id.

If VAL evaluates to non-nil, add it, otherwise remove it.

This changes the value of `mistty--inhibit' and reacts to that
change, unless NOSCHEDULE evaluates to true."
  (if val
      (mistty--inhibit-add sym)
    (mistty--inhibit-remove sym noschedule)))

(defun mistty--inhibit-clear (&optional noschedule)
  "Clear `mistty--inhibit'.

This changes the value of `mistty--inhibit' and reacts to that
change, unless NOSCHEDULE evaluates to true."
  (dolist (sym (cl-copy-list mistty--inhibit))
    (mistty--inhibit-remove sym noschedule)))

(defun mistty--detect-foreign-overlays (noschedule)
  "Detect foreign overlays.

Foreign overlays are used as a sign that a long-running command
is active and that normal MisTTY operations should be turned off
for a time.

If NOSCHEDULE evaluates to non-nil, don't schedule a
`mistty--post-command-1' when inhibition is turned off by this
call.

Does nothing if the option `mistty-detect-foreign-overlays' is
off."
  (when mistty-detect-foreign-overlays
    (let ((ovs
           (delq nil
                 (mapcar
                  (lambda (ov)
                    (unless (memq ov mistty--ignored-overlays)
                      (let ((ovprops (overlay-properties ov))
                            (match nil))
                        (dolist (prop mistty-foreign-overlay-properties)
                          (when (memq prop ovprops)
                            ;;(mistty-log "detected ov matching %s: %s" prop ovprops)
                            (setq match t)))
                        (if match
                            ov
                          ;;(mistty-log "ignored foreign overlay: %s" ovprops)
                          nil))))
                  (overlays-in mistty-sync-marker (point-max))))))
      ;; If there are foreign overlays in an empty buffer, there will
      ;; likely always be.
      (if (and ovs (= 1 (point-max)))
          (progn
            (mistty-log "Foreign overlays detected too early. Turning off detection.")
            (setq-local mistty-detect-foreign-overlays nil))
        (mistty--inhibit-set 'mistty-overlays ovs noschedule)))))

(defun mistty--ignore-foreign-overlays ()
  "Ignore currently active foreign overlays.

This function tells `mistty--detect-foreign-overlays' to ignore
whatever overlay currently exists."
  (dolist (ov (overlays-in mistty-sync-marker (point-max)))
    (cl-pushnew ov mistty--ignored-overlays)))

(defun mistty--detect-completion-in-region ()
  "Inhibit replay and refresh while completion is in progress."
  (mistty--inhibit-set
   'mistty-completion-in-region completion-in-region-mode))

(defun mistty--post-command-1 (buf point-moved)
  "Function called from `mistty--post-command'.

BUF is the buffer to work in. POINT-MOVED is non-nil if the point
was moved by the last command.

This is the body of `mistty--post-command', which replays any
modifications or cursor movement executed during the command. It
is run in an idle timer to avoid failures inside of the
post-command hook."
  (mistty--with-live-buffer buf
    (mistty--detect-foreign-overlays 'noschedule)
    (mistty--inhibit-undo
     (save-restriction
       (widen)
       (when (and (not mistty--inhibit)
                  (process-live-p mistty-proc)
                  (buffer-live-p mistty-term-buffer))
         (let ((cs (mistty--active-changeset))
               (replay nil))
           (when cs
             (if (setq replay (mistty--should-replay cs))
                 ;; Give changeset over to the interaction to replay.
                 (let ((last-interaction (mistty--queue-last-interact mistty--queue)))
                   (unless (and last-interaction
                                (eq 'replay (mistty--interact-type last-interaction))
                                ;; append to existing interaction
                                (mistty--call-interact last-interaction 'replay cs))
                     (mistty--enqueue mistty--queue (mistty--replay-interaction cs))))

               ;; Abandon changeset
               (mistty--release-changeset cs)
               (mistty--refresh-after-changeset)))

           (when (and (not replay) (not mistty--forbid-edit) point-moved (>= (point) mistty-sync-marker))
             (mistty--enqueue mistty--queue (mistty--cursor-to-point-interaction)))

           (mistty--refresh)))))))

(defun mistty--should-replay (cs)
  "Decide whether CS should be replayed.

Might modify CS before allowing replay."
  (let (replay)
    (cond
     ;; nothing to do
     ((not (mistty--changeset-p cs)))

     ;; modifications are part of the current prompt; replay them
     ((mistty-on-prompt-p (mistty-cursor))
      (setq replay t))

     ;; modifications are part of a possible prompt; realize it, keep
     ;; the modifications before the new prompt and replay the
     ;; modifications after the new prompt.
     ((and (mistty--possible-prompt-p)
           (mistty--changeset-restrict
            cs (mistty--scrolline-pos (mistty--prompt-start (mistty--prompt)))))
      (mistty--realize-possible-prompt)
      (setq replay t))

     ;; leave all modifications if there's enough of an unmodified
     ;; section at the end. moving the sync mark is only possible
     ;; as long as the term and work buffers haven't diverged.
     ((and (< (mistty--changeset-end cs)
              ;; modifiable limit
              (mistty--bol (point-max) -5))
           (not mistty--need-refresh))
      (let* ((pos (mistty--bol (mistty--changeset-end cs) 3))
             (scrolline (mistty--scrolline pos)))
        (mistty--set-sync-mark pos scrolline)))

     (t ;; revert everything

      ;; The following forces a call to refresh, in time, even if
      ;; the process sent nothing new.
      (mistty--needs-refresh)))

    replay))

(defun mistty--cursor-to-point-interaction ()
  "Build a `mistty--interact' to move the cursor to the point."
  (let ((interact (mistty--make-interact 'cursor-to-point)))
    (cl-labels
        ((can-move (from to)
           (and (mistty-on-prompt-p to)
                (not mistty--forbid-edit)
                (>= from (point-min))
                (<= from (point-max))
                (>= to (point-min))
                (<= to (point-max))
                (mistty--with-live-buffer mistty-term-buffer
                  (<= (mistty--from-pos-of to mistty-work-buffer)
                      (point-max)))))

         ;; Interaction entry point
         (start (&optional _)
           (let ((from (mistty-cursor))
                 (to (point)))
             (unless (can-move from to)
               (mistty--interact-done))
             (let ((distance (mistty--vertical-distance from to)))
               (let ((term-seq (mistty--move-vertically-str distance)))
                 (when (mistty--nonempty-str-p term-seq)
                   (mistty-log "cursor to point: %s -> %s lines: %s (can-move-vertically=%s)"
                               from to distance mistty--can-move-vertically)
                   (mistty--interact-send interact term-seq)
                   (mistty--interact-return-then
                    interact #'move-horizontally
                    :pred (let ((comparison (cond (mistty--can-move-vertically '=)
                                                  ((< distance 0) '<=)
                                                  (t '>=))))
                            (lambda ()
                              (funcall comparison 0 (mistty--vertical-distance
                                                     (mistty-cursor) (point)))))))
                 (move-horizontally)))))

         (move-horizontally (&optional _)
           (let ((from (mistty-cursor))
                 (to (point)))
             (unless (can-move from to)
               (mistty--interact-done))
             (let* ((distance (mistty--distance from to))
                    (term-seq (mistty--move-horizontally-str distance)))
               (when (mistty--nonempty-str-p term-seq)
                 (mistty-log "cursor to point: %s -> %s distance: %s" from to distance)
                 (mistty--interact-send interact term-seq)
                 (mistty--interact-return-then
                  interact
                  (lambda ()
                    (mistty-log "moved cursor to %s (goal: %s)"
                                (mistty-cursor) (point))
                    (mistty--interact-done))
                  :pred (lambda ()
                          ;; Ignoring skipped spaces is useful as, with
                          ;; multiline prompts, it's hard to figure out
                          ;; where the indentation should be without
                          ;; understanding the language.
                          (mistty--same-pos-ignoring-skipped
                           (mistty-cursor) (point))))))
             (mistty--interact-done))))
      (setf (mistty--interact-cb interact) #'start))

    interact))

(defun mistty--same-pos-ignoring-skipped (posa posb)
  "Return non-nil if POSA and POSB are the same, skipped spaces.

This returns non-nil if POSA and POSB are equal or if there are
only spaces with ==\'mistty-skip between them."
  (if (= posa posb)
      t
    (let ((low (min posa posb))
          (high (max posa posb)))
      (not (text-property-any low high 'mistty-skip nil)))))

(defun mistty-terminal-size-tracks-window ()
  "Set terminal size automatically from the size of the windows.

By default, MisTTY sets the terminal to a size that fits the windows the
buffer is currently displayed in.

This can be modified by calling `mistty-set-terminal-size', which see.
This function reverts to the default behavior."
  (interactive)
  (unless mistty-fullscreen
    (add-hook 'window-size-change-functions #'mistty--window-size-change nil t)
    (mistty--set-process-window-size-from-windows))
  (setq mistty--terminal-size nil))

(defun mistty--check-terminal-size (width height)
  "Make sure that WIDTH and HEIGHT are acceptable term sizes."
  (unless (and width height)
    (error "Specify both width and height"))
  (unless (>= width mistty-min-terminal-width)
    (error "Terminal width must be at least %s" mistty-min-terminal-width))
  (unless (>= height mistty-min-terminal-height)
    (error "Terminal height must be at least %s" mistty-min-terminal-height)))

(defun mistty-set-terminal-size (width height)
  "Set size of the terminal to WIDTH x HEIGHT outside of fullscreen.

By default, MisTTY sets the terminal to a size that fits the windows the
buffer is currently displayed in.

If you prefer, you can instead give the terminal a fixed dimension. This
might be useful if you're running program that don't check the size of
the terminal before displaying.

Note that this setting doesn't apply to the fullscreen mode. Terminal
size always matches window size when in fullscreen mode.

To go back to tracking window size, call
`mistty-terminal-size-tracks-window'."
  (interactive "nTerminal width: \nnTerminal height: ")
  (mistty--check-terminal-size width height)
  (unless mistty-fullscreen
    (remove-hook 'window-size-change-functions #'mistty--window-size-change t)
    (mistty--set-process-window-size width height))
  (setq mistty--terminal-size (cons width height)))

(defun mistty--window-size-change (win)
  "Update the process terminal size, reacting to WIN changing size."
  (when-let ((buffer (window-buffer win)))
    (with-current-buffer buffer
      (when (derived-mode-p 'mistty-mode)
        (mistty--set-process-window-size-from-windows)))))

(defun mistty--set-process-window-size-from-windows ()
  "Adjust process terminal size based on the windows displaying it."
  (when (process-live-p mistty-proc)
    (let* ((adjust-func (or (process-get mistty-proc 'adjust-window-size-function)
                            window-adjust-process-window-size-function))
           (size (funcall adjust-func mistty-proc
                          (append
                           (get-buffer-window-list mistty-work-buffer nil t)
                           (get-buffer-window-list mistty-term-buffer nil t)))))
      (when size
        (let ((width (car size))
              (height (cdr size)))
          (mistty-log "set-process-window-size %sx%s" width height)
          (mistty--set-process-window-size width height))))))

(defun mistty--set-process-window-size (width height)
  "Set the process terminal size to WIDTH x HEIGHT.

Width and height are limited to `mistty-min-terminal-width' and
`mistty-min-terminal-height'."
  (when (process-live-p mistty-proc)
    (let ((width (max width mistty-min-terminal-width))
          (height (max height mistty-min-terminal-height)))
      (mistty--with-live-buffer mistty-term-buffer
        (set-process-window-size mistty-proc height width)
        (term-reset-size height width)))))

(defun mistty--enter-fullscreen (proc)
  "Enter fullscreen mode for PROC."
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (mistty--detach)
    (let ((msg (mistty--fullscreen-message)))
      (save-excursion
        (goto-char (point-max))
        (insert msg)
        (message msg)))

    (let ((bufname (buffer-name)))
      (rename-buffer (generate-new-buffer-name (concat bufname " scrollback")))
      (with-current-buffer mistty-term-buffer
        (rename-buffer bufname)
        (jit-lock-mode t)
        (turn-on-font-lock)))
    (mistty--swap-buffer-in-windows mistty-work-buffer mistty-term-buffer)

    (setq mistty-fullscreen t)
    (mistty--with-live-buffer mistty-term-buffer
      (setq mistty-fullscreen t))

    (let ((accum (process-filter proc)))
      (mistty--accum-reset accum)
      (mistty--add-osc-detection accum)
      (mistty--add-skip-unsupported accum)
      (mistty--accum-add-processor
       accum
       '(seq CSI (or "47" "?47" "?1047" "?1049") ?l)
       (lambda (ctx str)
         (mistty--accum-ctx-push-down ctx str)
         (mistty--accum-ctx-flush ctx)
         (mistty--leave-fullscreen proc))))
    (set-process-sentinel proc #'mistty--fs-process-sentinel)
    (mistty--update-mode-lines proc)
    (mistty--set-process-window-size-from-windows)
    (run-hooks 'mistty-entered-fullscreen-hook)))

(defun mistty--fullscreen-message ()
  "Build a user message when entering fullscreen mode.

This function looks into the maps to find the key bindings for
`mistty-toggle-buffers' to include into the message."
  (let ((from-work (where-is-internal
                    #'mistty-toggle-buffers mistty-mode-map
                    'firstonly 'noindirect))
        (from-term (where-is-internal
                    #'mistty-toggle-buffers mistty-fullscreen-map
                    'firstonly 'noindirect))
        (keybinding-descr nil))
    (cond
     ((and from-work from-term (equal from-work from-term))
      (setq keybinding-descr
            (format "%s switches between terminal and scrollback buffers."
                    (key-description from-work))))
     ((and from-work from-term)
      (setq keybinding-descr
            (format "%s goes to terminal, %s to scrollback."
                    (key-description from-work)
                    (key-description from-term)))))
    (concat "Fullscreen mode ON"
            (when keybinding-descr ". ")
            keybinding-descr)))

(defun mistty--leave-fullscreen (proc)
  "Leave fullscreen mode for PROC."
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (save-restriction
      (widen)
      (setq mistty-fullscreen nil)
      (mistty--with-live-buffer mistty-term-buffer
        (setq mistty-fullscreen nil))

      (mistty--attach (process-buffer proc))
      (mistty--refresh)
      (when (and proc (process-live-p proc))
        (mistty-goto-cursor))

      (let ((bufname (buffer-name mistty-term-buffer)))
        (with-current-buffer mistty-term-buffer
          (rename-buffer (generate-new-buffer-name (concat " mistty tty " bufname))))
        (rename-buffer bufname))

      (mistty--swap-buffer-in-windows mistty-term-buffer mistty-work-buffer)
      (with-current-buffer mistty-term-buffer
        (font-lock-mode -1)
        (jit-lock-mode nil))

      (mistty--update-mode-lines proc)
      (run-hooks 'mistty-left-fullscreen-hook))))

(defun mistty--update-mode-lines (&optional proc)
  "Update the mode lines of the work and term buffers of PROC.

If PROC is not specified, use the value of `mistty-work-buffer'
and `mistty-term-buffer' to find the buffers.

Ignores buffers that don't exist."
  ;; work buffer modeline
  (mistty--with-live-buffer
      (or mistty-work-buffer
          (and proc (process-get proc 'mistty-work-buffer)))
    (cond
     (mistty-fullscreen
      (setq mode-line-process
            (propertize "scrollback"
                        'help-echo "mouse-1: Go to Term buffer"
                        'mouse-face 'mode-line-highlight
                        'local-map '(keymap
                                     (mode-line
                                      keymap
                                      (down-mouse-1 . mistty-toggle-buffers))))))
     (mistty-proc
      (setq mode-line-process
            (concat
             (cond
              (mistty--inhibit
               (propertize
                " CMD"
                'help-echo "Long-running command, mouse-1: ignore command and re-enable MisTTY replays"
                'mouse-face 'mode-line-highlight
                'local-map '(keymap
                             (mode-line
                              keymap
                              (down-mouse-1 . mistty-ignore-long-running-command)))))
              (mistty--forbid-edit
               (propertize
                " FE"
                'help-echo "Forbid Edit mode, see mouse-1: customize mistty-forbid-edit-regexps"
                'mouse-face 'mode-line-highlight
                'local-map '(keymap
                             (mode-line
                              keymap
                              (down-mouse-1 . (lambda ()
                                                (interactive)
                                                (customize-option 'mistty-forbid-edit-regexps))))))))
             (format ":%s" (process-status mistty-proc)))))
     (t
      (setq mode-line-process ":no process"))))
  ;; term buffer modeline
  (mistty--with-live-buffer
      (or mistty-term-buffer (and proc (process-buffer proc)))
    (cond
     (mistty-fullscreen
      (setq
       mode-line-process
       (concat
        (propertize
         "misTTY"
         'help-echo "mouse-1: Go to scrollback buffer"
         'mouse-face 'mode-line-highlight
         'local-map '(keymap
                      (mode-line
                       keymap
                       (down-mouse-1 . mistty-toggle-buffers))))
        ":%s")))
     (t
      (setq mode-line-process "misTTY:%s"))))
  (force-mode-line-update))

(defun mistty--swap-buffer-in-windows (a b)
  "Swap buffers A and B in windows.

This function keeps prev-buffers list unmodified."
  (walk-windows
   (lambda (win)
     (let ((prevs nil))
       (cond
        ((eq (window-buffer win) a)
         (setq prevs (cl-copy-list (window-prev-buffers win)))
         (set-window-buffer win b))
        ((eq (window-buffer win) b)
         (setq prevs (cl-copy-list (window-prev-buffers win)))
         (set-window-buffer win a)))
       (when prevs
         (set-window-prev-buffers win prevs))))))

(defun mistty-toggle-buffers ()
  "Toggle between the fullscreen buffer and the scrollback buffer."
  (interactive)
  (unless mistty-fullscreen
    (error "Not in fullscreen mode"))
  (let* ((from-buf (current-buffer))
         (to-buf (cond
                  ((eq from-buf mistty-work-buffer) mistty-term-buffer)
                  ((eq from-buf mistty-term-buffer) mistty-work-buffer))))
    (unless (buffer-live-p to-buf)
      (error "Buffer not available"))
    (switch-to-buffer to-buf)))

(defun mistty-sudo ()
  "Prepend sudo to the current command."
  (interactive)
  (mistty-send-string "\C-asudo \C-e"))

(defun mistty-on-prompt-p (pos)
  "Return non-nil if POS is on a prompt.

Prompts can span multiple lines; everything after the beginning of a
prompt is treated as being on a prompt, including completion options
displayed below the prompt as fish or zsh do."
  (and mistty-sync-marker
       mistty--active-prompt
       (>= pos mistty-sync-marker)))

(defun mistty-before-positional ()
  "Prepare the state for executing a positional command.

This creates any possible prompts that were detected at the last
minute. If a prompt is created, it also moves the cursor to the
point, as this is only done by the post-command hook on detected
prompts."
  (mistty--enqueue
   mistty--queue
   (mistty--interact before-position (interact)
     (let ((cursor (mistty-cursor)))
       (when (and (not (= cursor (point)))
                  (mistty-maybe-realize-possible-prompt))
         (mistty--enqueue
          mistty--queue
          (mistty--cursor-to-point-interaction)
          'prepend))
       (mistty--interact-done)))))

(defun mistty-maybe-realize-possible-prompt (&optional pos)
  "If a possible prompt was detected at POS, create it now."
  (let ((pos (or pos (point))))
    (when (and (not (mistty-on-prompt-p pos))
               (mistty--possible-prompt-p)
               (mistty--prompt-contains-pos (mistty--prompt) pos)
               (mistty--prompt-contains-pos (mistty--prompt) (mistty-cursor)))
      (mistty--realize-possible-prompt)
      t)))

(defun mistty--realize-possible-prompt ()
  "Realize a regexp prompt in `mistty--prompt'."
  (when-let* ((prompt (mistty--prompt))
              (scrolline (mistty--prompt-start prompt)))
    (when (mistty--maybe-move-sync-mark scrolline)
      (mistty-log "Realized %s prompt #%s [%s-] @%s"
                  (mistty--prompt-source prompt)
                  (mistty--prompt-input-id prompt)
                  (mistty--prompt-start prompt)
                  (marker-position mistty-sync-marker))
      (setq mistty--active-prompt prompt)
      (setf (mistty--prompt-realized prompt) t))))

(defun mistty--possible-prompt-p ()
  "Return non-nil if `mistty--possible-prompt' is usable."
  (when-let* ((prompt (mistty--prompt))
              (scrolline (mistty--prompt-start prompt))
              (start (mistty--scrolline-pos scrolline)))
    (when (and (eq 'regexp (mistty--prompt-source prompt))
               (not (mistty--prompt-realized prompt)))
      (let* ((content (mistty--prompt-text prompt))
             (length (length content))
             (end (+ start length))
             (cursor (mistty-cursor)))
        (and (or (> start mistty-sync-marker)
                 (and (= start mistty-sync-marker)
                      (not mistty--active-prompt)))
             (>= cursor end)
             (or (> cursor (point-max))
                 (<= cursor (mistty--bol start 2)))
             (string= content (mistty--safe-bufstring start end)))))))

(defun mistty--prompt-contains-pos (prompt pos)
  "Return non-nil if POS is inside PROMPT."
  (when-let* ((scrolline (mistty--prompt-start prompt))
              (start (mistty--scrolline-pos scrolline))
              (length (if-let ((text (mistty--prompt-text prompt)))
                          (length text)
                        0))
              (user-input-start (+ start length)))
    (and (>= pos user-input-start) (<= pos (mistty--eol start)))))

(defun mistty--create-backstage (proc)
  "Create a backstage buffer for PROC.

A backstage buffer is a partial copy of PROC's buffer that's kept
up-to-date with `replace-buffer-contents', so markers can be used
to keep positions stable while the buffer is being modified.

The value of the following buffer-local variables are carried
over to the backstage buffer: `mistty-log',
`mistty-bracketed-paste'."
  (let ((backstage (generate-new-buffer " *mistty-backstage" t))
        (calling-buffer (current-buffer)))
    (with-current-buffer backstage
      (setq-local mistty-sync-marker (point-min))
      (setq-local mistty-proc proc)
      (mistty--copy-buffer-local-variables
       '(mistty-bracketed-paste) calling-buffer)
      (mistty--update-backstage))
    backstage))

(defun mistty--update-backstage ()
  "Update backstage to catch up to its process output.

The current buffer must be a backstage buffer, created by
`mistty--create-backstage'.

The point is set to the equivalent of proc marker
position (cursor) in the buffer."
  (let ((buf (process-buffer mistty-proc)))
    (mistty--sync-buffer buf)
    (goto-char
     (mistty--from-pos-of (process-mark mistty-proc) buf))))

(defun mistty--delete-backstage (backstage)
  "Gets rid of a BACKSTAGE buffer."
  (when (buffer-live-p backstage)
    (kill-buffer backstage)))

(defun mistty--cursor-skip (win)
  "Move WIN point to skip fake spaces and newlines.

This function skips spaces marked with ==\'mistty-skip, depending
on the direction of the last move.

This is meant to be added to `pre-redisplay-functions'"
  (let (pos last-pos move-to)
    (when (and mistty-skip-empty-spaces
               ;; Never move point at cursor.
               (or (null mistty--cursor-after-last-refresh)
                   (not (equal (point) mistty--cursor-after-last-refresh)))
               (or (null mistty-proc)
                   (not (equal (point) (mistty-cursor))))
               (mistty-on-prompt-p (setq pos (window-point win))))
      (when-let ((last-state (window-parameter win 'mistty--cursor-skip-state)))
        (when (eq (car last-state) (current-buffer))
          (setq last-pos (cdr last-state))))
      (unless (equal pos last-pos)
        (pcase-dolist (`(,beg . ,end)
                       (mistty--cursor-skip-ranges
                        pos (lambda (type)
                              (memq type '(indent right-prompt continue-prompt empty-lines-at-eob dead)))))
          (unless move-to
            (setq
             move-to
             (cond
              ;; at the end of the range, which is also eob, move to beg
              ((and end (= end (point-max)))
               beg)

              ;; at a boundary, stay there
              ((or (null beg) (null end) (= pos beg) (= pos end)) nil)

              ;; horizontal movement from the left, go right
              ((and last-pos (<= last-pos beg) (mistty--same-line-p last-pos beg))
               end)
              ;; horizontal movement from the right, go left
              ((and last-pos (>= last-pos end) (mistty--same-line-p last-pos end))
               beg)
              ;; vertical move; on beg's line, so go to beg
              ((and (mistty--same-line-p pos beg) (not (mistty--same-line-p pos end)))
               beg)
              ;; vertical move; on end's line, so go to end
              ((and (mistty--same-line-p pos end) (not (mistty--same-line-p pos beg)))
               end)
              ;; closer to beg than to end, go to beg
              ((< (- pos beg) (- end pos))
               beg)
              ;; otherwise go to end
              (t
               end)))
            (when (equal move-to pos)
              (setq move-to nil))
            (when move-to
              (mistty-log "CURSOR MOVE beg %s end %s pos %s last-pos %s -> %s"
                          beg end pos last-pos move-to))))
        (when move-to
          (set-window-point win move-to))
        (set-window-parameter win 'mistty--cursor-skip-state
                              (cons (current-buffer) (or move-to pos)))))))

(defun mistty--cursor-skip-forward (pos)
  "Return the right end of the skip ranges containing POS.

Return POS if not in any skip ranges."
  (or (cdr (car (last (mistty--cursor-skip-ranges pos)))) pos))

(defun mistty--cursor-skip-ranges (pos &optional type-condition)
  "Ranges of spaces and newlines to skip over that contain POS.

Return a list of ( BEG . END ), sorted by BEG, increasing.

TYPE-CONDITION applies to mistty-skip types and defaults to skipping all
of them."
  (let ((beg (mistty--cursor-incomplete-skip-backward pos type-condition))
        (end (mistty--cursor-incomplete-skip-forward pos type-condition))
        (ranges nil))
    (save-excursion
      (goto-char beg)
      (while (and (> end (point))
                  (search-forward-regexp "\n *\n" end 'noerror))
        (let ((cursor (min (+ (match-beginning 0) 1
                              (mistty--line-indent beg))
                           (1- (match-end 0))))
              (actual-cursor (ignore-errors
                               (mistty-cursor))))
          (when (and actual-cursor
                     (> actual-cursor (match-beginning 0))
                     (< actual-cursor (match-end 0)))
            (setq cursor actual-cursor))
          (cond
           ((< pos cursor)
            (setq end cursor))
           ((> pos cursor)
            (setq beg cursor))
           (t ; (= pos cursor)
            (push (cons beg cursor) ranges)
            (setq beg cursor)))

          ;; The last \n can be the first \n of the next match.
          (goto-char (1- (match-end 0))))))
    (unless (= beg end)
      (push (cons beg end) ranges))
    (nreverse ranges)))

(defun mistty--line-indent (pos)
  "Return the intentation of the line at POS.

If the line contains a prompt, the indentation is the length of
the prompt from the beginning of the line."
  (save-excursion
    (goto-char (mistty--bol pos))
    (skip-chars-forward " ")))

(defun mistty--cursor-incomplete-skip-forward (pos type-condition)
  "Return the position of next non-skipped char after POS.

This implementation is incomplete. Always call
`mistty--cursor-skip-ranges' or `mistty--cursor-skip-forward' to
get correct results.

TYPE-CONDITION applies to mistty-skip types."
  (let ((stop nil)
        (type-condition (or type-condition #'identity)))
    (while (and (not stop)
                (< pos (point-max)))
      (cond
       ((and (eq ?\n (char-after pos))
             (or (and (> pos (point-min))
                      (funcall type-condition (get-text-property (1- pos) 'mistty-skip)))
                 (and (< pos (point-max))
                      (funcall type-condition (get-text-property (1+ pos) 'mistty-skip)))))
        (cl-incf pos))
       ((funcall type-condition (get-text-property pos 'mistty-skip))
        (setq pos (next-single-property-change
                   pos 'mistty-skip nil (point-max))))
       (t
        (setq stop t)))))
  pos)

(defun mistty--cursor-incomplete-skip-backward (pos type-condition)
  "Return the position of previous non-skipped char before POS.

This implementation is incomplete. Always call
`mistty--cursor-skip-ranges' to get correct results.

TYPE-CONDITION applies to mistty-skip types and defaults to skipping all
of them."
  (let ((stop nil)
        (type-condition (or type-condition #'identity)))
    (while (and (not stop) (> pos (point-min)))
      (cond
       ((and (eq ?\n (char-before pos))
             (or (funcall type-condition (get-text-property pos 'mistty-skip))
                 (and (>= (- pos 2) (point-min))
                      (funcall type-condition (get-text-property (- pos 2) 'mistty-skip)))))
        (cl-decf pos))
       ((funcall type-condition (get-text-property (1- pos) 'mistty-skip))
        (setq pos (previous-single-property-change
                   pos 'mistty-skip nil (point-min))))
       (t
        (setq stop t)))))
  pos)

(defun mistty--vertical-distance (beg end)
  "Compute the number up/down key presses to get from BEG to END.

Note that moving that distance vertically only gets the cursor to
the *line* on which END is."
  (mistty--count-lines beg end #'mistty--real-nl-p))

(defun mistty--distance (beg end)
  "Compute the number left/right key presses to get from BEG to END.

This function skips over the \\='term-line-wrap newlines as well
as \\='mistty-skip spaces."
  (let ((end (mistty--safe-pos (max beg end)))
        (sign (if (< end beg) -1 1))
        (pos (mistty--safe-pos (min beg end)))
        (count 0))
    (while (< pos end)
      (if (get-text-property pos 'term-line-wrap)
          ;; skip fake newlines
          (setq pos (1+ pos))
        (pcase-let ((`(,skip-beg . ,skip-end)
                     (car (last (mistty--cursor-skip-ranges pos)))))
          (cond
           ((or (null skip-beg) (null skip-end))
            ;; the position is not to be skipped over; count it towards
            ;; count.
            (setq pos (1+ pos))
            (setq count (1+ count)))
           ((= pos skip-end)
            (setq count (1+ count))
            (setq pos (1+ pos)))
           ((and (= pos skip-beg) (<= skip-end end))
            ;; we skipped a whole region; count it as 1 towards
            ;; count.
            (setq pos skip-end)
            (setq count (1+ count)))
           (t
            ;; we skipped a partial region; don't count it towards
            ;; count at all.
            (setq pos skip-end))))))
    (* sign count)))

(defun mistty--same-buffer-content-p (beg end)
  "Return non-nil if BEG to END is the same in both buffers.

BEG and END should be cons (WORK-POS . TERM-POS), with WORK-POS a
position on `mistty-work-buffer' and TERM-POS a position on
`mistty-term-buffer'. The text from the two regions is extracted
and compared, ignoring text properties."
  (equal
   (mistty--with-live-buffer mistty-work-buffer
     (mistty--safe-bufstring (car beg) (car end)))
   (mistty--with-live-buffer mistty-term-buffer
     (mistty--safe-bufstring (cdr beg) (cdr end)))))

(defun mistty-report-long-running-command (id active)
  "Report whether a specific long-running command is active.

MisTTY's replaying of modifications is turned off while a
long-running command is active, so the command can run without
interference. The result of the command is sent to the shell once
it's inactive.

MisTTY detects some long-running commands on its own by tracking
overlay, when `mistty-detect-foreign-overlays' is turned on. It
also detects completion in region by tracking
`completion-in-region-mode'.

Call this function when you'd like to use long-running command
that's not detected by MisTTY.

ID is a symbol that identifies the long-running command, so that
you can track and report several such commands without worrying
about interference. The same naming convention as you would for
functions to keep them unique.

If ACTIVE evaluates to true, report the long-running command as
active, which inhibits MisTTY's normal operation for the duration
of the command. If ACTIVE is nil, report the long-running command
as inactive, which turns MisTTY on again and triggers replaying
of any changes made in the terminal zone during that
command (unless a command with another ID is active; check with
`mistty-long-running-command-p')."
  (with-current-buffer mistty-work-buffer
    (mistty--inhibit-set id active)))

(defun mistty-long-running-command-p ()
  "Return non-nil if a long-running command is active.

See `mistty-report-long-running-command'."
  (with-current-buffer mistty-work-buffer
    (if mistty--inhibit t nil)))

(defun mistty-ignore-long-running-command ()
  "Ignore any long-running command currently active.

If MisTTY appears to be stuck, it might be because the end of
some long-running command hasn't been reported. This will
un-stuck MisTTY. It will not address the root cause of the
problem, however, which might be a bug in usage of
`mistty-report-long-running-command' or a bug in MisTTY. In the
later case, please file a bug report on
https://github.com/szermatt/mistty/issues"
  (interactive)
  (with-current-buffer mistty-work-buffer
    (if mistty--inhibit
        (progn
          (message "MisTTY: ignore long-running command %s" mistty--inhibit)
          (mistty--inhibit-clear))
      (error "MisTTY: no long-running command is active"))))

(defun mistty--wrap-capf-functions ()
  "Wrap the content `completion-at-point-functions' locally.

This function applies `mistty-capf-wrapper' to all function on
`completion-at-point-functions' in the current buffer.

This can be turned off by setting the option
`mistty-wrap-capf-functions' to nil."
  (when mistty-wrap-capf-functions
    (let ((capf-list (cl-copy-list completion-at-point-functions)))
      (setq-local completion-at-point-functions capf-list)
      (while capf-list
        (add-function :around (car capf-list) #'mistty-capf-wrapper)
        (setq capf-list (cdr capf-list))))))

(defun mistty-capf-wrapper (func &rest args)
  "Run FUNC with ARGS, with buffer narrowed to point.

This wrapper is meant to be applied to functions on
`completion-at-point-functions' to prevent them from seeing
autosuggestions. For details, see the option
`mistty-wrap-capf-functions'."
  (if (and (markerp mistty-sync-marker)
           (> (point) mistty-sync-marker))
      (save-restriction
        (narrow-to-region (point-min) (point))
        (apply func args))
    (apply func args)))

(defun mistty-kill-buffer (proc)
  "Kill MisTTY work buffer if PROC ended successfully.

This is meant to be added to `mistty-after-process-end-hook` if
you don't want a MisTTY buffer to remain after the shell ends. Be
sure that you never care for the buffer content after the shell
ends, as, in this case, it is simply lost.

Usage example:

  (add-hook \\='mistty-after-process-end-hook
            \\='mistty-kill-buffer)"
  (mistty--at-end proc 'kill-buffer))

(defun mistty-kill-buffer-and-window (proc)
  "Kill MisTTY work buffer and window if PROC ended successfully.

This is meant to be added to `mistty-after-process-end-hook` if
you don't want a MisTTY buffer and window to remain after the
shell ends. Be sure that you never care for the buffer content
after the shell ends, as, in this case, it is simply lost.

Usage example:

  (add-hook \\='mistty-after-process-end-hook
            \\='mistty-kill-buffer-and-window)"
  (mistty--at-end proc 'kill-buffer-and-window))

(defun mistty--at-end (proc &optional option-override)
  "Consider whether buffer and window should be kill.

This is called by `mistty-after-process-end-hook' and to apply the value
of option `mistty-after-end'.

PROC is the process that just ended.

If OPTION-OVERRIDE is non-nil, use that instead of the value of
`mistty-at-end'."
  (let ((option (or option-override mistty-at-end)))
    (when (and (memq option '(kill-buffer kill-buffer-and-window))
               (eq 'exit (process-status proc))
               (zerop (process-exit-status proc))
               mistty--interacted)
      (let ((buf (current-buffer))
            (win (get-buffer-window)))
        (cond
         ((eq option 'kill-buffer) (kill-buffer buf))
         ((eq option 'kill-buffer-and-window)
          (if (and (kill-buffer buf)
                   (window-parent win))
              (ignore-errors (delete-window)))))))))

(defun mistty--active-prompt-map ()
  "Return the map active in the synced region."
  (cond
   (mistty--inhibit nil)
   (mistty--forbid-edit mistty-forbid-edit-map)
   (t mistty-prompt-map)))

(defun mistty-new-buffer-name ()
  "Generate a name for a new MisTTY buffer.

This function interprets the customization option
`mistty-buffer-name' within the current context and returns the
result."
  (with-connection-local-variables
   (concat "*"
           (mapconcat
            (lambda (part)
              (if (functionp part)
                 (funcall part)
                part))
            mistty-buffer-name)
           "*")))

(defun mistty-buffer-name-shell ()
  "Return the short name of the shell to be run.

For example, if the command that's run is \"/usr/bin/bash\", this
function returns \"bash\".

This function is meant to be used in the configuration option
`mistty-buffer-name'. It relies on `mistty-shell-command' being
set by `mistty-create' in the environment the function is called."
  (file-name-sans-extension
   (file-name-nondirectory (if (consp mistty-shell-command)
                               (car mistty-shell-command)
                             mistty-shell-command))))

(defun mistty-buffer-name-user ()
  "TRAMP user, if not the current user, as \"-<user>\".

This function is meant to be used in the configuration option
`mistty-buffer-name'."
  (when-let ((remote-user (file-remote-p default-directory 'user)))
    (unless (string= (getenv "USER") remote-user)
      (concat "-" remote-user))))

(defun mistty-buffer-name-host ()
  "TRAMP host, if not localhost, as \"@<host>\".

This function is meant to be used in the configuration option
`mistty-buffer-name'."
  (when-let ((remote-host (file-remote-p default-directory 'host)))
    (unless (string= remote-host (system-name))
      (concat "@" remote-host))))

(defun mistty--realign-buffers()
  "Realign the work and terminal buffers, if possible."
  (mistty--require-term-buffer)
  (let ((term-top (mistty--term-scrolline-at-screen-start)))
    (mistty--with-live-buffer mistty-work-buffer
      (let ((prop (save-excursion
                    (goto-char mistty-sync-marker)
                    (text-property-search-backward
                     'mistty-scrolline
                     term-top ;; goal
                     (lambda (goal val)
                       (and val (<= val goal)))))))
        (cond
         ;; Found exact scrolline, align there or below if rows match
         ((and prop (= term-top (prop-match-value prop)))
          (let ((sync-pos (prop-match-beginning prop)))
            (pcase-setq `(,sync-pos . ,term-top)
                        (mistty--skip-identical-rows sync-pos term-top))
            (mistty-log "REALIGN scrolline %s to pos %s; terminal [%s-]"
                        term-top sync-pos term-top)
            (mistty--set-sync-mark sync-pos term-top)))

         ;; Found scrolline < goal, align at line after
         (prop
          (let ((sync-pos (mistty--bol (prop-match-beginning prop) 2)))
            (mistty-log "REALIGN APPROXIMATE scrolline %s to pos %s; terminal [%s-]"
                        term-top sync-pos term-top)
            (mistty--set-sync-mark sync-pos term-top)))

         ;; Couldn't find beginning. It might have been deleted. Sync
         ;; whole buffer.
         (t
          (mistty-log "REALIGN FALLBACK scrolline %s to point-min %s"
                      term-top (point-min))
          (mistty--set-sync-mark (point-min) term-top)))))))

(defun mistty--skip-identical-rows (pos scrolline)
  "Skip rows from POS to `mistty-sync-marker' that are the same on the term buffer.

POS is a position in the work buffer before `mistty-sync-marker' that is
believed to correspond to SCROLLINE.

Return a (CONS new-pos new-scrolline), possibly modified values for POS
and SCROLLINE."
  (mistty--with-live-buffer mistty-work-buffer
    (while
        (when (equal scrolline (get-text-property pos 'mistty-scrolline))
          (when-let ((end-row (next-single-property-change pos 'mistty-scrolline)))
            (when (string= (string-trim (buffer-substring-no-properties pos end-row)
                                        "" "\n")
                           (mistty--with-live-buffer mistty-term-buffer
                             (save-excursion
                               (goto-char (mistty--term-scrolline-pos scrolline))
                               (mistty--current-scrolline-text 'no-properties))))
              (setq pos end-row))))
      (goto-char pos)
      (cl-incf scrolline)))
  `(,pos . ,scrolline))

(defun mistty-beginning-of-defun (&optional n)
  "Go to the beginning of the N'th defun.

This is meant to be set as `beginning-of-defun-function' in MisTTY
buffers."
  (let ((n (or n 1)))
    (let ((accept (let ((accepted-count 0)
                        (abs-n (abs n)))
                    (lambda (prompt)
                      (unless (and (eq this-command 'beginning-of-defun)
                                   (= (point) (nth 0 prompt)))
                        (cl-incf accepted-count))
                      (>= accepted-count abs-n)))))
      (if (>= n 0)
          (when-let ((prompt (mistty--backward-prompt-ranges accept)))
            (prog1 t
              (goto-char (nth 0 prompt))))
        (when-let ((prompt (mistty--forward-prompt-ranges accept)))
          (prog1 t
            (goto-char (nth 2 prompt))))))))

(defun mistty-end-of-defun ()
  "Go to the end of the current defun.

This function assumes that the point is at the beginning of a defun, set
by `mistty-beginning-of-defun'.

This is meant to be set as `end-of-defun-function' in MisTTY buffers."
  (let ((active-prompt-ranges (mistty--active-or-potential-prompt-ranges)))
    (when-let ((ranges (mistty--prompt-ranges-at (point) active-prompt-ranges)))
      (prog1 t
        (goto-char (nth 2 ranges))))))

(defun mistty-imenu-create-index ()
  "Create an imenu index of command-lines and outputs.

This is meant to be bound to `imenu-create-index-function'."
  (let ((active-prompt (mistty--active-or-potential-prompt-ranges))
        (maybe-marker (lambda (pos)
                        (if imenu-use-markers
                            (copy-marker pos)
                          pos)))
        output-alist prompt-alist)
    (save-excursion
      (goto-char (point-max))
      (mistty--backward-prompt-ranges
       (lambda (prompt)
         (prog1 nil ;; keep looking
           (if (equal prompt active-prompt)
               ;; Active prompts
               (push (cons "CURRENT"
                           (funcall maybe-marker
                                    (mistty--user-input-start prompt)))
                     prompt-alist)

             ;; Previous prompts
             (when-let ((command (mistty--command-for-output prompt)))
               (when (> (nth 2 prompt) (nth 1 prompt))
                 (push (cons command
                             (funcall maybe-marker (nth 1 prompt)))
                       output-alist))
               (when (> (nth 1 prompt) (nth 0 prompt))
                 (push (cons command
                             (funcall maybe-marker
                                      (mistty--user-input-start prompt)))
                       prompt-alist)))))))
      `(("Outputs" . ,(nreverse output-alist))
        ("Commands" . ,(nreverse prompt-alist))))))

(provide 'mistty)

;;; mistty.el ends here
