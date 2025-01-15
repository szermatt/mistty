;;; mistty.el --- Shell/Comint alternative based on term.el -*- lexical-binding: t -*-

;; Copyright (C) 2023-2024 Stephane Zermatten

;; Author: Stephane Zermatten <szermatt@gmx.net>
;; Version: 1.3.1snapshot
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
(eval-when-compile
  (require 'files-x) ; with-connection-local-variables
  (require 'minibuffer))

(require 'mistty-changeset)
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

(defconst mistty--prompt-regexp
  "[^[:alnum:][:cntrl:][:blank:]][[:blank:]]$"
  "Regexp used to identify prompts.

New, empty lines that might be prompts are evaluated against this
regexp. This regexp should match something that looks like the
end of a prompt with no commands.")

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

  ;; While in a shell, when bracketed paste is on, this allows
  ;; sending a newline that won't submit the current command. This
  ;; is handy for editing multi-line commands in bash.
  "S-<return>" #'newline

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

(defvar-local mistty--has-active-prompt nil
  "Non-nil there is a prompt at `mistty-sync-marker'.")

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

(defvar-local mistty--refresh-timer nil
  "A timer for calling refresh after some delay.

This is used to cover the case where modifications that should
cause changes are just ignored by the command.")

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

Normally, fake newlines are removed by mistty--set-sync-mark-,
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
  (setq-local window-point-insertion-type t)

  ;; Mistty hides the fake newlines term added and relies on large
  ;; lines being wrapped for display and proper resizing.
  (setq-local truncate-lines nil)

  (add-hook 'pre-redisplay-functions #'mistty--cursor-skip nil t)
  (add-hook 'completion-in-region-mode-hook #'mistty--detect-completion-in-region nil t)

  (setq mistty-sync-marker (point-max-marker))
  (setq mistty--sync-ov (make-overlay mistty-sync-marker (point-max) nil nil 'rear-advance))
  (setq mistty--ignored-overlays (list mistty--sync-ov))

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
  (unless (eq mistty-work-buffer (current-buffer)) (error "Work buffer required")))

(defsubst mistty--require-term-buffer ()
  "Asserts that the current buffer is the term buffer."
  (unless (eq mistty-term-buffer (current-buffer)) (error "Term buffer required")))

(defsubst mistty--require-proc ()
  "Asserts that the current buffer has a live process."
  (unless (process-live-p mistty-proc) (error "No running process")))

(defun mistty--exec (program)
  "Execute PROGRAM in the current buffer.

PROGRAM can be either a string or a list. If it is a string, it
should be the name of an executable to run, without arguments. If
it is a string, it should be a list of executable and its
arguments."
  (unless (derived-mode-p 'mistty-mode)
    (error "Not a mistty-mode buffer"))
  (let ((win (or (get-buffer-window (current-buffer)) (selected-window)))
        (command (if (consp program) (car program) program))
        (args (if (consp program) (cdr program) nil)))
    (mistty--attach
     (mistty--create-term
      (concat " mistty tty " (buffer-name)) command args
      ;; local-map
      mistty-fullscreen-map
      ;; width
      (- (window-max-chars-per-line win) left-margin-width)
      ;;height
      (floor (with-selected-window win (window-screen-lines))))))
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
    (setq mistty--queue (mistty--make-queue proc))

    (with-current-buffer term-buffer
      (setq mistty-proc proc)
      (setq mistty-work-buffer work-buffer)
      (setq mistty-term-buffer term-buffer)
      (unless mistty-sync-marker
        (setq mistty-sync-marker (copy-marker term-home-marker))))

    (when proc
      (set-process-filter proc #'mistty--process-filter)
      (set-process-sentinel proc #'mistty--process-sentinel))

    (add-hook 'kill-buffer-hook #'mistty--kill-term-buffer nil t)
    (add-hook 'window-size-change-functions #'mistty--window-size-change nil t)
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)
    (add-hook 'before-change-functions #'mistty--before-change-on-work nil t)
    (add-hook 'pre-command-hook #'mistty--pre-command nil t)
    (add-hook 'post-command-hook #'mistty--post-command nil t)))

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
  (remove-hook 'window-size-change-functions #'mistty--window-size-change t)
  (remove-hook 'after-change-functions #'mistty--after-change-on-work t)
  (remove-hook 'before-change-functions #'mistty--before-change-on-work t)
  (remove-hook 'pre-command-hook #'mistty--pre-command t)
  (remove-hook 'post-command-hook #'mistty--post-command t)

  (when mistty--queue
    (mistty--cancel-queue mistty--queue)
    (setq mistty--queue nil))
  (when mistty-proc
    (set-process-filter mistty-proc #'term-emulate-terminal)
    (set-process-sentinel mistty-proc #'term-sentinel)
    (setq mistty-proc nil)))

(defun mistty--kill-term-buffer ()
  "Kill-buffer-hook handler for `mistty-term-buffer'."
  (let ((term-buffer mistty-term-buffer))
    (when (buffer-live-p mistty-work-buffer) ;; might be nil
      (mistty--detach))
    (mistty--update-mode-lines)
    (when (buffer-live-p term-buffer)
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
    (with-current-buffer buf (mistty--exec command))
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

The hooks are called from inside BUF. Since these hooks tend to kill
their buffer, make sure that running stops once the buffer is killed."
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
      (mistty--leave-fullscreen proc "")
      (mistty--process-sentinel proc msg))
     ((and process-dead (not (buffer-live-p term-buffer)) (buffer-live-p work-buffer))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer (process-get proc 'mistty-work-buffer)))
      (term-sentinel proc msg)
      (mistty--run-after-process-end-hooks work-buffer proc))
     (t (term-sentinel proc msg)))))

(defun mistty--process-filter (proc str)
  "Process filter for MisTTY shell processes.

PROC is the calling shell process and STR the string it sent."
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
     ((or (string-match "\ec" str)
          (string-match "\e\\[H\e[[02]?J" str))
      (let ((rs1-before-pos (match-beginning 0))
            (rs1-after-pos (match-end 0))
            (home-pos))
        ;; The work buffer must be updated before sending the reset to
        ;; the terminal, or we'll lose data. This might interfere with
        ;; collecting and applying modifications, but then so would
        ;; reset.
        (mistty--with-live-buffer term-buffer
          (mistty--process-terminal-seq proc (substring str 0 rs1-before-pos)))
        (mistty-log "RESET")
        (mistty--with-live-buffer work-buffer
          (setq mistty--need-refresh t)
          (mistty--cancel-queue mistty--queue) ; might call mistty--refresh
          (when mistty--need-refresh
            (mistty--refresh))
          (setq home-pos (point)))
        (mistty--reset-markers)
        (mistty--with-live-buffer term-buffer
          (mistty--process-terminal-seq proc (substring str rs1-before-pos rs1-after-pos)))
        (mistty--with-live-buffer work-buffer
          (setq mistty-bracketed-paste nil))
        (mistty--process-filter proc (substring str rs1-after-pos))
        ;; After clear or reset, scroll the main window so the region
        ;; that was cleared is not visible anymore, so it looks like
        ;; the buffer was cleared even though history is kept.
        (mistty--with-live-buffer work-buffer
          (when-let (win (get-buffer-window work-buffer))
            (with-selected-window win
              (set-window-start win (mistty--bol home-pos) 'noforce))))))

     ;; normal processing
     (t
      (mistty-log "RECV[%s]" str)
      (mistty--with-live-buffer term-buffer
        (mistty--process-terminal-seq proc str))
      (mistty--with-live-buffer work-buffer
        (mistty--copy-buffer-local-variables
         mistty-variables-to-copy term-buffer)

        (mistty--cancel-timeout mistty--queue)

        (let ((old-point-max (point-max)))
          (mistty--refresh)

          ;; If there's something below the point in a prompt, scroll
          ;; the window up so it's visible. Emacs won't do it on its
          ;; own, since the point would still be visible in such a case.
          (when (and (> (point-max) old-point-max)
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

        (mistty--maybe-truncate-when-idle)
        (mistty--dequeue mistty--queue 'intermediate)
        (mistty--dequeue-with-timer mistty--queue 'stable))))))

(defun mistty--process-terminal-seq (proc str)
  "Process STR, sent to PROC, then update MisTTY internal state."
  (mistty--require-term-buffer)
  (let ((old-sync-position (marker-position mistty-sync-marker))
        (old-last-non-ws (mistty--last-non-ws)))
    (mistty--emulate-terminal proc str mistty-work-buffer)
    (goto-char (process-mark proc))
    (when (/= mistty-sync-marker old-sync-position)
      (mistty-log "RESET; unexpected change")
      (pcase-let ((`(,work-sync . ,term-sync) (mistty--align-buffers)))
        (if (or work-sync term-sync)
            (mistty-log "ALIGN %s <-> %s" work-sync term-sync)
          (mistty-log "ALIGN failed; fallback to default"))
        (mistty--reset-markers work-sync term-sync)))
    (when (> (mistty--bol (point)) old-last-non-ws) ;; on a new line
      (mistty--detect-possible-prompt (point)))))

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

(defun mistty--detect-possible-prompt (cursor)
  "Look for a new prompt at CURSOR and store its position.

This function updates `mistty--possible-prompt' after the content
of the terminal buffer has been updated."
  (mistty--require-term-buffer)
  (let* ((bol (mistty--bol cursor)))
    (when (and (> cursor bol)
               (>= cursor (mistty--last-non-ws))
               (string-match
                mistty--prompt-regexp
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

(defun mistty--reset-markers (&optional work-sync term-sync)
  "Reset the sync marker on both the term and work buffer.

If specified WORK-SYNC is the new position of sync mark in the
work buffer, and TERM-SYNC the corresponding mark in the term
buffer.

This function should be called to fix a situation where the
markers have gone out-of-sync."
  (let ((old-work-sync (mistty--with-live-buffer mistty-work-buffer (marker-position mistty-sync-marker)))
        (old-term-sync (mistty--with-live-buffer mistty-term-buffer (marker-position mistty-sync-marker)))
        (work-sync (or work-sync
                       (mistty--with-live-buffer mistty-work-buffer
                         (let ((inhibit-read-only t)
                               (inhibit-modification-hooks t))
                           (delete-region (mistty--last-non-ws) (point-max))
                           (insert "\n")
                           (point-max)))))
        (term-sync (or term-sync
                       (mistty--with-live-buffer mistty-term-buffer
                         (save-excursion
                           (goto-char term-home-marker)
                           (skip-chars-forward mistty--ws)
                           (point))))))
    (mistty--with-live-buffer mistty-work-buffer
      (setq mistty--end-prompt nil)
      (setq mistty--cursor-after-last-refresh nil)
      (move-marker mistty-sync-marker work-sync))
    (mistty--with-live-buffer mistty-term-buffer
      (move-marker mistty-sync-marker term-sync))
    (mistty-log "RESET SYNC MARKER %s/%s to %s/%s term home: %s"
                old-work-sync old-term-sync
                work-sync term-sync
                (mistty--with-live-buffer mistty-term-buffer
                  (marker-position term-home-marker)))))

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
  ;; Not using mistty--require-proc as a non-live process is
  ;; acceptable here.
  (unless mistty-proc
    (error "No process"))
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
  (if (or mistty--inhibit-refresh mistty--inhibit)
      (setq mistty--need-refresh t)
    (let ((inhibit-modification-hooks t)
          (inhibit-read-only t)
          (point-was-at-cursor (or (null mistty--cursor-after-last-refresh)
                                   (= (point) mistty--cursor-after-last-refresh)))
          on-prompt)
      (mistty--inhibit-undo
       (save-restriction
         (widen)
         (setq mistty--need-refresh nil)
         (when (timerp mistty--refresh-timer)
           (cancel-timer mistty--refresh-timer)
           (setq mistty--refresh-timer nil))

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
             (add-text-properties pos (point-max) '(mistty-skip empty-lines-at-eob))))

         ;; Right after a mistty-send-command, we're waiting for a line
         ;; after mistty--end-prompt that's not part of the old prompt.
         (when (and mistty--end-prompt
                    (< mistty-sync-marker mistty--end-prompt (point-max)))
           (when-let ((command-end
                       (if (get-text-property mistty--end-prompt 'mistty-input-id)
                           (next-single-property-change mistty-sync-marker 'mistty-input-id nil)
                         (mistty--bol mistty--end-prompt 2))))
             ;; Some versions of ZSH end the prompt just before the
             ;; newline instead of just after. Extend the input-id
             ;; region if necessary to account for that.
             (when (and (not (eq ?\n (char-before command-end)))
                        (eq ?\n (char-after command-end)))
               (cl-incf command-end)
               (when-let ((input-id (get-text-property mistty--end-prompt 'mistty-input-id)))
                 (put-text-property (1- command-end) command-end 'mistty-input-id input-id)))
             (when (and (eq ?\n (char-before command-end))
                        (> (mistty--last-non-ws) command-end)
                        (> command-end mistty-sync-marker))
               (unless (get-text-property command-end 'mistty-input-id)
                 (mistty-log "End prompt. Mark input range: [%s-%s]"
                             (marker-position mistty-sync-marker) command-end)
                 (put-text-property mistty-sync-marker command-end
                                    'mistty-input-id (mistty--next-id)))
               (mistty--set-sync-mark-from-end command-end))))

         ;; detect prompt from bracketed-past region and use that to
         ;; restrict the sync region.
         (when (process-live-p mistty-proc)
           (let* ((cursor (mistty-cursor))
                  (prompt-beg
                   (let ((pos cursor))
                     (unless (and (> pos (point-min))
                                  (get-text-property (1- pos) 'mistty-input-id))
                       (setq pos (previous-single-property-change
                                  pos 'mistty-input-id nil mistty-sync-marker)))
                     (when (and (> pos (point-min))
                                (get-text-property (1- pos) 'mistty-input-id))
                       (setq pos (previous-single-property-change
                                  pos 'mistty-input-id nil mistty-sync-marker)))
                     (when (and (>= pos (point-min)) (< pos (point-max)))
                       pos))))
             (when (and prompt-beg
                        (get-text-property prompt-beg 'mistty-input-id)
                        (or (> prompt-beg mistty-sync-marker)
                            (and (= prompt-beg mistty-sync-marker)
                                 (not mistty--has-active-prompt)))
                        (< prompt-beg cursor)
                        (string-match
                         mistty--prompt-regexp
                         (mistty--safe-bufstring
                          (mistty--bol cursor) cursor)))
               (mistty-log "Detected prompt: [%s-%s]" prompt-beg cursor)
               (mistty--set-sync-mark-from-end prompt-beg)
               (setq mistty--has-active-prompt (> cursor prompt-beg)))))

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
         (when (process-live-p mistty-proc)
           (when (and (not (eq mistty-goto-cursor-next-time 'off))
                      (or mistty-goto-cursor-next-time point-was-at-cursor))
             (mistty-goto-cursor))
           (unless mistty--cursor-after-last-refresh
             (setq mistty--cursor-after-last-refresh (make-marker)))
           (move-marker mistty--cursor-after-last-refresh (mistty-cursor)))
         (setq mistty-goto-cursor-next-time nil))))

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
  "Check whether vertical moves are allowed, return either nil or t.

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
    ;; In most cases, the solution to deal with a buffer being
    ;; truncated is to use markers, but it won't work with
    ;; possible-prompt, as it often points to locations that
    ;; don't yet exist in the work buffer.
    (when mistty--possible-prompt
      (let ((diff (- cutoff (point-min))))
        (cl-decf (nth 0 mistty--possible-prompt) diff)
        (cl-decf (nth 1 mistty--possible-prompt) diff)))
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

(defun mistty--set-sync-mark-from-end (sync-pos)
  "Set the sync marker to SYNC-POS, assuming buffer ends are the same.

This function sets the `mistty-sync-marker' on both the term and
the work buffer.

SYNC-POS is a position on the current buffer, which might be
either the work or the term buffer.

For this to work, the term and work buffer starting with SYNC-POS
must have the same content, which is only true when SYNC-POS is
bigger than `mistty-sync-marker' and `mistty--refresh' was
called recently enough."
  (let ((chars-from-end (- (point-max) sync-pos)))
    (with-current-buffer mistty-term-buffer
      (move-marker mistty-sync-marker (- (point-max) chars-from-end)))
    (mistty--with-live-buffer mistty-work-buffer
      (mistty--set-work-sync-mark (- (point-max) chars-from-end)))))

(defun mistty--move-sync-mark-with-shift (sync-pos shift)
  "Move the sync marker on the work buffer to SYNC-POS.

SYNC-POS must be a position on the work buffer, which must be the
current buffer.

SHIFT specifies the current difference between the sync marker on
the work buffer and the term buffer. The shift value comes
from `mistty--after-change-on-work' tracking the changes."
  (mistty--require-work-buffer)
  (let ((diff (- sync-pos mistty-sync-marker)))
    (with-current-buffer mistty-term-buffer
      (move-marker mistty-sync-marker (+ mistty-sync-marker diff shift))))
  (mistty--set-work-sync-mark sync-pos))

(defun mistty--set-work-sync-mark (sync-pos)
  "Set sync mark to SYNC-POS in the work buffer and init some vars.

The caller is responsible for setting the sync mark on the term
buffer.

In most cases, this is the wrong function to call. Consider
instead `mistty--move-sync-mark-with-shift' or
`mistty--set-sync-mark-from-end'."
  (mistty--require-work-buffer)
  (unless (= sync-pos mistty-sync-marker)
    (mistty-log "MOVE SYNC MARKER %s to %s"
                (marker-position mistty-sync-marker)
                sync-pos)
    (setq mistty--has-active-prompt nil)
    (setq mistty--end-prompt nil)
    (let ((old-marker-position (marker-position mistty-sync-marker)))
      (move-marker mistty-sync-marker sync-pos)
      (when (< old-marker-position sync-pos)
        (mistty--prepare-for-scrollback old-marker-position mistty-sync-marker)))
    (move-overlay mistty--sync-ov mistty-sync-marker (point-max))))

(defun mistty--prepare-for-scrollback (beg end)
  "Transition a region from the terminal to the scrollback zone.

This function modifies the region from BEG to END as appropriate
for a scrollback area that's not going to be refreshed from the
terminal anymore.

It removes the fake newlines which are not useful anymore and
just tend to cause issues."
  (when (not mistty--inhibit-fake-nl-cleanup)
    (mistty--remove-fake-newlines beg end)))

(defun mistty--last-non-ws ()
  "Return the position of the last non-whitespace in the buffer."
  (save-excursion
    (goto-char (point-max))
    (skip-chars-backward mistty--ws)
    (point)))

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
     (mistty--interact-return
      interact "\C-m"
      :then (lambda (&optional _)
              (setq mistty--interacted t)
              (mistty--interact-done))))))

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
      (mistty-send-key n "\x7f" 'positional))))

(defun mistty-delete-char (&optional n)
"Send Control D N times to the terminal.

If N is unset, send Control d once. If N is negative, send DEL
that many times instead."
  (interactive "p")
  (if mistty--inhibit
      (delete-char n)
    (setq mistty--interacted t)
    (if (and (numberp n) (< n 0))
        (mistty-send-key (abs n) "\x7f" 'positional)
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
           (mistty--interact-return
            interact (if fire-and-forget
                         `(fire-and-forget ,translated-key)
                       translated-key)
            :then #'mistty--interact-done)))))

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
                 (mistty--interact-return
                  interact "\C-a"
                  :then #'mistty--interact-done))
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
               (mistty--interact-return
                interact "\C-e"
                :then #'mistty--interact-done))
           (end-of-line n)
           (mistty--interact-done)))))
     (t
      (end-of-line n)))))

(defun mistty--before-change-on-work (beg end)
  "Handler called before making modifications to the work buffer.

This is meant to be added to ==\'before-change-functions before
changing the region BEG to END.

This function deletes the right prompt just before a change, so
it won't look too strange when something is inserted that would
otherwise shift the prompt right into a new line. This is
especially visible and confusing during a long-running command.

Right prompts appearing at the end of inserted text with newlines
also confuse `replace-buffer-content', which then sets the point
at the wrong position after a refresh."
  (when (and mistty-sync-marker (>= end mistty-sync-marker))
    (when-let ((right-prompt
                (text-property-any
                 beg (mistty--eol end) 'mistty-skip 'right-prompt)))
      (delete-region right-prompt (mistty--eol right-prompt)))))

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

Replay interactions are callable with `mistty--call-interact'.
They take a single argument, another changeset, and attempt to
append that changeset to the current one. If that works, the
changeset is released and the call returns t, otherwise the call
returns nil."
  (let ((interact (mistty--make-interact 'replay))
        next-modification-f
        move-to-target-f after-move-vertically-f after-move-to-target-f
        insert-and-delete-f after-insert-and-delete-f

        ;; mistty--changeset-modification extracts modification from
        ;; the buffer. It must be called when the interaction is
        ;; created, not when it is run
        (modifications (mistty--changeset-modifications cs))
        (calling-buffer (current-buffer))
        (inhibit-moves mistty--forbid-edit)
        (beg (make-marker))
        (old-end (make-marker))
        target
        backstage lower-limit upper-limit distance
        orig-beg content old-length waiting-for-last-change
        inserted-detector-regexp)

    (setf
     (mistty--interact-cb interact)
     (lambda (&optional _)
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
       (funcall next-modification-f)))

    (setq
     next-modification-f
     (lambda ()
       ;; No more modifications.
       (unless modifications
         (set-buffer calling-buffer)

         ;; Force refresh, even if nothing was sent, if only to revert what
         ;; couldn't be replayed.
         (setq mistty--need-refresh t)

         (if inhibit-moves
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

       ;; Handle modification.
       (let ((m (car modifications)))
         (setq modifications (cdr modifications))

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

         ;; don't even try to delete trailing ws, as they can't be
         ;; trusted (Issue #34)
         (when (memq (char-after old-end) '(nil ?\n))
           (while (eq ?\  (char-before old-end))
             (move-marker old-end (1- old-end))
             (cl-decf old-length)))

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
             (funcall next-modification-f)
           (funcall move-to-target-f)))))

    (setq
     move-to-target-f
     (lambda ()
       (cond
        ((> target upper-limit)
         (mistty-log "SKIP target=%s > upper-limit=%s"
                     (marker-position target)
                     (marker-position upper-limit))
         (funcall next-modification-f))

        ((and (< target lower-limit))
         (mistty-log "SKIP target=%s < lower-limit=%s"
                     (marker-position target)
                     (marker-position lower-limit))
         (funcall next-modification-f))

        (t
         (when inhibit-moves
           (mistty-log "INHIBITED: to target: %s -> %s" (point) target)
           (funcall after-move-to-target-f))

         (when mistty--can-move-vertically
           (let ((distance (mistty--vertical-distance (point) target)))
             (mistty-log "to target: %s -> %s lines: %s"
                         (point) target distance)
             (let ((term-seq (mistty--move-vertically-str distance)))
               (when (mistty--nonempty-str-p term-seq)
                 (mistty--interact-return
                  interact term-seq
                  :wait-until (lambda ()
                                (mistty--update-backstage)
                                (mistty--same-line-p (point) target))
                  :then after-move-vertically-f)))))
         (funcall after-move-vertically-f)))))

    (setq after-move-vertically-f
          (lambda ()
            (setq distance (mistty--distance (point) target))
            (mistty-log "to target: %s -> %s distance: %s" (point) target distance)
            (let ((term-seq (mistty--move-horizontally-str distance)))
              (when (mistty--nonempty-str-p term-seq)
                (mistty--interact-return
                 interact term-seq
                 :wait-until (lambda ()
                               (mistty--update-backstage)
                               (= (point) target))
                 :then after-move-to-target-f)))
            (funcall insert-and-delete-f)))

    (setq
     after-move-to-target-f
     (lambda ()
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
               (funcall insert-and-delete-f))
           ;; skip delete or replace
           (mistty-log "SKIP delete or replace; %s (wanted %s)"
                       (point) (marker-position target))
           (funcall next-modification-f)))

        ((and (> target (point))
              (> (mistty--distance (point) target) 0))
         (mistty-log "UPPER LIMIT: %s (wanted %s)"
                     (point) (marker-position target))
         (move-marker upper-limit (point))
         (funcall next-modification-f))

        (t
         (move-marker target (point))
         (funcall insert-and-delete-f)))))

    (setq
     insert-and-delete-f
     (lambda ()
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
                   (mistty--repeat-string char-count "\b")))
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
                  (regexp-quote content)))
           (mistty-log "RE /%s/" inserted-detector-regexp)
           (unless modifications
             (setq waiting-for-last-change t))
           (mistty--interact-return
            interact term-seq
            :wait-until (lambda ()
                          (mistty--update-backstage)
                          (mistty--remove-text-with-property 'term-line-wrap)
                          (mistty--remove-text-with-property 'mistty-skip)
                          (looking-back inserted-detector-regexp (point-min)))
            :then after-insert-and-delete-f)))
       (funcall next-modification-f)))

    (setq
     after-insert-and-delete-f
     (lambda ()
       (setq waiting-for-last-change nil)
       (mistty--update-backstage)
       (funcall next-modification-f)))

    (setf
     (mistty--interact-call interact)
     (lambda (other-cs)
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
           t))))

    (setf
     (mistty--interact-cleanup interact)
     (lambda ()
       (mistty--delete-backstage backstage)

       ;; Always release the changeset at the end and re-enable
       ;; refresh.
       (mistty--release-changeset cs)
       (mistty--refresh-after-changeset)))

    interact))

(defun mistty--refresh-after-changeset ()
  "Refresh the work buffer again if there are not more changesets."
  (unless mistty--changesets
    (setq mistty--inhibit-refresh nil)
    (when mistty--need-refresh
      (mistty--refresh))))

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
               mistty-up-str
             mistty-down-str)))
      (mistty--repeat-string distance towards-str))))

(defun mistty-next-input (n)
  "Move the point to the Nth next input in the buffer."
  (interactive "p")
  (dotimes (_ n)
    (if-let ((prop-match (text-property-search-forward 'mistty-input-id nil nil 'not-current)))
        (goto-char (prop-match-beginning prop-match))
      (if (and (process-live-p mistty-proc)
               (< (point) mistty-sync-marker)
               (or (mistty-maybe-realize-possible-prompt (mistty-cursor))
                   (mistty-on-prompt-p (mistty-cursor))))
          (goto-char (mistty--bol (mistty-cursor)))
        (error "No next input")))))

(defun mistty-previous-input (n)
  "Move the point to the Nth previous input in the buffer."
  (interactive "p")
  (dotimes (_ n)
    (if-let ((prop-match (text-property-search-backward 'mistty-input-id nil nil 'not-current)))
        (goto-char (prop-match-beginning prop-match))
      (error "No previous input"))))

(defun mistty-next-output (n)
  "Move the point to the beginning of the Nth next output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (let (end)
    (dotimes (_ n)
      (if-let ((prop-match (text-property-search-forward 'mistty-input-id nil t 'not-current))
               (pos (prop-match-beginning prop-match)))
          (if (or (not mistty-sync-marker)
                  (not mistty--has-active-prompt)
                  (< pos mistty-sync-marker))
              (progn
                (goto-char pos)
                (setq end (prop-match-end prop-match)))
            (error "No next output"))
        (error "No next output")))
    (cons (point) end)))

(defun mistty-previous-output (n)
  "Move the point to the beginning of the Nth previous output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (let (end)
    (dotimes (_ n)
      (if-let ((prop-match (text-property-search-backward 'mistty-input-id nil t 'not-current))
               (pos (prop-match-beginning prop-match)))
          (progn (goto-char pos)
                 (setq end (prop-match-end prop-match)))
        (error "No previous output")))
    (cons (point) end)))

(defun mistty-current-output-range ()
  "Return the range of the current output, if point is on an output.

This function returns nil, if the point is not inside an output.
Otherwise, it returns a cons containing the start and end
position of the output."
  (save-excursion
    (unless (get-text-property (point) 'mistty-input-id)
      (when-let ((beg (if (or (bobp)
                              (get-text-property (1- (point)) 'mistty-input-id))
                          (point)
                        (previous-single-property-change
                         (point) 'mistty-input-id nil (point-min))))
                 (end (next-single-property-change (point) 'mistty-input-id)))
        (cons beg end)))))

(defun mistty-clear (n)
  "Clear the MisTTY buffer until the end of the last output.

With an argument, clear from the end of the last Nth output."
  (interactive "p")
  (let ((range (save-excursion (mistty-previous-output (or n 1)))))
    (mistty-truncate (min mistty-sync-marker (cdr range)))))

(defun mistty-select-output (&optional n)
  "Select the current or Nth previous output range."
  (interactive "P")
  (let ((range (mistty--current-or-previous-output-range n)))
    (goto-char (car range))
    (set-mark (cdr range))))

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
                 (mistty--command-for-output
                  current-prefix-arg)
               (error nil))
             "command output")
            30)))
          current-prefix-arg))
  (let ((range (mistty--current-or-previous-output-range n))
        (buffer (generate-new-buffer buffer-name)))
    (copy-to-buffer buffer (car range) (cdr range))
    (with-current-buffer buffer
      (set-auto-mode 'keep-mode-if-sane))
    (pop-to-buffer buffer)
    buffer))

(defun mistty--command-for-output (&optional n)
  "Extract command for the Nth output, if possible.

Note that this is VERY UNRELIABLE. It won't work with multi-line
commands and might contain parts of the prompt. Use it only in
cases where reliability doesn't matter.

Return nil if no command could be extracted."
  (let* ((range (mistty--current-or-previous-output-range n))
         (eol (mistty--eol (car range) 0))
         (bol (mistty--bol eol))
         (command
          (save-excursion
            (goto-char bol)
            (when (search-forward-regexp
                   "\\(^\\|[[:blank:]]\\)\\([[:alpha:]]+\\)"
                   eol 'noerror)
              (goto-char (match-beginning 2)))
            (string-trim
             (buffer-substring-no-properties
              (point)
              (or (text-property-any
                   (point) eol 'mistty-skip 'right-prompt)
                  eol))))))
    (when (and (length> command 0)
               (not (string-match-p
                     "^\\(end\\|done\\|fi\\)" command)))
      command)))

(defun mistty--current-or-previous-output-range (&optional n)
  "Return the range of the current or the Nth output.

If called with no arguments and the point is on an output, create
a buffer with that output, otherwise return the range of a
previous output.

This function fails if there is no current or previous output."
  (save-excursion
    (if (numberp n)
        (mistty-previous-output n)
      (or (mistty-current-output-range)
          (mistty-previous-output 1)))))

(defun mistty--pre-command ()
  "Function called from the `pre-command-hook' in `mistty-mode' buffers."
  (mistty--detect-foreign-overlays 'noschedule)
  (mistty--pre-command-for-undo)
  (when (and mistty--self-insert-line
             (not (eq this-command 'mistty-self-insert)))
    (setq mistty--self-insert-line nil))
  (setq mistty--old-point (point)))

(defun mistty--post-command ()
  "Function called from the `post-command-hook' in `mistty-mode' buffers."
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
     mistty-work-buffer point-moved)))

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
             (let ((last-interaction (mistty--queue-last-interact mistty--queue)))
               (unless (and last-interaction
                            (eq 'replay (mistty--interact-type last-interaction))
                            ;; append to existing interaction
                            (mistty--call-interact last-interaction 'replay cs))
                 (mistty--enqueue mistty--queue (mistty--replay-interaction cs)))))

           ;; Abandon changesets that haven't been picked up for replay.
           (when (and (not replay) (mistty--changeset-p cs))
             (mistty--release-changeset cs)
             (mistty--refresh-after-changeset))

           (when (and (not replay) (not mistty--forbid-edit) point-moved)
             (mistty--enqueue mistty--queue (mistty--cursor-to-point-interaction)))

           (when mistty--need-refresh
             (mistty--refresh))))))))

(defun mistty--cursor-to-point-interaction ()
  "Build a `mistty--interact' to move the cursor to the point."
  (let ((interact (mistty--make-interact 'cursor-to-point))
        after-move-vertically-f
        (can-move-f (lambda (from to)
                      (and (mistty-on-prompt-p to)
                           (not mistty--forbid-edit)
                           (>= from (point-min))
                           (<= from (point-max))
                           (>= to (point-min))
                           (<= to (point-max))
                           (mistty--with-live-buffer mistty-term-buffer
                             (<= (mistty--from-pos-of to mistty-work-buffer)
                                 (point-max)))))))
    (setf
     (mistty--interact-cb interact)
     (lambda (&optional _)
       (when mistty--can-move-vertically
         (let ((from (mistty-cursor))
               (to (point)))
           (unless (funcall can-move-f from to)
             (mistty--interact-done))
           (let ((distance (mistty--vertical-distance from to)))
             (mistty-log "cursor to point: %s -> %s lines: %s"
                         from to distance)
             (let ((term-seq (mistty--move-vertically-str distance)))
               (when (mistty--nonempty-str-p term-seq)
                 (mistty--interact-return
                  interact term-seq
                  :wait-until (lambda ()
                                (mistty--same-line-p
                                 (mistty-cursor) (point)))
                  :then after-move-vertically-f))))))
       (funcall after-move-vertically-f)))

    (setq after-move-vertically-f
          (lambda (&optional _)
            (let ((from (mistty-cursor))
                  (to (point)))
              (unless (funcall can-move-f from to)
                (mistty--interact-done))
              (let* ((distance (mistty--distance from to))
                     (term-seq (mistty--move-horizontally-str distance)))
                (when (mistty--nonempty-str-p term-seq)
                  (mistty-log "cursor to point: %s -> %s distance: %s" from to distance)
                  (mistty--interact-return
                   interact term-seq
                   :wait-until
                   (lambda ()
                     ;; Ignoring skipped spaces is useful as, with
                     ;; multiline prompts, it's hard to figure out
                     ;; where the indentation should be without
                     ;; understanding the language.
                     (mistty--same-pos-ignoring-skipped
                      (mistty-cursor) (point)))
                   :then
                   (lambda ()
                     (mistty-log "moved cursor to %s (goal: %s)"
                                 (mistty-cursor) (point))
                     (mistty--interact-done)))))
              (mistty--interact-done))))

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

(defun mistty--window-size-change (_win)
  "Update the process terminal size, reacting to _WIN changing size."
  (when (and (process-live-p mistty-proc)
             (eq mistty-work-buffer (current-buffer)))
    (let* ((adjust-func (or (process-get mistty-proc 'adjust-window-size-function)
                            window-adjust-process-window-size-function))
           (size (funcall adjust-func mistty-proc
                          (get-buffer-window-list mistty-work-buffer nil t))))
      (mistty-log "set-process-window-size %s" size)
      (when size
        (let ((width (- (car size) left-margin-width))
              (height (cdr size)))
        (mistty--set-process-window-size width height))))))

(defun mistty--set-process-window-size (width height)
  "Set the process terminal size to WIDTH x HEIGHT."
  (mistty--with-live-buffer mistty-term-buffer
    (set-process-window-size mistty-proc height width)
    (term-reset-size height width)))

(defun mistty--enter-fullscreen (proc terminal-sequence)
  "Enter fullscreen mode for PROC.

TERMINAL-SEQUENCE is processed in fullscreen mode."
  (mistty--with-live-buffer (process-get proc 'mistty-work-buffer)
    (mistty--detach)
    (setq mistty-fullscreen t)
    (mistty--with-live-buffer mistty-term-buffer
      (setq mistty-fullscreen t))

    (let ((msg (mistty--fullscreen-message)))
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
    (run-hooks 'mistty-entered-fullscreen-hook)
    (when (length> terminal-sequence 0)
      (funcall (process-filter proc) proc terminal-sequence))))

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
      (mistty--refresh)
      (when (and proc (process-live-p proc))
        (mistty-goto-cursor))

      (let ((bufname (buffer-name mistty-term-buffer)))
        (with-current-buffer mistty-term-buffer
          (rename-buffer (generate-new-buffer-name (concat " mistty tty " bufname))))
        (rename-buffer bufname))

      (mistty--swap-buffer-in-windows mistty-term-buffer mistty-work-buffer)
      (with-current-buffer mistty-term-buffer
        (font-lock-mode -1))

      (mistty--update-mode-lines proc)
      (run-hooks 'mistty-left-fullscreen-hook)
      (when (length> terminal-sequence 0)
        (funcall (process-filter proc) proc terminal-sequence)))))

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
  "Return non-nil if POS is on a prompt."
  (and mistty-sync-marker
       mistty--has-active-prompt
       (>= pos mistty-sync-marker)
       (or mistty-bracketed-paste
           (<= pos (mistty--eol mistty-sync-marker))
           ;; The position passed to (mistty-on-prompt-p
           ;; (mistty-cursor)) might not be on the same line as
           ;; mistty-sync-marker on the work buffer; check on the term
           ;; buffer as well.
           (mistty--with-live-buffer mistty-term-buffer
             (<= (mistty--from-pos-of pos mistty-work-buffer)
                 (mistty--eol mistty-sync-marker))))))

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
               (mistty--possible-prompt-contains pos)
               (mistty--possible-prompt-contains (mistty-cursor)))
      (mistty--realize-possible-prompt)
      t)))

(defun mistty--realize-possible-prompt (&optional shift)
  "Create the prompt in `mistty--possible-prompt'.

If SHIFT is non-nil, it specifies a position difference between
the sync markers in the work and term buffer at the beginning of
the prompt."
  (pcase-let ((`(,start ,_ ,_ ) mistty--possible-prompt))
    (if shift
        (mistty--move-sync-mark-with-shift start shift)
      (mistty--set-sync-mark-from-end start)))
  (setq mistty--has-active-prompt t)
  (setq mistty--possible-prompt nil))

(defun mistty--possible-prompt-p ()
  "Return non-nil if `mistty--possible-prompt' is usable."
  (when mistty--possible-prompt
    (pcase-let ((`(,start ,end ,content) mistty--possible-prompt))
      (let ((cursor (mistty-cursor)))
        (and (or (> start mistty-sync-marker)
                 (and (= start mistty-sync-marker)
                      (not mistty--has-active-prompt)))
             (>= cursor end)
             (or (> cursor (point-max))
                 (<= cursor (mistty--bol start 2)))
             (string= content (mistty--safe-bufstring start end)))))))

(defun mistty--possible-prompt-contains (pos)
  "Return non-nil if POS is on `mistty--possible-prompt'."
  (when mistty--possible-prompt
    (pcase-let ((`(,start ,line-start ,_) mistty--possible-prompt))
      (and (>= pos line-start) (<= pos (mistty--eol start))))))

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
               (mistty-on-prompt-p (setq pos (window-point win))))
      (when-let ((last-state (window-parameter win 'mistty--cursor-skip-state)))
        (when (eq (car last-state) (current-buffer))
          (setq last-pos (cdr last-state))))
      (unless (equal pos last-pos)
        (pcase-dolist (`(,beg . ,end)
                       (mistty--cursor-skip-ranges
                        pos (lambda (type)
                              (memq type '(indent right-prompt continue-prompt empty-lines-at-eob)))))
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
  (save-excursion
    (let ((count 0)
          (sign (if (> beg end) -1 1))
          (beg (min beg end))
          (end (max beg end)))
      (goto-char beg)
      (while (search-forward "\n" end 'noerror)
        (unless (get-text-property (match-beginning 0) 'term-line-wrap)
          (cl-incf count)))

      (* sign count))))

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

(defun mistty--align-buffers()
  "Return reasonable sync marks for the work and term buffers.

Return nil or \\='(WORK-SYNC . TERM-SYNC) with WORK-SYNC
aposition on the work buffer and TERM-SYNC the position on the
term buffer."
  (mistty--require-term-buffer)

  (when (buffer-live-p mistty-work-buffer)
    (let* ((term-home term-home-marker)
           (term-buffer mistty-term-buffer)
           (prelude
            (with-temp-buffer
              (insert-buffer-substring term-buffer nil term-home)
              (mistty--prepare-for-scrollback (point-min) (point-max))
              (buffer-substring-no-properties (point-min) (point-max)))))
      (mistty-log "prelude: [%s]" prelude)
      (when (string-match-p "[^[:space:]]" prelude)
        (let* ((term-lines (count-lines (point-min) (point-max)))
               (work-sync (with-current-buffer mistty-work-buffer
                            (let ((limit
                                   (save-excursion
                                     (goto-char (point-max))
                                     (forward-line (- term-lines))
                                     (point))))

                              (save-excursion
                                (goto-char (point-max))
                                (when (search-backward prelude limit 'noerror)
                                  (goto-char (match-end 0))
                                  (point)))))))
          (when work-sync
            (cons work-sync (marker-position term-home-marker))))))))

(provide 'mistty)

;;; mistty.el ends here
