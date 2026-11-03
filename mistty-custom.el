;;; mistty-custom.el --- Customizable MisTTY options. -*- lexical-binding: t -*-

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
;; This file collects customization options for the MisTTY package.

(defcustom mistty-set-EMACS nil
  "Whether the EMACS env variable should be set, for Bash 4.3 and older.

NOTE: Bash detecting that it's running under Emacs only works when using
term.el as Bash wants the TERM env variable to be set to eterm. When
using the module, only OSC7 work for directory tracking.

You can set set this if:
 - you're stuck using a very old version of Bash (4.3 or older)
 - you don't want to set up directory tracking using OSC7
   as described in the manual

When set, MisTTY sets the EMACS env variable, which Bash 4.3 and
older check to decide whether to send out directory tracking
information. (Newer version check INSIDE_EMACS instead.)

As this is usually host-specific, it can be set as a
connection-local variable. This might be useful when connecting
with TRAMP to hosts or docker instances that use a very old
version of Bash that you don't want to configure.

For example:

  (connection-local-set-profile-variables
   \\='profile-old-bash
   \\='((mistty-set-EMACS . t)
     (mistty-shell-command . (\"/bin/bash\" \"-i\"))))

  (connection-local-set-profiles \\='(:machine \"oldhost.example.com\")
   \\='profile-old-bash)
  (connection-local-set-profiles \\='(:protocol \"docker\")
   \\='profile-old-bash)"
  :group 'mistty
  :type 'boolean)

(defcustom mistty-multi-line-continue-prompts
  '("^    *\\.\\.\\.: " ; ipython
    )
  "Regexp used to identify multi-line command prompts.

These regexps identifies prompts that tell the user that they can
type more, while still allowing them to edit what's above. MisTTY
uses these regexps to identify sections of texts it should ignore
when editing.

Note that bash \"> \" is not a continuation prompt, with this
definition, because it doesn't allow editing what's above."
  :group 'mistty
  :type '(list regexp))

(defcustom mistty-allow-tramp-paths t
  "If true, allow generating TRAMP paths as shell-specified directory.

This affects directories set using OSC 7, which can then build
TRAMP remote paths based on the hostname specified in the file://
URL, using the default method.

This option doesn't affect hosts configured using
`mistty-host-to-tramp-path-alist'."

  :group 'mistty
  :type '(boolean))

(defcustom mistty-host-to-tramp-path-alist nil
  "Maps hostnames to TRAMP paths.

This alist maps hosts as self-declared using OSC 7 to the TRAMP
path MisTTY should use for directory tracking.

This is useful for hosts that require a specific method to
connect to.

The values should be of the same form as the remote identifiers
returned by `file-remote-p', with method, user and host, for
example \"/ssh:example.com:\". Invalid values are ignored.

The values can also be nil to disable directory tracking for that
specific host.

Note that for the ssh method, it's often easier to add the
configuration option you need to a host to ~/.ssh/config."
  :group 'mistty
  :type '(alist :key-type (string :tag "Host")
                :value-type (choice (string :tag "TRAMP Path")
                                    (const nil))))


(defcustom mistty-exit-capture-keyboard-key "C-g"
  "Key that ends `mistty-capture-keyboard'.

While it is running, `mistty-capture-keyboard' sends all keys to the
terminal, all except this specific key or key combination, which exits
the mode.

It must be a single key or key combination reported by `read-key', not a
key sequence.

It's also possible to exit keyboard capture with the mouse, clicking
anywhere, or programatically by calling `mistty-exit-capture-keyboard'."
  :group `mistty
  :type 'key)

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

(autoload 'mistty-osc7 "mistty-osc7")
(autoload 'mistty-osc-query-color "mistty-osc-colors")
(autoload 'ansi-osc-window-title-handler "ansi-osc")
(autoload 'ansi-osc-hyperlink-handler "ansi-osc")

(defcustom mistty-osc-handlers
  '(
    ;; not using ansi-osc-directory-tracker because it doesn't decode
    ;; the coding system of the path after percent-decoding it.
    ;; TODO: propose a fix for ansi-osc
    ("7" . mistty-osc7)

    ;; These handlers are reasonably compatibly with MisTTY OSC. This
    ;; isn't necessary going to be the case for all such handlers.
    ("0" . ansi-osc-window-title-handler)
    ("2" . ansi-osc-window-title-handler)
    ("8" . ansi-osc-hyperlink-handler)

    ;; Allow querying foreground and background color. While OSC 10/11
    ;; normally supports changing color, this isn't supported here.
    ("10" . mistty-osc-query-color)
    ("11" . mistty-osc-query-color)
    ("133" . mistty-osc133))
  "Hook run when unknown OSC sequences have been received.

This hook is run on the `term-mode' buffer. It is passed the OSC code as
a string and the content of OSC sequence - everything between OSC (ESC
]) and ST (ESC \\ or \\a) and may choose to handle them.

The current buffer a`term-mode' buffer. The hook is allowed to
modify it, to add text properties, for example. In such case,
consider using `mistty-register-text-properties'.

Most handlers written for the ansi-osc package (Emacs 29) should
work here as well.

If you add here a handler that sets a buffer-local variable,
consider adding that variable to `mistty-variables-to-copy' so
that its value is available in the main MisTTY buffer, not just
the terminal buffer.

This option only works on eterm terminals. It has no effect on alacritty
terminals."
  :group 'mistty
  :type '(alist :key-type string :value-type function))

(declare-function mistty-call-term-mode-hook "mistty")
(defcustom mistty-term-mode-hook (list #'mistty-call-term-mode-hook)
  "Hook run in in `term-mode' buffers created by MisTTY.

This hook overrides `term-mode-hook' for term buffers started by MisTTY
to allow configuring MisTTY's term buffers differently from normal term
buffers.

The default includes `mistty-call-term-mode-hook', which calls the
original `term-mode-hook'.

If you'd like to have completely different configuration for normal `term-mode'
buffers and `term-mode' buffers started by Mistty, call:

  (remove-hook \\='mistty-term-mode-hook \\='mistty-call-term-mode-hook)

You might want to execute the above command as well if you have reasons
to think that some `term-mode' customization are interfering with MisTTY's
operations.

This option only works on eterm terminals. It has no effect on alacritty
terminals."
  :group 'mistty
  :type 'hook)

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
this variable for different hosts by setting `mistty-shell-command'
as a connection-local variable."
  :type '(repeat string)
  :group 'mistty)

;;;###autoload
(defcustom mistty-terminal-type nil
  "Choose a terminal emulator.

The terminal emulator MisTTY uses can be either:

- \\='eterm the pure elisp terminal that's part of Emacs. It's found in
  eterm.el.

- \\='alacritty a modern terminal emulator implemented in Rust.

Alacritty is more standard-compliant and supports more features than
eterm, but it requires installing and loading an Emacs module.

If this option is nil (auto), MisTTY uses alacritty if the module is
available and otherwise fallback."
  :group 'mistty
  :type '(choice (const :tag "auto" nil)
                 (const :tag "eterm (built-in)" eterm)
                 (const :tag "alacritty (module required)" alacritty)))

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

(declare-function mistty-fringe-mode "mistty")
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
the up/down special-prompt navigate through history instead of moving the
cursor.

Vertical moves are turned on when a regexp on this list matches
the beginning of the terminal zone."
  :group 'mistty
  :type '(list regexp))

(define-obsolete-variable-alias
  'mistty-forbid-edit-prompt-regexps
  'mistty-special-prompt-regexps "2.1")
(defcustom mistty-special-prompt-regexps
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

`mistty-special-prompt-map' is the active map in the synced region
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

Note that this setting doesn't apply to the split buffer fullscreen
mode, as then terminal size always matches window size.

You can also change the terminal size of existing MisTTY buffers with
the commands `mistty-set-terminal-size' and
`mistty-terminal-size-tracks-window'."
  :group 'mistty
  :type '(choice (const :tag "Track Windows" nil)
                 (cons :tag "Fixed Size"
                       (natnum :tag "Width")
                       (natnum :tag "Height"))))

(defcustom mistty-bracketed-paste-command-alist
  '((yank . t)
    (yank-pop . t)
    (yank-rectangle . t)
    (insert-register . t)
    (insert-rectangle . t)
    (undo . t)
    (self-insert-command . nil))
  "Specify whether to use bracketed paste to replay command changes.

This option exists because some applications handle strings pasted using
bracketed paste differently from others. Bash, for example, highlights
strings inserted using bracketed paste. Fish, on the other hand, might
try to match opening braces, causing havoc, unless the string is
inserted using bracketed paste.

Commands that insert strings, such as `yank' should definitely use
bracketed paste, because that most closely matches what applications
expect. Commands that modify strings, on the other hand, benefit from
not using bracketed paste, so the application handles the text as if it
was typed.

See also `mistty-bracketed-paste-default', which controls how commands
are handled that are not on this list."
  :group 'mistty
  :type '(alist :key-type function :value-type boolean))

(defcustom mistty-bracketed-paste-default nil
  "Specify whether to use bracketed paste even when not necessary.

This option specifies whether MisTTY should use bracketed paste when
replaying changes made to the Emacs buffer even when the string contains
no special characters that need to be escaped.

This option exists because some applications handle strings pasted using
bracketed paste differently from others. Bash, for example, highlights
strings inserted using bracketed paste. Fish, on the other hand, might
try to match opening braces, causing havoc, unless the string is
inserted using bracketed paste.

Whether to use bracketed paste or not can also be configured on a
command-by-command basis by configuring
`mistty-bracketed-paste-command-alist'.

Note that no matter the value of this option, bracketed paste will not
be used if the application doesn't support it even if this option is t.
Also, bracketed paste will be used if the inserted string contains
newlines or other control characters even if this option is nil."
  :group 'mistty
  :type 'boolean)

(defcustom mistty-allow-mouse-reporting t
  "Send clicks and mouse position to applications.

If non-nil, MisTTY sends clicks and motions to the application running
on the terminal if that application requests it.

Set this to nil if you don't want to send mouse information by default.
Users can still enable it on a case-by-case basis by clicking on the
mouse in the mode line or calling the command `mistty-toggle-mouse'."
  :group 'mistty
  :type 'boolean)

(defcustom mistty-allow-click-to-move t
  "Allow using clicks, when available, to move the cursor on the prompt.

If non-nil, MisTTY sends clicks instead of the arrow keys to
applications that report supporting it to move the cursor. This may work
well, or it may cause issues as moving may accidentally press buttons.

Set this to nil if you only want to use the arrow key to move the cursor
in prompt mode."
  :group 'mistty
  :type 'boolean)

(defvar mistty-tag-alist-alist
  `((textual
     (dead . "dead")
     (running . "running")
     (prompt . "prompt")
     (off . "off")
     (special-prompt . "prompt!")
     (full . "fullscreen")
     (scrollback . "scrollback")
     (split-fullscreen . "fullscreen")
     (capture . "kb capture")
     (keyboard . "[kb]")
     (keyboard-inactive . "(kb)")
     (mouse . "[mouse]")
     (mouse-inactive . "(mouse)"))

    (ascii
     (dead . "×_×")
     (running . "...")
     (prompt . ">_")
     (off . "...")
     (special-prompt . ">_!")
     (full . "[_]")
     (scrollback . "v=v")
     (split-fullscreen . "^=^")
     (capture . "[#]")
     (keyboard . "#")
     (keyboard-inactive . "")
     (mouse . "[M]")
     (mouse . "(m)n"))

    (unicode
     (dead . "×_×")
     (running . "…")
     (prompt . ">_")
     (off . "…")
     (special-prompt . "←↑↓→")
     (full . "[_]")
     (scrollback . "▼ ")
     (split-fullscreen . "▲ ")
     (capture . "[#]")
     (keyboard . "⌨")
     (keyboard-inactive . "")
     (mouse . "▮")
     (mouse-inactive . "▯")))
  "Group of mode-line tags used by this package.

The active theme can be configured using mistty-tag-theme.

All themes must define tags for the following ids:
 - \\='dead, displayed when the terminal command died
 - \\='running, displayed when there is no prompt
 - \\='prompt, displayed when a prompt is detected
 - \\='off, displayed when command is running
 - \\='special-prompt, displayed when a special prompt is detected
 - \\='full, displayed in fullscreen mode
 - \\='scrollback, displayed in the scrollback buffer, when in
   split-buffer fullscreen mode (eterm only)
 - \\='capture, displayed while `mistty-capture-keyboard'
   is running
 - \\='keyboard, displayed when the fullscreen mode map is active.
   It can be set to the empty string to display nothing in that
   case.
 - \\='keyboard-inactive, displayed when the fullscreen mode map
   is inactive. It can be set to the empty string to display
   nothing in that case.
 - \\='mouse, displayed when mouse reporting is enabled. It can be set to
   the empty string to display nothing in that case.
 - \\='mouse-inactive, displayed when mouse reporting was requested
   but is disabled. It can be set to the empty string to display
   nothing in that case.

All IDs must be set in all themes.")

(defcustom mistty-tag-theme 'unicode
  "Choose the set of tags that appear in the modeline.

This option allows you to choose which family of tags, defined in
`mistty-tag-theme' are used to display the modeline.

This can be used to select your preferred theme as well as to adapt the
tags to the capabilities of your display and font.

Define your own themes to mistty-tag-alist-alist before selecting them
here."
  :group 'mistty
  :require 'mistty
  :type `(choice ,@(mapcar (lambda (elt) `(const ,(car elt)))
                           mistty-tag-alist-alist)
                 (restricted-sexp
                  :tag "Other tag from mistty-tag-alist-alist"
                  :match-alternatives
                  ((lambda (val)
                     (and (symbolp val)
                          (assq val mistty-tag-alist-alist))))))
  :group 'mistty)

(defcustom mistty-backlog-size 0
  "Log entries to track when logging is disabled.

As many as `mistty-backlog-size' entries will be backfilled
by `mistty-start-log' when logging is enabled.

Setting this value allows turning on logging once something wrong
has happened."
  :group 'mistty
  :type '(integer))

(defcustom mistty-log-max-backtrace-length 2400
  "Maximum length, in characters, of logged backtraces.

Backtraces that are longer than this value are truncated by
`mistty-log-error'."
  :group 'mistty
  :type 'integer)

;;;###autoload
(defcustom mistty-install-dir nil
  "Directory where the module should be installed.

`mistty-install' will attempt to store the module into this directory.
It should be a writable directory that's on `load-path'.

It defaults to the directory where `mistty-install.el' is stored, if it
is writable, otherwise `user-emacs-directory'."
  :group 'mistty
  :type 'directory)

;;;###autoload
(defcustom mistty-install-keep-output nil
  "Keep around the *mistty-install* output buffer.

When this option is nil, which is the default, the buffer that displays
installation details and progress created by `mistty-install' and
`mistty-install-dwim' is displayed only after a delay and killed at the
end of a successful command.

With this option set, the buffer is displayed right away and kept around
for the user to kill."
  :group 'mistty
  :type 'boolean)

(provide 'mistty-custom)

;;; mistty-custom.el ends here

