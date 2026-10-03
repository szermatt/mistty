# MisTTY, a terminal for Emacs with shell/comint behavior

[![CI Status](https://github.com/szermatt/mistty/actions/workflows/CI.yml/badge.svg)](https://github.com/szermatt/mistty/actions/workflows/CI.yml)
[![Documentation Status](https://readthedocs.org/projects/mistty/badge/?version=latest)](https://mistty.readthedocs.io/en/latest/?badge=latest)
[![MELPA](https://melpa.org/packages/mistty-badge.svg)](https://melpa.org/#/mistty)
[![MELPA Stable](https://stable.melpa.org/packages/mistty-badge.svg)](https://stable.melpa.org/#/mistty)

**MisTTY** is a terminal application for Emacs 29.1 and up that allows
editing prompts using Emacs commands, like in the `shell` command, but
in a full-featured, modern terminal.

> [!NOTE]
>
> Terminal support is provided by [alacritty](https://alacritty.org),
> with the help of an Emacs module. Without that module, MisTTY
> falls back to using the more limited Emacs built-in terminal `eterm`.
> To avoid that install the module with `M-x mistty-install` or `M-x
> mistty-install-dwim`. For more details, head over to
> https://mistty.readthedocs.io/en/latest/usage.html#module-with-alacritty

`M-x mistty` creates a buffer with an interactive shell. See
[launching](https://mistty.readthedocs.io/en/latest/usage.html#launching)
for details.

In that buffer you can move around freely and run any Emacs command
you want - until you press TAB and end up with the native completion
or notice the shell autosuggestions. With MisTTY you have access to
both Emacs and the shell commands and editing tools. Like `comint`,
MisTTY supports [remote shells with
TRAMP](https://mistty.readthedocs.io/en/latest/usage.html#tramp).

Additionally, commands that take over the entire screen, such as
`less` or `vi` also work, temporarily taking over the terminal zone
and keyboard.

MisTTY works well with Bash and ZSH, but it is especially well suited
to running [Fish](https://fishshell.com). With Fish, you get
autosuggestion, completion in full colors, directory tracking with
OSC7, and prompt detection with OSC133 out of the box. [Other shells
can be configured to support
all that](https://mistty.readthedocs.io/en/latest/shells.html)

Special configuration isn't absolutely needed, as MisTTY tries to
support supports any command-line programs with a prompt in a
reasonable way, like python or ipython.

It is, however, possible that a shell or prompt command confuses it,
especially advanced ones. In such a case, please consider [turning on
OSC133 support in the
command](https://mistty.readthedocs.io/en/latest/usage.html#osc133)
and [filing a bug report](
https://mistty.readthedocs.io/en/latest/contrib.html#reporting-issues).

![screen grab](https://github.com/szermatt/mistty/blob/master/screengrab.gif?raw=true)

MisTTY is known to work on Linux and MacOS.

## COMPARISON

As its core, MisTTY is a frontend to a terminal emulator. Terminal
emulation is provided by [alacritty
library](https://github.com/alacritty/alacritty), which emulates xterm
and has [support for modern terminal
extensions](https://alacritty.org/misc-alacritty-escapes.html).

MisTTY goes beyond plain terminal emulation in Emacs to provide a
convenient tools for command-line editing. It does its best to make as
much of Emacs editing capabilities available while on a prompt.

This idea is similar to `coterm`, which offers the same switch between
full-screen and line mode.

`eat` also has a semi-char mode, which is the closest there is to
MisTTY. In that mode, Emacs movements commands are available. However,
Emacs commands that modify the buffer, aren't available to edit the
command line. In contrast, MisTTY allows Emacs to navigate to and edit
the whole buffer, then replays changes made to the command-line.

Other terminal emulators are available for Emacs, such as `vterm` and
`ghostty`, which do terminal emulation well, but don't offer the deep
integration with Emacs commands that MisTTY does.

## INSTALLATION

> **The following is just a quick introduction. Read the full documentation at https://mistty.readthedocs.io/en/latest/**

You can install MisTTY:
- from [MELPA](https://melpa.org/#/getting-started), by typing `M-x package-install mistty`
- from source, by executing `(package-vc-install "https://github.com/szermatt/mistty")`

Once the package is installed, run `M-x mistty-install-dwim` to
install the module. Without it, MistTTY will be usable, but limited to
the Emacs built-in terminal emulator.

## USAGE

Type `M-x mistty` to launch a new shell buffer in MisTTY mode, then
use it as you would comint.

You'll quickly notice some differences. For example TAB completion
working just like in a terminal instead of relying of Emacs
completion.

The purple line on the left indicates the portion of the buffer
that's a terminal. What you type in there gets sent to the program,
usually a shell, and translated by that program. The rest of the
buffer is normal, editable, text.

Commands that takes the whole screen such as `less` or `vi` take you
into fullscreen mode for the duration of that command and most keys -
except for `C-c` and `C-x` are sent to the terminal. You can still
access previous commands in the scrollback zone by typing `C-c C-j`.

If you ever get into a situation where a command needs you to press
keys normally sent to Emacs, press `C-q <key>`.

You can also temporarily switch to the fullscreen mode map using `C-c
C-k` or switch to a special keyboard capture mode with `C-c C-q` that
send everything but `C-g` to the terminal.

You will very likely want to send some keys you use often directly
to the terminal. This is done by binding keys to `mistty-send-key`
in `mistty-prompt-map`. For example:

```elisp
(Use-package mistty
  :bind (("C-c s" . mistty)
         ;; bind here the shortcuts you'd like the
         ;; shell to handle instead of Emacs.
         :map mistty-prompt-map
         ;; fish: directory history
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key)))
```

Also, unless the shell you're using does it automatically, you might
also need to configure your shell to send out directory tracking
information. For more details, see [Directory
Tracking](https://mistty.readthedocs.io/en/latest/usage.html#directory-tracking).

See also [the documentation](https://mistty.readthedocs.io/en/latest/)
for more details on configuring MisTTY .

## SOMETHING IS WRONG !

Please check the [FAQ](https://mistty.readthedocs.io/en/latest/faq.html)
and, if that doesn't help, take the time to [file a bug report](https://mistty.readthedocs.io/en/latest/contrib.html#reporting-issues).

## CONTRIBUTING

See the [Contributing](https://mistty.readthedocs.io/en/latest/contrib.html)
section of the documentation.

## COMPATIBILITY

MisTTY requires Emacs 29.1 or later.
