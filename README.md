# MisTTY, a shell/comint alternative with a fully functional terminal

[![CI Status](https://github.com/szermatt/mistty/actions/workflows/CI.yml/badge.svg)](https://github.com/szermatt/mistty/actions/workflows/test.yml)
[![Documentation Status](https://readthedocs.org/projects/mistty/badge/?version=latest)](https://mistty.readthedocs.io/en/latest/?badge=latest)
[![MELPA](https://melpa.org/packages/mistty-badge.svg)](https://melpa.org/#/mistty)
[![MELPA Stable](https://stable.melpa.org/packages/mistty-badge.svg)](https://stable.melpa.org/#/mistty)

**MisTTY** is a major mode for Emacs 29.1 and up that runs
a shell inside of a buffer, similarly to comint mode. It is built on
top of `term.el`.

`M-x mistty` creates a buffer with an interactive shell. See
[launching](https://mistty.readthedocs.io/en/latest/usage.html#launching)
for details.

MisTTY feels very much like `comint` mode: you can move around freely
and run any Emacs command you want - until you press TAB and end up
with the native completion or notice the shell autosuggestions. With
MisTTY you have access to both Emacs and the shell commands and
editing tools.

Additionally, commands that take over the entire screen, such as
`less` or `vi` also work, temporarily taking over the window, while
scrollback remains available in another buffer.

MisTTY works well with Bash and ZSH, but it is especially well
suited to running [Fish](https://fishshell.com): you get
autosuggestions, completion in full colors. Here's what the end
result might look like:

![screen grab](https://github.com/szermatt/mistty/blob/master/screengrab.gif?raw=true)

MisTTY is known to work on Linux and MacOS. It also supports non-shell
command-line programs, such as python.

## COMPARISON

MisTTY isn't a terminal emulator, but rather a frontend to an existing
terminal emulator, the built-in `term.el`. Its goal is to make it more
convenient to use while inside of Emacs and better integrate with
Emacs itself. In theory, other terminal emulators than `term.el` might
be used as engine for MisTTY, such as `vterm` and `eat`.

MisTTY has some similarities with `coterm`; it offers the same switch
between full-screen and line mode.

`Coterm`, `ansi-term` and `eat` all have a line mode, just like comint.
While in line mode, rendering is done by Emacs and editing commands
are Emacs commands. In constrast, with MisTTY, all rendering is done
by the shell through the terminal. This is why native shell completion
and autosuggestion is available with MisTTY and not with these other
line modes and why you can freely mix shell commands with Emacs
commands while editing the command line.

## INSTALLATION

> **The following is just a quick introduction. Read the full documentation at https://mistty.readthedocs.io/en/latest/**

You can install MisTTY:
- from [MELPA](https://melpa.org/#/getting-started), by typing `M-x package-install mistty`
- from source, by executing `(package-vc-install "https://github.com/szermatt/mistty")`

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
into terminal mode for the duration of that command. You can still 
access previous commands in the "scrollback" MisTTY buffer by typing 
`C-c C-j`. 

If you ever get into a situation where a command needs you to press 
keys normally sent to Emacs, such as the arrow keys, press `C-c C-q`. 
It'll send all key strokes directly to the terminal until you exit 
the mode by pressing `C-g`. To send a single key to the terminal 
you can also press `C-q <key>` instead.

You will very likely want to send some keys you use often directly 
to the terminal. This is done by binding keys to `mistty-send-key` 
in `mistty-prompt-map`. For example:

```elisp
(use-package mistty
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

See also [the documentation](https://mistty.readthedocs.io/en/latest/)
for more details on configuring MisTTY [for different
shells](https://mistty.readthedocs.io/en/latest/shells.html).

## COMPATIBILITY

MisTTY requires Emacs 29.1 or later.

## CONTRIBUTING

See the [Contributing](https://mistty.readthedocs.io/en/latest/contrib.html) 
section of the documentation.
