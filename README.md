# MisTTY, a shell/comint alternative with a fully functional terminal

[![CI Status](https://github.com/szermatt/mistty/actions/workflows/CI.yml/badge.svg)](https://github.com/szermatt/mistty/actions/workflows/test.yml)
[![Documentation Status](https://readthedocs.org/projects/mistty/badge/?version=latest)](https://mistty.readthedocs.io/en/latest/?badge=latest)


MisTTY runs a shell interactively under Emacs 29.1 and later, just like
`M-x shell` does. 

In a MisTTY buffer, just like in a normal shell buffer, the usual
native Emacs movement and editing work. What *also* work is everything
that you normally only have in a terminal, such as TAB-completion and
native shell history.

MisTTY works well with Bash and ZSH, but it is especially well suited
to running [Fish](https://fishshell.com): you get autosuggestions,
completion in full colors. Here's what the end result might look like:

![screen grab](https://github.com/szermatt/mistty/blob/master/screengrab.gif?raw=true)

## INSTALLATION

> **The following is just a short introduction. Read the full documentation at https://mistty.readthedocs.io/en/latest/**

Here's one way of installing MisTTY and binding it to the key `C-c s`:

```elisp
(package-vc-install "https://github.com/szermatt/mistty")
(use-package mistty
  :bind (("C-c s" . mistty)
  
         ;; bind here the shortcuts you'd like the 
         ;; shell to handle instead of Emacs.
         :map mistty-prompt-map
         ;; all shells: go up/down in the shell history
         ("C-p" . mistty-send-key)
         ("C-r" . mistty-send-key)
         ;; bash: history-token-search-backward
         ("M-." . mistty-send-key)
         ;; fish: dir history, more history manipulation
         ("M-<up>" . mistty-send-key)
         ("M-<down>" . mistty-send-key)
         ("M-<left>" . mistty-send-key)
         ("M-<right>" . mistty-send-key)))
```

Keys bound to `mistty-send-key` are sent to the terminal, so the action is executed by the shell. Bind the keys you actually use in a shell and leave Emacs handle everything else.

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

If you call `M-x mistty` another time, you'll be taken back to any
existing MisTTY buffer - see its documentation. If you don't like this
behavior, use `M-x mistty-create` instead.

## COMPATIBILITY

MisTTY requires Emacs 29.1 or later.

## CONTRIBUTING

See the [Contributing](https://mistty.readthedocs.io/en/latest/contrib.html) 
section of the documentation.
