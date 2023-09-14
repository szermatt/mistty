Usage
=====

.. _installation:

Installation
------------

To use MisTTY, first install its package.

.. code-block:: elisp

    (package-vc-install "https://github.com/szermatt/mistty")

You can then call it, as described in :ref:`launching`, but you'll
likely want to configure it and add some key bindings you use often in
shells, for example:

.. code-block:: elisp

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

Read on for details on the commands and key bindings configured above.

.. _launching:

Launching
---------

To create a new interactive shell buffer in MisTTY mode, call
:kbd:`M-x mistty` or :kbd:`M-x mistty-create`. If you use MisTTY
regularly, you'll want to bind either of these to a global shortcut.

  - :kbd:`M-x mistty-create` launches a new interactive shell in a
    MisTTY buffer. The shell that is launched is the one that's
    configured by :kbd:`M-x configure-option explicit-shell-file-name`
    If this is unset, MisTTY falls back to :code:`shell-file-name`,
    the environment variables :envvar:`ESHELL` or :envvar:`SHELL`.
    :kbd:`M-x mistty-create-other-window` does the same, but opens the
    buffer in another window.

  - :kbd:`M-x mistty` behaves the same way the first time it is
    called. Afterwards, it tries to do the right thing, either jumping
    to an existing MisTTY buffer or creating a new one. :kbd:`M-x
    mistty-other-window` does the same, but opens a buffer in another
    window.

.. _term-vs-scroll:

Terminal vs. Scrollback
-----------------------

MisTTY buffers are split into two zones, with different behaviors:

 - The :dfn:`terminal zone`, where you can type command and interact
   with the terminal. In this zone, :kbd:`TAB` triggers the shell
   completion, if available. With some shells, you'll see
   autosuggestions as you type. The terminal zone is marked by a
   purple line on the left of the window.

 - The :dfn:`scrollback zone`, where you can see commands that have
   been executed and their output.

The scrollback zone behaves as a normal Emacs buffer. You can modify
it as you see fit without interference.

The terminal zone limits what you can do, as a terminal application
would. When a shell is attached to the terminal, you can edit the
command you're about to run, but you can't edit the prompt - or
rather, if you edit the prompt, your change will soon be undone.

The terminal zone is where the magic happens, where you can a mix of
Emacs and shell key bindings to edit the command. The trickiest part
is choosing which key bindings you want Emacs to handle and which key
bindings you want the shell to handle.

By default, a few key bindings are sent directly to the terminal,
bypassing Emacs:

- :kbd:`RET`, to ask the shell to run the command
- :kbd:`TAB`, to ask the shell to run command completion,
- :kbd:`C-a` to ask it to move the cursor to the beginning of the
  line, and
- :kbd:`C-e` to ask it to move the cursor to the end of the line.
- :kbd:`C-d` to ask it to either delete the next character or exit the
  program.

In addition, :kbd:`C-c C-c` sends the TERM signal to the terminal.

The program attached to the terminal decides what the actual effect of
these shortcuts is. Most shells and command-line editing tools
supports these by default, but they might not work everywhere as
expected.

.. warning::

    MisTTY will not work if you've configured your shell to turn on VI
    mode by default. Please turn it off before trying out MisTTY, for
    details on how to turn off VI mode only of MisTTY buffers and
    leave it on otherwise, if that's what you prefer. Checkout the
    instructions in :ref:`shells` for details. You need to do that for
    MisTTY to work at all, even if you'll just end up controlling it
    with VI commands using Evil.

To get the most out of MisTTY, it's worth it to take the time to
configure it forward the shell key bindings that you actually use to
the terminal and keep everything else behaving as usual for your Emacs
configuration.

  - The interactive function :code:`mistty-send-key` forwards the key
    it was called from. It is meant to be bound to the shell key
    bindings you want to work in the terminal zone map,
    :code:`mistty-prompt-map`.

    For example, moving up the shell's command history is usually
    bound to :kbd:`C-p` and searching in the shell command history to
    :kbd:`C-r`, so if you'd like to access these from a MisTTY buffer,
    you'd do the following:

    .. code-block:: elisp

        (keymap-set mistty-prompt-map "C-p" #'mistty-send-key)
        (keymap-set mistty-prompt-map "C-r" #'mistty-send-key)

    If you'd prefer to have the key available in both the scrollback
    and terminal zones, bind it :code:`mistty-mode-map` instead.

  - The interactive function :code:`mistty-send-last-key` forwards the
    last key combination of a sequence it was called from to the
    terminal. For example, :kbd:`C-c C-c` is bound to
    :code:`mistty-send-last-key` so that the terminal eventually just
    gets :kbd:`C-c`.

To just try things out, or for shell shortcuts you don't use
regularly, use the :kbd:`C-q` prefix, which sends the next key
directly to the terminal. For example, :kbd:`C-q <right>` sends a
right arrow key press to the terminal instead of moving the cursor.

If that's not enough:

  - :kbd:`C-c C-q`, :kbd:`M-x mistty-send-key-sequence` sends all
    keys you press to the terminal until there's an error or you press
    :kbd:`C-g`.

.. _navigation:

Navigating the scrollback zone
------------------------------

  - :kbd:`C-e C-e` moves the point back inside the prompt. This is
    handled by the interactive function
    :code:`mistty-end-of-line-or-goto-cursor`

  - :kbd:`M-x mistty-goto-cursor` also moves the point back inside the
    prompt. You can bind it to a custom shortcut if you don't like
    overloading C-e.

  - :kbd:`C-c C-p` or :kbd:`M-x mistty-goto-previous-output` goes to
    the beginning of the previous command output. This is useful to if
    the buffer has scrolled too far and you want to see it from the
    beginning.

  - :kbd:`C-c C-n` or :kbd:`M-x mistty-goto-next-output` does the
    reverse, that is, it goes to the next command output.

  - :kbd:`M-x mistty-goto-previous-input` goes to the beginning of the
    previous command input, that is, the previous prompt. While this
    is a way of going back the command you've previously input, it's
    best to use the shell native command history, as discussed in
    :ref:`history`.

  - :kbd:`M-x mistty-goto-next-input` goes to the next command input.

.. _fullscreen:

Fullscreen Mode
---------------

MisTTY detects when a program such as :program:`less` or :program:`vi`
asks to run full screen and splits the MisTTY buffers into:

- a terminal buffer, which shows the program output and lets you interact
  with it. This is a normal term-mode buffer.
- a scrollback buffer, which shows the previous command lines and
  their output.

:kbd:`C-c C-j` or :kbd:`M-x mistty-toggle-buffers` switches between
these two.

When the program exits, the two buffers are again merged. Note that
the output of the full screen app isn't available in the scrollback.

.. _history:

Command History
---------------

MisTTY doesn't track command history. It relies on being able to
access the history of the different interactive command-line tools.

The command history available in most shells and command-line editing tools is
available in MisTTY using the following shortcuts:

- :kbd:`C-q C-p` Move up command history
- :kbd:`C-q C-n` Move down command history
- :kbd:`C-q C-r` Backward search command history

If you can do without :kbd:`C-p`, :kbd:`C-n`, or :kbd:`C-r` in the
terminal zone of the MisTTY buffer, it's worth sending them directly
to the shell, doing, for example:

.. code-block:: elisp

    (keymap-set mistty-prompt-map "C-p" #'mistty-send-key)
    (keymap-set mistty-prompt-map "C-n" #'mistty-send-key)
    (keymap-set mistty-prompt-map "C-r" #'mistty-send-key)

Or by calling :code:`use-package` as shown in :ref:`installation`.

.. _dirtrack:

Directory tracking and TRAMP
----------------------------

In order for Emacs to know your shell's current directory, the shell
has to tell MisTTY about it. This is usually done from the prompt.

:program:`Bash` does it by default, for local shells, when it detects
a terminal of type :code:`TERM=eterm-color` run from inside Emacs.
Other shells can be configured to do the same. See the :ref:`bash`,
:ref:`zsh` or :ref:`fish` section for details.

If you have configured TRAMP and know that the hosts you ssh into are
accessible with the default TRAMP method, you might consider allowing
MisTTY to report remote paths on :kbd:`M-x configure-option
mistty-allow-tramp-paths`

