.. _shells:

Shells
======

.. _bash:

Bash
----

A recent version of Bash is preferable. Bash 5.1 and later work best,
as bracketed paste mode is on by default in that version.

Older versions of Bash work, but with limitations, and it might
behaves unexpectedly when yanking text containing special characters.

To use Bash between 4.4 and 5.0 without these limitations, add the
following to your :file:`.inputrc`:

.. code-block::

  set enable-bracketed-paste on


Multi-line prompts
^^^^^^^^^^^^^^^^^^

When you press :kbd:`RET` on an incomplete command, :program:`bash`
has the annoying habit of starting a secondary prompt which doesn't
let you go back to the previous line with the default keybindings.

To work around that, type :kbd:`S-<return>` instead of :kbd:`RET`
while on the terminal zone of a MisTTY buffer. This sends a newline
without running the command. You'll then end up with one multi-line
prompt that you can edit normally. This requires Bash 5.1 or an
earlier version with bracketed paste mode turned on.

You'll get the same effect if you yank a multi-line command while in a
prompt or go up the command history to a previous multi-line command.

Directory tracking
^^^^^^^^^^^^^^^^^^

Recent versions of :program:`bash` already send the current directory
when they detects that it's called from Emacs with
:code:`TERM=eterm-color`. This works fine for local shell as well as remote
shells run with TRAMP.

If you ssh into a host from an existing MisTTY buffer, however,
:program:`bash` will not send the remote directory. If you want this
use case to work, extend your prompt to send out an OSC7 sequence to
have :program:`bash` send the current directory and hostname to
MisTTY.

To do that, you might add the following to :file:`~/.bashrc`:

.. code-block:: bash

    if [ "$TERM" = "eterm-color" ]; then
      PS1='\e]7;file://$HOSTNAME$PWD\e\\\\'$PS1
    fi

Such sequence are either ignored or understood by most terminals, so
you don't absolutely need to check TERM.

VI mode
^^^^^^^

To communicate with :program:`bash`, MisTTY requires the shell to be
in its default editing mode, that is, the emacs mode. **Please make
sure you haven't put readline or bash in vi mode before trying out
MisTTY.**

To turn on vi mode in readline everywhere but MisTTY, you can add
something like the following into :file:`~/.inputrc`:

.. code-block::

    $if term=eterm
      set editing-mode emacs
    $else
      set editing-mode vi
    $endif

Or, in bash :file:`~/.bashrc`:

.. code-block:: bash

   if [ "$TERM" != "eterm-color" ]; then
     set -o vi
   fi


.. _fish:

Fish
----

A recent version of Fish is preferable. MisTTY relies on bracketed
paste mode, on by default, so it should not be turned off.

Autosuggestions
^^^^^^^^^^^^^^^

:program:`fish` autosuggestions work normally in MisTTY. However, the
usual way of accepting an autosuggestion, pressing the right arrow
key, is very inconvenient as this is bound to an Emacs point movement.

The recommended way of accepting an autosuggestion in MisTTY is to
type :kbd:`C-e`. This works in normal terminals as well.

Command History
^^^^^^^^^^^^^^^

To make full use of :program:`fish` command history, you'll want to
forward some additional shortcuts to fish:

.. code-block:: elisp

    (keymap-set mistty-prompt-map "M-<up>" #'mistty-send-key)
    (keymap-set mistty-prompt-map "M-<down>" #'mistty-send-key)
    (keymap-set mistty-prompt-map "M-<left>" #'mistty-send-key)
    (keymap-set mistty-prompt-map "M-<right>" #'mistty-send-key)

This can also be done by calling :code:`use-package` as shown in
:ref:`installation`.

When in reverse history search mode, :program:`fish` enters a mode
that lets you select an option using the arrow keys. To send
up/down/left/right directly to :program:`fish`, you can:

- use :kbd:`M-p` to go up and :kbd:`M-n` to go down, or, if you prefer

- use :kbd:`C-q <up>` :kbd:`C-q <down>` :kbd:`C-q <left>` :kbd:`C-q <right>`, or even

- :kbd:`C-c C-q` to temporarily send all send key presses to :program:`fish`


Directory tracking
^^^^^^^^^^^^^^^^^^

Extend your prompt to send out an OSC7 sequence to have
:program:`fish` send the current directory and hostname to MisTTY. To
do that, you might add the following to
:file:`~/.local/config/fish/config.fish`:

.. code-block:: fish

    if [ "$TERM" = "eterm-color" ]
      function osc7_send_pwd --on-event fish_prompt
        printf "\e]7;file://%s%s\e\\\\" (hostname) "$PWD"
      end
    end

such sequence are either ignored or understood by most terminals. You
might already have it set up.

Multi-line prompts
^^^^^^^^^^^^^^^^^^

:program:`fish` automatically detects when a command is incomplete
when you type :kbd:`RET` and launches a multi-line prompt, which
MisTTY knows to navigate.

.. index:: pair: variable; mistty-skip-empty-spaces

The cursor jumps over indent space while on such a prompt, just like
in a normal terminal. :kbd:`M-x customize-option
mistty-skip-empty-spaces` allows you to turn that on or off in a
MisTTY buffer.

VI mode
^^^^^^^

To communicate with :program:`fish`, MisTTY requires the shell to be
in its default editing mode, that is, the emacs mode. **Please make
sure you haven't put readline or bash in vi mode before trying out
MisTTY.**

To turn on vi mode in readline everywhere but in MisTTY, you can add
something like the following in :file:`~/.zshrc`:

.. code-block:: fish

   if [ "$TERM" != "eterm-color" ]
     fish_vi_key_bindings
   end

.. _zsh:

Zsh
---

A recent version of Zsh is preferable.

Zsh supports bracketed paste, which MisTTY relies on, since version
5.1. Older versions will work, but with limitations, and you might get
unexpected results if you yank text containing special characters.

Autosuggestions
^^^^^^^^^^^^^^^

Fish-like :program:`zsh` autosuggestions work normally in MisTTY, if
you've turned these on. However, the usual way of accepting an
autosuggestion, pressing the right arrow key, is very inconvenient as
this is normally bound to an Emacs point movement.

The recommended way of accepting an autosuggestion in MisTTY is to
type :kbd:`C-e`. This works in normal terminals as well.

Directory tracking
^^^^^^^^^^^^^^^^^^

Extend your prompt to send out an OSC7 sequence to have
:program:`zsh` send the current directory and hostname to MisTTY. To
do that, you might add the following to :file:`~/.zshrc`:

.. code-block:: zsh

    if [ "$TERM" = "eterm-color" ]; then
        PS1='\e]7;file://$HOSTNAME$PWD\e\\\\'$PS1
    fi

such sequence are either ignored or understood by most terminals.

Multi-line prompts
^^^^^^^^^^^^^^^^^^

When you press :kbd:`RET` on an incomplete command, :program:`zsh`
has the annoying habit of starting a secondary prompt. MisTTY doesn't
know how to go back to the previous prompt from such a prompt.

To work around that, type :kbd:`S-<return>` instead of :kbd:`RET`
while on the terminal zone of a MisTTY buffer. This sends a newline
without running the command. You'll then end up with one multi-line
prompt that you can edit normally. This requires a version of Zsh that
supports bracketed paste mode, 5.1 or later.

You'll get the same effect if you yank a multi-line command while in a
prompt or go up the command history to a previous multi-line command.

VI mode
^^^^^^^

To communicate with :program:`zsh`, MisTTY requires the shell to be in
its default editing mode, that is, the emacs mode. **Please make sure
you haven't put readline or bash in vi mode before trying out
MisTTY.**

To turn on vi mode in readline everywhere but in MisTTY, you can add
something like the following in :file:`~/.zshrc`:

.. code-block:: zsh

   if [ "$TERM" != "eterm-color" ]; then
     bindkey -v
   fi
