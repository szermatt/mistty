Shells
======

.. _bash:

Bash
----

Multi-line prompts
^^^^^^^^^^^^^^^^^^

When you press :kbd:`RET` on an incomplete command, :program:`bash`
has the annoying habit of starting a secondary prompt which doesn't
let you go back to the previous line with the default keybindings.

To work around that, type :kbd:`S-<return>` instead of :kbd:`RET`
while in MisTTY's terminal zone. This sends a newline without running
the command. You'll then end up with one multi-line prompt that you
can fully edit.

You can get the same effect if you yank a multi-line command while in
a prompt or go up the command history to a previous multi-line
command.

VI mode
^^^^^^^

Directory tracking
^^^^^^^^^^^^^^^^^^

:program:`bash` already sends the current directory when it detects
that it's called from Emacs with :code:`TERM=eterm-color` so as long
as you don't want TRAMP remote paths, you don't have to do anything.

Extend your prompt to send out an OSC7 sequence to have
:program:`bash` send the current directory and hostname to MisTTY. To
do that, you might add the following to :file:`~/.bashrc`:

.. code-block:: bash

    PS1='\e]7;file://$HOSTNAME$PWD\e\\\\'$PS1

such sequence are either ignored or understood by most terminals. You
might already have it set up.

.. _zsh:

Zsh
---

Directory tracking
^^^^^^^^^^^^^^^^^^

Extend your prompt to send out an OSC7 sequence to have
:program:`zsh` send the current directory and hostname to MisTTY. To
do that, you might add the following to :file:`~/.zshrc`:

.. code-block:: zsh

    PS1='\e]7;file://$HOSTNAME$PWD\e\\\\'$PS1

such sequence are either ignored or understood by most terminals. You
might already have it set up.

Multi-line prompts
^^^^^^^^^^^^^^^^^^

When you press :kbd:`RET` on an incomplete command, :program:`bash`
has the annoying habit of starting a secondary prompt. MisTTY doesn't
know how to go back to the previous prompt from such a prompt.

To work around that, type :kbd:`S-<return>` instead of :kbd:`RET`
while in MisTTY's terminal zone. This sends a newline without running
the command. You'll then end up with one multi-line prompt that you
can fully edit.

You can get the same effect if you yank a multi-line command while in
a prompt or go up the command history to a previous multi-line
command.

VI mode
^^^^^^^

.. _fish:

Fish
----

Directory tracking
^^^^^^^^^^^^^^^^^^

Extend your prompt to send out an OSC7 sequence to have
:program:`fish` send the current directory and hostname to MisTTY. To
do that, you might add the following to :file:`~/.local/config/fish/config.fish`:

.. code-block:: fish

    function osc7_send_pwd --on-event fish_prompt
      printf "\e]7;file://%s%s\e\\\\" (hostname) "$PWD"
    end

such sequence are either ignored or understood by most terminals. You
might already have it set up.

Multi-line prompts
^^^^^^^^^^^^^^^^^^

:program:`fish` automatically detects when a command is incomplete
when you type :kbd:`RET` and launches a multi-line prompt, which
MisTTY knows to navigate.

The cursor jumps over indent space while on such a prompt, just like
in a normal terminal. :kbd:`M-x customize-option
mistty-skip-empty-spaces` allows you to turn that on or off.

VI mode
^^^^^^^


