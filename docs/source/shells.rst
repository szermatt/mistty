.. _shells:

Shells
======

.. _bash:

Bash
----

A recent version of Bash is preferable. Bash 5.1 or later is
recommended.

MisTTY works best with shells that support bracketed paste. Without
bracketed paste support, MisTTY will still work, but might behaves
unexpectedly when yanking text containing special characters.

Bash 4.5 to 5.0 supports bracketed paste, but it must be turned
on in your :file:`.inputrc`, as follows:

.. code-block::

  set enable-bracketed-paste on

Bash versions older than 4.5 don't support bracketed paste.

Additionally, Bash versions older than 4.4 require extra setup to
enable directory tracking, as documented in :ref:`bash_dirtrack`.

Multi-line prompts in Bash
^^^^^^^^^^^^^^^^^^^^^^^^^^

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

Please be aware that when editing a multi-line command in Bash, MisTTY
may leave trailing spaces at the end of some lines. In situations
where these are significant, you will need to remove trailing spaces
using :kbd:`C-d` or :kbd:`DEL`.

.. _bash_dirtrack:

Directory tracking in Bash
^^^^^^^^^^^^^^^^^^^^^^^^^^

.. index:: pair: variable; mistty-set-EMACS

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

Versions of :program:`bash` older than 4.4 only enable directory
tracking if the env variable EMACS is set. You can have MisTTY set
this env variable when it starts a shell by going to `M-x
customize-option mistty-set-EMACS`. :code:`mistty-set-EMACS` also
works as a connection-local variable, to set the EMACS env variable
only on some hosts that use an old version of :program:`bash`.

For example:

.. code-block:: elisp

  (connection-local-set-profile-variables
   'profile-old-bash
   '((mistty-set-EMACS . t)
     (mistty-shell-command . ("/bin/bash" "-i"))))

  (connection-local-set-profiles '(:machine "oldhost.example.com")
   'profile-old-bash)
  (connection-local-set-profiles '(:protocol "docker")
   'profile-old-bash)"

VI mode in Bash
^^^^^^^^^^^^^^^

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

Fancy prompts in Bash
^^^^^^^^^^^^^^^^^^^^^

MisTTY is compatible with stylized prompts, such as those produced by
`powerline-go <https://github.com/justjanne/powerline-go>`_. However,
given the numerous variations in how these prompts are created, issues
may arise.

If you suspect that your shell prompt may be causing new problems,
begin by switching to a standard prompt to confirm this. Regardless of
the results, please report any bugs you find (:ref:`reporting`.)

.. _fish:

Fish
----

A recent version of Fish is preferable. MisTTY relies on bracketed
paste mode, on by default, so it should not be turned off.

Autosuggestions in Fish
^^^^^^^^^^^^^^^^^^^^^^^

:program:`fish` autosuggestions work normally in MisTTY. However, the
usual way of accepting an autosuggestion, pressing the right arrow
key, is very inconvenient as this is bound to an Emacs point movement.

The recommended way of accepting an autosuggestion in MisTTY is to
type :kbd:`C-e`. This works in normal terminals as well.

Command History in Fish
^^^^^^^^^^^^^^^^^^^^^^^

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

.. _fish_dirtrack:

Directory tracking in Fish
^^^^^^^^^^^^^^^^^^^^^^^^^^

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

Multi-line prompts in Fish
^^^^^^^^^^^^^^^^^^^^^^^^^^

:program:`fish` automatically detects when a command is incomplete
when you type :kbd:`RET` and launches a multi-line prompt, which
MisTTY knows to navigate.

.. index:: pair: variable; mistty-skip-empty-spaces

The cursor jumps over indent space while on such a prompt, just like
in a normal terminal. :kbd:`M-x customize-option
mistty-skip-empty-spaces` allows you to turn that on or off in a
MisTTY buffer.

VI mode in Fish
^^^^^^^^^^^^^^^

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

Fancy prompts in Fish
^^^^^^^^^^^^^^^^^^^^^

MisTTY is known to work with powerline-shell prompts or `Tide, on Fish
<https://github.com/IlanCosman/tide>`_. This includes right prompts,
for the most part - though there might be temporary artifacts and
troublesome corner cases left.

If you suspect your shell prompt is causing issues, please first try
setting a traditional prompt to confirm, then, whatever the outcome,
please file a bug (:ref:`reporting`.)

.. _zsh:

Zsh
---

A recent version of Zsh is preferable.

Zsh supports bracketed paste, which MisTTY relies on, since version
5.1. Older versions will work, but with limitations, and you might get
unexpected results if you yank text containing special characters.

Autosuggestions in Zsh
^^^^^^^^^^^^^^^^^^^^^^

Fish-like :program:`zsh` autosuggestions work normally in MisTTY, if
you've turned these on. However, the usual way of accepting an
autosuggestion, pressing the right arrow key, is very inconvenient as
this is normally bound to an Emacs point movement.

The recommended way of accepting an autosuggestion in MisTTY is to
type :kbd:`C-e`. This works in normal terminals as well.

.. _zsh_dirtrack:

Directory tracking in Zsh
^^^^^^^^^^^^^^^^^^^^^^^^^

Extend your prompt to send out an OSC7 sequence to have
:program:`zsh` send the current directory and hostname to MisTTY. To
do that, you might add the following to :file:`~/.zshrc`:

.. code-block:: zsh

  function osc7_precmd() {
    printf "\e]7;file://%s%s\e\\\\" "$HOSTNAME" "$PWD"
  }
  precmd_functions+=(osc7_precmd)

Such sequence are either ignored or understood by any well-behaved
terminals, so you shouldn't need to check the terminal.

Multi-line prompts in Zsh
^^^^^^^^^^^^^^^^^^^^^^^^^

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

Please be aware that when editing a multi-line command in Zsh, MisTTY
may leave trailing spaces at the end of some lines. In situations
where these are significant, you will need to remove trailing spaces
using :kbd:`C-d` or :kbd:`DEL`.

VI mode in Zsh
^^^^^^^^^^^^^^

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

Fancy prompts in Zsh
^^^^^^^^^^^^^^^^^^^^

MisTTY is compatible with fancy prompts, such as the ones created by
`powerlevel10k <https://github.com/romkatv/powerlevel10k>`_, though
there are some limitations.

To ensure proper functionality, keep the `prompt_sp` option enabled,
as MisTTY depends on it to identify the start of multi-line prompts.
If you prefer not to have any end-of-line (EOL) markers, set
`PROMPT_EOL_MARK` to an empty string.

Using right prompts can lead to issues that disrupt multi-line editing
in Zsh. For now, please disable this feature.

Transient prompts can interfere with MisTTY's commands, such as
`mistty-previous-output` (:kbd:`C-c C-p`) and
`mistty-create-buffer-with-output` (:kbd:`C-c C-r`). If these commands
are important to you, disable transient prompts when the `TERM` is set
to `eterm-color`.

If you suspect that your shell prompt is introducing new problems,
start by switching to a traditional prompt to verify this. Regardless
of the outcome, please report any bugs you encounter
(:ref:`reporting`.)

.. _ipython:

IPython
-------

Editing, and cursor movements should work out of the box with
:program:`ipython`, even in multi-line commands, *provided you use the
default prompts*.

.. index::
   pair: variable; mistty-move-vertically-regexps
   pair: variable; mistty-multi-line-continue-prompts

If you modified the :program:`ipython` prompts, you'll need to teach
MisTTY about these prompts for multi-line movement and editing to
work.

Go to :kbd:`M-x configure-option mistty-move-vertically-regexps` and
add to the list a regular expression that matches your prompt. Make
sure that your regular expression is specific to your IPython prompt,
as mistakenly matching with :program:`bash` or :program:`zsh` would
have rather catastrophic results.

Go to :kbd:`M-x configure-option mistty-multi-line-continue-prompts`
and add to the list a regular expression that matches your IPython
continue prompt, that is, the prompt that IPython adds to the second
and later lines of input. Again, be specific. The regular expression
shouldn't match any other prompts. You don't need to do anything here
if you configured IPython to not output any continue prompt.
