Usage
=====

.. _installation:

Installation
------------

To use MisTTY, first install its package,

 - from `MELPA or MELPA Stable
   <https://melpa.org/#/getting-started>`_, using :kbd:`M-x
   package-install mistty`
 - from source using :kbd:`M-x package-vc-install https://github.com/szermatt/mistty`

You can then call it, as described in :ref:`launching`.

However, you'll likely want to configure it and add some key bindings
you use often in shells, for example:

.. code-block:: elisp

    (use-package mistty
      :bind (("C-c s" . mistty) ;; or mistty-in-project

             ;; bind here the shortcuts you'd like the
             ;; shell to handle instead of Emacs.
             :map mistty-prompt-map

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
regularly, you'll want to bind some of these to global shortcuts:

  .. index::
     pair: command; mistty-create
     pair: variable; explicit-shell-file-name
     pair: variable; shell-file-name

  - :kbd:`M-x mistty-create` launches a new interactive shell in a
    MisTTY buffer. The shell that is launched is the one that's
    configured on :kbd:`M-x configure-option explicit-shell-file-name`

    If :code:`explicit-shell-file-name` is unset, MisTTY falls back to
    :code:`shell-file-name`, then the environment variables
    :envvar:`ESHELL` and :envvar:`SHELL`.

    :kbd:`M-x customize-option display-comint-buffer-action` lets you
    configure how the buffer is displayed.

  .. index:: pair: command; mistty-create-other-window

  - :kbd:`M-x mistty-create-other-window` does the same, but opens the
    buffer in another window.

  .. index:: pair: command; mistty

  - :kbd:`M-x mistty` creates a new MisTTY buffer the first time it is
    called. Afterwards, it'll try to guess what's most appropriate,
    displaying an existing MisTTY buffer or creating a new one.

    With a prefix argument, this command always creates a new buffer.

  .. index:: pair: command; mistty-other-window

  - :kbd:`M-x mistty-other-window` does the same, but opens the buffer
    in another window.

  .. index::
     pair: command; mistty-in-project
     pair: function; mistty-project-init-kill-buffer

  - :kbd:`M-x mistty-in-project` creates a new MisTTY buffer in the
    root directory of the current project the first time it is called.
    Afterwards, it'll try to guess what's most appropriate, displaying
    an existing MisTTY buffer or creating a new one.

    With a prefix argument, this command always creates a new buffer.

    Note that if you want :kbd:`M-x project-kill-buffers` to kill such
    buffers, you'll want to execute
    :code:`mistty-project-init-kill-buffer` somewhere in your
    configuration or tell :kbd:`M-x configure-option
    project-kill-buffer-conditions` about MisTTY.

.. _term-vs-scroll:

Terminal vs. Scrollback
-----------------------

MisTTY buffers are split into two zones, with different behaviors:

The :dfn:`scrollback zone`, is where you can see commands that have
been executed and their output.

The :dfn:`terminal zone`, marked by a purple line on the left of the
window, is where you can type command and interact with the
terminal. In this zone, :kbd:`TAB` triggers the shell completion, if
available. With some shells, you'll see autosuggestions as you type.

The scrollback zone behaves as a normal Emacs buffer. You can modify
it as you see fit.

The terminal zone, on the other hand, limits what you can do: When a
shell is attached to the terminal, you can edit the command you're
about to run, but you can't edit the prompt itself - or rather, if you
do change the prompt, your change will be undone by the shell.

The terminal zone is where the magic happens: this is where you can
use a mix of Emacs and shell key bindings to edit the command
line. The trickiest part is choosing which key bindings you want Emacs
to handle and which key bindings you want the shell to handle.

By default, Emacs handles everything but a few key bindings are sent
directly to the terminal, bypassing Emacs:

- :kbd:`RET`, to ask the shell to run the command
- :kbd:`TAB`, to ask the shell to run command completion,
- :kbd:`C-a` to ask it to move the cursor to the beginning of the
  line, and
- :kbd:`C-e` to ask it to move the cursor to the end of the line.
- :kbd:`C-d` to ask it to either delete the next character or exit the
  program.
- :kbd:`M-p` to ask it to go up, or up the command history, sending
  :kbd:`C-p` to the terminal.
- :kbd:`M-n` to ask it to go down, or down the command history,
  sending :kbd:`C-n` to the terminal.
- :kbd:`M-r` to ask it to do backward history search, sending
  :kbd:`C-r` to the terminal.
- :kbd:`M-.` to ask the shell to insert the last history argument.

In addition, :kbd:`C-c C-c` sends the TERM signal to the terminal.

The program attached to the terminal decides what the actual effect of
these shortcuts is. Most shells and command-line editing tools
supports the shortcuts above by default, but they might not work
everywhere as expected.

.. warning::

    MisTTY will not work if you've configured your shell to turn on
    **VI mode** by default. Please **turn it off** before trying out
    MisTTY, for details on how to turn off VI mode only of MisTTY
    buffers and leave it on otherwise, check out the instructions in
    :ref:`shells` for details. VI mode must be turned off even if you
    just end up controlling it with VI commands using Evil.

To get the most out of MisTTY, it's worth it to take the time to
configure it to send to the terminal the shell key bindings that you
actually use and keep everything else behaving as usual for your Emacs
configuration.

.. index::
   pair: map; mistty-prompt-map
   pair: map; mistty-mode-map

To bind keys only in the terminal zone, bind them to
:code:`mistty-prompt-map`. To bind keys in both zones, bind them to
:code:`mistty-mode-map`. See examples below.

The following commands are useful to send key sequences to the current
shell or program controlling the terminal:

  .. index:: pair: command; mistty-send-key

  - The command :code:`mistty-send-key`, called interactively,
    forwards the key it was called from. It is meant to be bound to
    the shell key bindings you want to work in the terminal zone map,
    :code:`mistty-prompt-map`.

    For example, searching in the shell command history is usually
    bound to :kbd:`C-r`, MisTTY binds that to :kbd:`M-r`, like comint
    does, but if you'd like it to be accessible using the original key
    binding, you can do:

    .. code-block:: elisp

        (keymap-set mistty-prompt-map "C-r" #'mistty-send-key)

    If you'd prefer to have the key available in both the scrollback
    and terminal zones, bind it :code:`mistty-mode-map` instead.

    You can also pass arbitrary keys to :code:`mistty-send-key`, for
    example:

    .. code-block:: elisp

       (defun my-mistty-M-s (n)
         (interactive "p")
         (mistty-send-key n (kbd "M-s")))
       (keymap-set mistty-prompt-map "C-c a" #'my-mistty-M-s)

  .. index:: pair: command; mistty-send-last-key

  - The command :code:`mistty-send-last-key` forwards the last key
    combination of a sequence it was called from to the terminal. For
    example, :kbd:`C-c C-c` is bound to :code:`mistty-send-last-key`
    so that the terminal eventually just gets :kbd:`C-c`.

To just try things out, or for shell shortcuts you don't use
regularly, you can use the :kbd:`C-q` prefix to bypass Emacs key
bindings and send keys directly to the terminal. For example,
:kbd:`C-q <right>` sends a right arrow key press to the terminal
instead of moving the cursor.

If that's not enough,

  .. index:: pair: command; mistty-send-key-sequence

  - :kbd:`C-c C-q`, :kbd:`M-x mistty-send-key-sequence` sends all keys
    you press to the terminal until you press :kbd:`C-g`.


.. _navigation:

Navigating the scrollback zone
------------------------------

  .. index:: pair: command; mistty-end-of-line-goto-cursor

  - :kbd:`C-e C-e` moves the point back inside the prompt. This is
    handled by the interactive function
    :code:`mistty-end-of-line-or-goto-cursor`

  .. index:: pair: command; mistty-goto-cursor

  - :kbd:`M-x mistty-goto-cursor` also moves the point back inside the
    prompt. You can bind it to a custom shortcut if you don't like
    overloading C-e.

  .. index:: pair: command; mistty-previous-output

  - :kbd:`C-c C-p` or :kbd:`M-x mistty-goto-previous-output` goes to
    the beginning of the previous command output. This is useful to if
    the buffer has scrolled too far and you want to see it from the
    beginning.

  .. index:: pair: command; mistty-next-output

  - :kbd:`C-c C-n` or :kbd:`M-x mistty-goto-next-output` does the
    reverse, that is, it goes to the next command output.

  .. index:: pair: command; mistty-create-buffer-with-output

  - :kbd:`C-c C-r` or :kbd:`M-x mistty-create-buffer-with-output` creates
    a new buffer containing the last command output.

  .. index:: pair: command; mistty-goto-previous-input

  - :kbd:`M-x mistty-goto-previous-input` goes to the beginning of the
    previous command input, that is, the previous prompt. While this
    is a way of going back the command you've previously input, it's
    best to use the shell native command history, as discussed in
    :ref:`history`.

  .. index:: pair: command; mistty-goto-next-input

  - :kbd:`M-x mistty-goto-next-input` goes to the next command input.

.. _fullscreen:

Fullscreen Mode
---------------

MisTTY detects when a program such as :program:`less` or :program:`vi`
asks to run full screen and splits the MisTTY buffers into:

- a terminal buffer, which shows the program output and lets you
  interact with it. This is a term-mode buffer.
- a scrollback buffer, which shows the previous command lines and
  their output.

.. index:: pair: command; mistty-toggle-buffers

:kbd:`C-c C-j` or :kbd:`M-x mistty-toggle-buffers` switches between
these two.

When the program exits, the two buffers are again merged. Note that
the output of the full screen app isn't available in the scrollback.

.. _history:

Command History
---------------

MisTTY doesn't track command history. It relies instead on being able
to access the history of the different interactive command-line tools.

The command history available in most shells and command-line editing tools is
available in MisTTY using the following shortcuts:

- :kbd:`M-p` moves up command history
- :kbd:`M-n` moves down command history
- :kbd:`M-r` triggers a backward search in command history
- :kbd:`M-.` insert the last argument from command history

To get the same key bindings you'd get in a normal terminal, you can
bind :kbd:`C-p`, :kbd:`C-n`, or :kbd:`C-r` to :code:`mistty-send-key`
in the terminal zone of the MisTTY buffer. For example:

.. code-block:: elisp

    (keymap-set mistty-prompt-map "C-p" #'mistty-send-key)
    (keymap-set mistty-prompt-map "C-n" #'mistty-send-key)
    (keymap-set mistty-prompt-map "C-r" #'mistty-send-key)

.. _dirtrack:

Directory tracking and TRAMP
----------------------------

In order for Emacs to know your shell's current directory, the shell
has to tell MisTTY about it. This is usually done from the prompt.

:program:`Bash` does it by default, for local shells, when it detects
a terminal of type :code:`TERM=eterm-color` run from inside Emacs.

Other shells need to be configured to do the same. For more details,
see :ref:`shells`.

.. index:: pair: variable; mistty-allow-tramp-path

If you have configured TRAMP and know that the hosts you ssh into are
accessible with the default TRAMP method, you might consider allowing
MisTTY to report remote paths on :kbd:`M-x configure-option
mistty-allow-tramp-paths`.


Completion-at-point
-------------------

When in a MisTTY buffer, it's best to rely on the completion or
autosuggestions provided by the shell or other command-line tool
currently running, as they're more up-to-date and context-sensitive
than what Emacs can provide.

However, some form of Emacs-based completion can still be useful from
inside of a MisTTY buffer, to complete abbreviations, expand templates
or add emojis.

The following completion packages are known to work with MisTTY out of
the box, including auto-completion, if enabled:

- Emacs builtin `complete-in-region`
- `corfu <https://github.com/minad/corfu>`_
- `company-mode <http://company-mode.github.io>`_

Emacs `hippie-expand` also works. That's not completion, but it's
close.

Other packages might work or might be made to work with some efforts.
Auto-completion is usually the main challenge. See :ref:`autocomplete`
for some pointers. Please :ref:`file a bug <reporting>` if you
encounter issues with other completion packages.

Autosuggestions
^^^^^^^^^^^^^^^
.. index::
   pair: variable; mistty-wrap-capf-functions

:code:`completion-at-point` completes the text *around* the point.
This is generally convenient, but gets confused by shell
autosuggestions, available in Fish or ZSH.

What if you typed "com" and the shell helpfully suggests "completion"?
The buffer would look like: "com<>pletion", with <> representing
the point. :code:`completion-at-point` would then think you typed
"completion" and not suggest anything else.

To avoid that problem MisTTY modifies the functions it finds in
:code:`completion-at-point-functions` so that they just won't see
anything after the point when in the terminal region. In the example
above, they'd only complete "com", not "completion".

That is, :code:`completion-at-point` in the MisTTY terminal region
completes the text *before* the point.

If you don't like that or don't use a shell that supports
autosuggestions, you can turn this off with :kbd:`M-x customize-option
mistty-wrap-capf-functions`

Template Expansion
------------------

Template expansion and other form of long-running editing command
might be confused by the way MisTTY work in the terminal region. See
:ref:`lrc` for details.

The following template expansion packages are known to work with
MisTTY out of the box, if enabled:

- Emacs built-in `tempo` package
- `tempel <https://github.com/minad/tempel>`_
- `yasnippet <https://github.com/joaotavora/yasnippet>`_

Other packages might work or might be made to work with some efforts.
Please :ref:`file a bug <reporting>` if you encounter issues with
other packages.

Fancy prompts
-------------

MisTTY is known to work with powerline-shell prompts or `Tide, on Fish
<https://github.com/IlanCosman/tide>`_. This includes right prompts,
for the most part - though there might be temporary artifacts and
troublesome corner cases left.

If you suspect your shell prompt is causing issues, please first try
setting a traditional prompt to confirm, then :ref:`file a bug
<reporting>`, whatever the outcome.
