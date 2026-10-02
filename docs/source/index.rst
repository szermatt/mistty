MisTTY
======

**MisTTY** is a major mode for :program:`Emacs` 29.1 and up that runs
a shell inside of a buffer, similarly to comint mode. It is built on
top of :program:`eterm`, the terminal emulator built into Emacs, or
the `alacritty <https://alacritty.org/>`_ library. Check out its
project page at https://github.com/szermatt/mistty.

:kbd:`M-x mistty` creates a buffer with an interactive shell. (:ref:`launching`)

MisTTY feels very much like comint mode: you can move around freely
and run any Emacs command you want - until you press TAB and end up
with the native completion or notice the shell autosuggestions. With
MisTTY you have access to both Emacs and the shell commands and
editing tools.

Additionally, commands that take over the entire screen such as
:command:`less` or :command:`vi` also work, temporarily taking over
the terminal zone. See :ref:`modes`.

.. only:: builder_html

  MisTTY works well with Bash and ZSH, but it is especially well
  suited to running `Fish <https://fishshell.com>`_: you get
  autosuggestions, completion in full colors. Here's what the end
  result might look like:

  .. image:: ../../screengrab.gif
    :width: 600
    :alt: Screen grab showing MisTTY in action

MisTTY is known to work on Linux and MacOS. It also supports non-shell
command-line programs, such as :program:`python`.

The latest version of this documentation is available at
https://mistty.readthedocs.io/en/latest/.  Once MisTTY is installed,
this documentation can be accessed from inside Emacs using :kbd:`M-x
info g mistty`

.. note::

   If you encounter issues, please take the time to file a bug. (:ref:`reporting`)

Comparison with other packages
------------------------------

MisTTY is a frontend to a terminal emulator, currently either
:program:`eterm`, a pure elisp terminal emulator built into Emacs, or
:program:`alacritty` using a custom module built against the
`alacritty library <https://github.com/alacritty/alacritty>`_

Its goal is to make it more convenient to use while inside of Emacs
and better integrate with Emacs itself.

MisTTY has some similarities with `coterm
<https://github.com/emacsmirror/coterm>`_; it offers the same switch
between full-screen and line mode.

:program:`Coterm`, :program:`term.el` and :program:`eat` all have a
line mode, just like :program:`comint` does, which allows you to edit
a command line as a whole before sending it to the shell. While in
line mode, rendering is done by Emacs and editing commands are Emacs
commands. In constrast, with MisTTY, all rendering is done by the
shell through the terminal. This is why native shell completion and
autosuggestion is available with MisTTY and not in line modes and why
you can freely mix shell commands with Emacs commands while editing
the command line.

:program:`term.el` and :program:`eat` also have a char mode, where
rendering and command execution is handled by the shell, and editing
with Emacs isn't available. The difference with MisTTY is then that
MisTTY makes Emacs editing commands available when possible.

:program:`eat` also has a semi-char mode, which is the closest there
is to MisTTY. In that mode, Emacs movements commands are available.
However, Emacs commands that modify the buffer, aren't available to
edit the command line. In contrast, MisTTY allows Emacs to navigate to
and edit the whole buffer, then replays changes made to the
command-line.

Contents
--------

.. toctree::

   usage
   shells
   extensions
   faq
   contrib
