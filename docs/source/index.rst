MisTTY
======

**MisTTY** is a major mode for :program:`Emacs` 29.1 and up that runs
a shell inside of a buffer, similarly to comint mode. It is built on
top of :file:`term.el`. Check out its project page at
https://github.com/szermatt/mistty.

:kbd:`M-x mistty` creates a buffer with an interactive shell. See
:ref:`launching` for details.

MisTTY feels very much like comint mode: you can move around freely
and run any Emacs command you want - until you press TAB and end up
with the native completion or notice the shell autosuggestions. With
MisTTY you have access to both Emacs and the shell commands and
editing tools.

Additionally, commands that take over the :ref:`entire
screen<fullscreen>`, such as :command:`less` or :command:`vi` also
work, temporarily taking over the window, while scrollback remains
available in another buffer.

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
info gmistty`

.. note::

   If you encounter issues, please take the time to file a bug. See
   :ref:`reporting` for details.

Comparison with other packages
------------------------------

MisTTY isn't a terminal emulator, but rather a frontend to an existing
terminal emulator, the built-in :file:`term.el`. Its goal is to make
it more convenient to use while inside of Emacs and better integrate
with Emacs itself. In theory, other terminal emulators than
:file:`term.el` might be used as engine for MisTTY, such as
:program:`vterm` and :program:`eat`.

MisTTY has some similarities with :program:`coterm`; it offers the
same switch between full-screen and line mode.

:program:`Coterm`, :program:`ansi-term` and :program:`eat` all have a
line mode, just like :program:`comint` does, which allows you to edit
a command line as a whole before sending it to the shell. While in
line mode, rendering is done by Emacs and editing commands are Emacs
commands. In constrast, with MisTTY, all rendering is done by the
shell through the terminal. This is why native shell completion and
autosuggestion is available with MisTTY and not in line modes and why
you can freely mix shell commands with Emacs commands while editing
the command line.

:program:`ansi-term` and :program:`eat` also have a char mode, where
rendering and command execution is handled by the shell, and editing
with Emacs isn't available. The difference with MisTTY is then that
MisTTY makes Emacs editing commands available when possible.

Contents
--------

.. toctree::

   usage
   shells
   extensions
   faq
   contrib
