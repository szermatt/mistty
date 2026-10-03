MisTTY
======

**MisTTY** is a terminal application for Emacs 29.1 and up that allows
editing prompts using Emacs commands, like in the `shell` command,
but in a full-featured, modern terminal.

Terminal support is provided by the `alacritty
<https://alacritty.org>`_ library, with the help of an Emacs module.
Without that module, MisTTY falls back to using the more limited Emacs
built-in terminal `eterm`. To avoid that install the module with `M-x
mistty-install` or `M-x mistty-install-dwim`. For more details, head
over to :ref:`installation`.

Once installed, :kbd:`M-x mistty` creates a buffer with an interactive
shell. (:ref:`launching`)


In that buffer you can move around freely and run any Emacs command
you want - until you press TAB and end up with the native completion
or notice the shell autosuggestions. With MisTTY you have access to
both Emacs and the shell commands and editing tools.

Additionally, commands that take over the entire screen, such as
`less` or `vi` also work, temporarily taking over the terminal zone
and keyboard.

.. only:: builder_html

  MisTTY works well with Bash and ZSH, but it is especially well
  suited to running `Fish <https://fishshell.com>`_: you get
  autosuggestions, completion in full colors, directory tracking with
  OSC7 and prompt detection with OSC133 out of the box. Here's what
  the end result might look like:

  .. image:: ../../screengrab.gif
    :width: 600
    :alt: Screen grab showing MisTTY in action

MisTTY is known to work on Linux and MacOS. It also supports non-shell
command-line programs, such as python.

Special configuration isn't absolutely needed, as MisTTY tries to
support supports any command-line programs with a prompt in a
reasonable way, like python or ipython.

The latest version of this documentation is available at
https://mistty.readthedocs.io/en/latest/.  Once MisTTY is installed,
this documentation can be accessed from inside Emacs using :kbd:`M-x
info g mistty`

.. note::

   If you encounter issues, please take the time to file a bug. (:ref:`reporting`)

Comparison with other packages
------------------------------

As its core, MisTTY is a frontend to a terminal emulator. Terminal
emulation is provided by the `alacritty
library <https://github.com/alacritty/alacritty>`_, which emulates xterm
and has `support for modern terminal
extensions <https://alacritty.org/misc-alacritty-escapes.html>`_.

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

Contents
--------

.. toctree::

   usage
   shells
   extensions
   faq
   contrib
