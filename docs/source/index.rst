MisTTY
======

**MisTTY** is a major mode for :program:`Emacs` 29.1 and up that runs
a shell inside of a buffer, similarly to comint mode. It is built on
top of :file:`term.el`. Check out its project page at
https://github.com/szermatt/mistty.

:kbd:`M-x mistty` creates a buffer with an interactive shell. Inside
that buffer, you can move freely and use the usual Emacs commands and
editing tools to run shell command and work with their output. See
:ref:`launching` for details.

In addition to these, you also have access to your shell's native
command and editing tools, including TAB-completion and
autosuggestions.

Commands that take over the :ref:`entire screen<fullscreen>`, such as
:command:`less` or :command:`vi` are also available.

.. only:: builder_html

  MisTTY works well with Bash and ZSH, but it is especially well suited
  to running `Fish <https://fishshell.com>`_: you get autosuggestions,
  completion in full colors. Here's what the end result might look like:

  .. image:: ../../screengrab.gif
    :width: 600
    :alt: Screen grab showing MisTTY in action

MisTTY is known to work with :program:`bash`, :program:`zsh` and
:program:`fish` on Linux and MacOS. It also supports non-shell
command-line programs, such as :program:`python`.

The latest version of this documentation is available at
https://mistty.readthedocs.io/en/latest/.  Once MisTTY is installed,
this documentation can be accessed from inside Emacs using :kbd:`M-x
info gmistty`

.. note::

   If you encounter issues, please take the time to file a bug. See
   :ref:`reporting` for details.

Contents
--------

.. toctree::

   usage
   shells
   extensions
   faq
   contrib
