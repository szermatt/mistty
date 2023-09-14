MisTTY
======

**MisTTY** is a major mode for :program:`Emacs` 29.1 and up that runs
a shell inside of a buffer, similarly to comint mode. It is built on
top of :file:`term.el`.

:kbd:`M-x mistty` creates a buffer with an interactive shell. Inside
that buffer, you can move freely and use the usual Emacs commands and
editing tools to run shell command and work with their output.

In addition to these, you also have access to your shell's native
command and editing tools, including TAB-completion and
autosuggestions.

Commands that take over the :ref:`entire screen<fullscreen>`, such as
:command:`less` or :command:`vi` are also available.

MisTTY` is known to work with :program:`bash`, :program:`zsh` and
:program:`fish` on Linux and MacOS. It also supports non-shell
command-line programs, such as :program:`python`.

.. note::

   MisTTY is Beta software! It might not work with your setup our your
   tools. If you try it out and encounter issues, please take the time
   to file a bug describing what you did and what happened.

Contents
--------

.. toctree::

   usage
   shells
   extensions
   faq
   contrib
