Writing Extensions
==================

.. _osc:

OSC Sequences
-------------

OSC are “operating system command” control sequences. MisTTY supports
some of these sequences and ignores the others.

By default, the supported sequences are OSC 2 (set window title), 7
(directory tracking, already mentioned in :ref:`dirtrack`) and 8
(hyperlinks), thanks to :file:`ansi-osc.el`.

To add more, register handlers to :code:`mistty-osc-handlers`. The
format is the same as the handlers supported for
:code:`ansi-osc-handlers` and can usually be used interchangeably.

When working on OSC handlers for MisTTY, it's important to keep the
following in mind: MisTTY separate buffers for the terminal (a
:code:`term-mode` buffer) and for MisTTY itself. The OSC handlers run
in the term-mode buffer.

One consequence of this is that if you set a buffer-local variable in
a handler, it won't be available in the MisTTY buffer unless you
register it to :kbd:`M-x configure-option mistty-variables-to-copy`

MisTTY provides helpers for writing OSC handlers that set text
properties:

- The function :code:`mistty-register-text-properties` registers a set
  of text properties to set on any text written to the terminal until
  :code:`mistty-unregister-text-properties` is called with the
  same argument.

.. _custom-commands:

Writing Your Own Commands
-------------------------

You might find the following functions useful if you'd like to write
commands that extend MisTTY's behavior:

- :code:`mistty-send-string` sends a string to the terminal,
  unmodified. The string that is sent appear only after the function
  return - and it might not ever appear at all depending on the
  application attached to the terminal. This is used to implement
  :code:`mistty-sudo` for example.

- :code:`mistty-on-prompt-p` returns non-nil if the given position is
  inside of a prompt MisTTY is aware of. This is useful for writing
  commands that behave differently on a prompt than on program output,
  even while inside of the terminal zone. It is used to implement
  :code:`mistty-beginning-of-line` for example.

- :code:`mistty-maybe-realize-possible-prompt` might be useful to call
  in your commands to tell MisTTY that there's likely a prompt at the
  current pointer position.

- :code:`mistty-before-position` not only check whether there's a
  prompt at the position, but also attempt to move the terminal cursor
  to that position.

.. _term-keymap:

Terminal Keymap
---------------

To forward a key binding to the application attached to the terminal
`mistty-send-key` first needs to convert that key binding to something
applications will understand. The translation is done by
:code:`mistty-translate-key`.

    The function :code:`mistty-translate-key` takes an Emacs key
    binding, as returned by `kbd` and returns a string containing the
    sequence of characters that correspond to that key to send to the
    application tied to the terminal.

The default terminal keymap used by MisTTY mimics :program:`xterm` key
bindings. You might extend it or change it by changing the map
:code:`mistty-term-key-map`.

For example, you can change the string that correspond to the first
function keys from their default ("\\eOP" - "\\eOS") as follows:

.. code-block:: elisp
                
    (define-key mistty-term-key-map (kbd "<f1>") "\e[11~")
    (define-key mistty-term-key-map (kbd "<f2>") "\e[12~")
    (define-key mistty-term-key-map (kbd "<f3>") "\e[13~")
    (define-key mistty-term-key-map (kbd "<f4>") "\e[14~")


:kbd:`M-x mistty-reverse-input-decode-map` generates alternative
values for :code:`mistty-term-key-map` for you if you'd like mimic
another set of key bindings than xterm, for example, to generate a
keymap that simulates rxvt, you might do:

.. code-block:: elisp
                    
   (load-library "term/rxvt.el")
   (mistty-reverse-input-decode-map rxvt-function-map)
   
:file:`mistty-reverse-input-decode-map.el` is not included into the
distribution; it's only available on `github
<https://github.com/szermatt/mistty/tree/master/extras>`_.
