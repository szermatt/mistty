Extending MisTTY
================

.. _hooks:

Hooks
-----

mistty-mode-hook
^^^^^^^^^^^^^^^^

.. index::
   pair: variable; mistty-mode-hook
   pair: hook; mistty-mode-hook

The hook :code:`mistty-mode-hook` is called on every MisTTY buffer
just after creating the buffer and selecting a window for it but
before executing the shell, with the buffer selected.

If you have enabled autocomplete or autosuggestion globally, you might
want to disable it for MisTTY buffers from a function called by
:code:`mistty-mode-hook`.

This hook also provides a good time to rename the buffer, change its
directory or change environment variables, to be inherited by the
process.

For example, if you wanted a more generic name for the MisTTY buffers,
you could do:

.. code-block:: elisp

  (defun my-lets-call-it-shell ()
    (rename-buffer (generate-new-buffer-name "*shell*")))
  (add-hook 'mistty-mode-hook #'my-lets-call-it-shell)

.. index::
   pair: variable; mistty-interactive-insert-hook
   pair: hook; mistty-interactive-interactive

mistty-interactive-insert-hook
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:code:`mistty-interactive-insert-hook` is a hook that is called when
text is typed in the terminal region. It's not called, for example,
for text that is inserted or displayed by the shell.

This hook provides an appropriate time to trigger auto-completion UI,
which, by default, doesn't work in the terminal region, as discussed
in :ref:`autocomplete`.

Auto-completion doesn't work in the terminal region because it often
requires calling a post-command function. To work around that, in most
case, it's enough to just turn on the option :kbd:`M-x
customize-option mistty-simulate-self-insert-command`, which enables
the function :code:`mistty-self-insert-command`, called by this hook
by default.

This might not always work and have unintended effects, so you might
prefer to trigger the auto-completion UI yourself by adding your own
function to this hook and turning the above option off.

mistty-after-process-start-hook
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:code:`mistty-after-process-start-hook` is a normal hook called from
within a new MisTTY work buffer just after starting the process,
usually a shell. The process itself is available as
:code:`mistty-proc`. At the time this hook is called, the buffer is
typically empty, as no output from the process has been processed.

mistty-after-process-end-hook
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:code:`mistty-after-process-end-hook` is called from within a MisTTY
work buffer just after the process ended. The process is passed as an
argument to the hook and its status can be accessed using
:code:`process-status`.

This can be used to, for example, kill the MisTTY work buffer after
the shell exits successfully, with :code:`mistty-kill-buffer` or
:code:`mistty-kill-buffer-and-window`.

.. code-block:: elisp

  (add-hook 'mistty-after-process-end-hook
            'mistty-kill-buffer-and-window)

mistty-entered-fullscreen-hook
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

:code:`mistty-entered-fullscreen-hook` is a normal hook called from
within a MisTTY work buffer just after switching to fullscreen mode.

In this mode, :code:`mistty-fullscreen` is non-nil and user commands
run within the terminal buffer, available as
:code:`mistty-term-buffer`. The work buffer is kept, but usually
buried until :code:`mistty-toggle-buffers` is called.

mistty-left-fullscreen-hook
^^^^^^^^^^^^^^^^^^^^^^^^^^^

:code:`mistty-left-fullscreen-hook` is a normal hook called from
within a MisTTY work buffer just after switching back to normal mode.

In this mode, :code:`mistty-fullscreen` is nil and user commands run
in the work buffer.

.. _ext_osc:

OSC Sequences
-------------

OSC are “operating system command” control sequences. MisTTY supports
some of these sequences and ignores the others.

By default, the supported sequences are OSC 2 (set window title), 7
(directory tracking, already mentioned in :ref:`dirtrack`) and 8
(hyperlinks), thanks to :file:`ansi-osc.el`.

.. index:: pair: variable; mistty-osc-handlers

To add more, register handlers to :code:`mistty-osc-handlers`. The
format is the same as the handlers supported for
:code:`ansi-osc-handlers` and can usually be used interchangeably.

When working on OSC handlers for MisTTY, it's important to keep the
following in mind: MisTTY separate buffers for the terminal (a
:code:`term-mode` buffer) and for MisTTY itself. The OSC handlers run
in the term-mode buffer.

.. index:: pair: variable; mistty-variables-to-copy

One consequence of this is that if you set a buffer-local variable in
a handler, it won't be available in the MisTTY buffer unless you
register it to :kbd:`M-x configure-option mistty-variables-to-copy`

MisTTY provides helpers for writing OSC handlers that set text
properties:

.. index::
   pair: command; mistty-register-text-properties
   pair: command; mistty-unregister-text-properties

- The function :code:`mistty-register-text-properties` registers a set
  of text properties to set on any text written to the terminal until
  :code:`mistty-unregister-text-properties` is called with the
  same argument.

.. _custom-commands:

Writing Your Own Commands
-------------------------

You might find the following functions useful if you'd like to write
commands that extend MisTTY's behavior:

.. index:: pair: function; mistty-mode

(mistty-mode): function
  This function sets the major mode of the current buffer to
  mistty-mode. This only useful when followed by a call to
  ``mistty-exec``, described below.

.. index:: pair: function; mistty-exec

(mistty-exec PROGRAM &optional :width WIDTH :height HEIGHT)
  This functions starts PROGRAM in the current buffer, which must
  be a ``mistty-mode`` buffer.

  PROGRAM is normally a list of executable and its argument. It can
  also be a string containing only the executable, if no arguments
  to be set.

  It is a good idea to display the buffer before calling this
  function, because the size of the terminal when PROGRAM is started
  is taken from the windows displaying the buffer.

  Example:

  .. code-block:: elisp

    (with-current-buffer (generate-new-buffer "*terminal*")
      (mistty-mode)
      (pop-to-buffer (current-buffer))
      (mistty-exec '("bash" "-i")))


  If the buffer isn't displayed, the terminal size is taken from the
  currently selected window, which might not be what you want. You can
  also set an arbitrary terminal size by passing the :width and
  :height optional keyword arguments and calling
  ``mistty-terminal-size-tracks-windows`` once the buffer has been
  tied to a window of a reasonable size, but that might cause a
  visible terminal refresh.

.. index:: pair: function; mistty-send-string

(mistty-send-string STR): function
  This function sends a string to the terminal, unmodified. The string
  that is sent appear only after the function return - and it might
  not ever appear at all depending on the application attached to the
  terminal. This is used to implement :code:`mistty-sudo` for example.

.. index:: pair: function; mistty-on-prompt-p

(mistty-on-prompt-p POS) : function
  This function returns non-nil if the POS is inside of a prompt
  MisTTY is aware of. This is useful for writing commands that behave
  differently on a prompt than on program output, even while inside of
  the terminal zone. It is used to implement
  :code:`mistty-beginning-of-line` for example.

.. index:: pair: function; mistty-maybe-realize-possible-prompt

(mistty-maybe-realize-possible-prompt &optional POS) : function
  This function might be useful to call in your commands to tell
  MisTTY that there's likely a prompt at the current pointer
  position or at POS.

.. index:: pair: function; mistty-before-position

(mistty-before-positional) : function
  This function not only checks whether there's a prompt at the
  position, but also attempt to move the terminal cursor to that
  position.

.. _term-keymap:

Terminal Keymap
---------------

.. index::
   pair: function; mistty-translate-key
   pair: map; mistty-term-key-map

To forward a key binding to the application attached to the terminal
`mistty-send-key` first needs to convert that key binding to something
applications will understand. The translation is done by
:code:`mistty-translate-key`.

mistty-translate-key : function
    This function takes an Emacs key binding, as returned by `kbd` and
    returns a string containing the sequence of characters that
    correspond to that key to send to the application tied to the
    terminal.

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

.. index:: pair: function; mistty-reverse-input-decode-map

mistty-reverse-input-decode-map: function
    This function generates alternative values for
    :code:`mistty-term-key-map` for you if you'd like mimic another
    set of key bindings than xterm, for example, to generate a keymap
    that simulates rxvt, you might do:

    .. code-block:: elisp

       (load-library "term/rxvt.el")
       (mistty-reverse-input-decode-map rxvt-function-map)

:file:`mistty-reverse-input-decode-map.el` is not included into the
distribution; it's only available on `github
<https://github.com/szermatt/mistty/tree/master/extras>`_.

.. _autocomplete:

Auto-complete
-------------

.. index::
   pair: variable; mistty-simulate-self-insert-command

Auto-complete is a completion UI that shows up automatically after
some delay, without having to call `completion-at-point`. This used
not to work in MisTTY terminal region. The hook
:code:`mistty-simulates-self-insert-command` was introduced to fix
that. See :code:`mistty-interactive-insert-hook` in :ref:`hooks`.

By default this hook calls the buffer :code:`pre-command-hook` and
:code:`post-command-hook` with :code:`this-command` set to
:code:`self-insert-command`, as this is the way auto-complete is
normally triggered. This can be turned off if necessary using the
option on :kbd:`M-x customize-option
mistty-simulate-self-insert-command`.

If the behavior described above doesn't work for some packages, it
should be possible to build a custom bridge between this hook and the
auto-completion package.

.. _lrc:

Long-running commands
---------------------

In Emacs, most editing tools are run as a single Emacs command, but
some tools span multiple Emacs command, for example, when you expand a
snippet with `yasnippet <https://github.com/joaotavora/yasnippet>`_,
the snippet template is inserted into the buffer, together with
placeholders for you to fill some missing information.

Filling in a template is a series of Emacs commands, that, together,
have a single effect: to insert a snippet of text. MisTTY calls this a
long-running command.

When run in the terminal region, such long-running commands fail as
MisTTY sends the initial text to the shell, which echoes it back to be
redisplayed, possibly jumbling things and definitely destroying any
overlays.

To avoid such situations, MisTTY holds back sending text to the shell
until long-running commands are done. For that to work, MisTTY needs
to know when such command start and end.

You can tell whether MisTTY thinks a long-running command is active,
as it displays *CMD* in the modeline. You can also do it
programmatically:

.. index::
   pair: function; mistty-long-running-command-p

mistty-long-running-running-command-p : function
    This function returns non-nil if MisTTY thinks a long-running
    command is active.


.. index::
   pair: variable; mistty-detect-foreign-overlays
   pair: option; mistty-detect-foreign-overlays
   pair: variable; mistty-foreign-overlay-properties
   pair: option; mistty-foreign-overlay-properties

MisTTY detects some long-running commands by looking for overlays they
typically add to the buffer. This can be extended with :kbd:`M-x
customize-option mistty-foreign-overlay-properties` or turned off with
:kbd:`M-x customize-option mistty-detect-foreign-overlays`.

To add a new property to `mistty-foreign-overlay-properties`, start
the interactive command, look for overlays with `overlays-in` then get
their properties with `overlay-properties`. You can then choose, on
that list, a property or face that identifies the feature or package.

If you find yourself extending `mistty-foreign-overlay-properties`,
please add an issue to https://github.com/szermatt/mistty/issues/new
so it can be integrated into the next version.

Alternatively, as not all long-running commands that can be confused
by MisTTY use overlays, you might need to tell MisTTY about them.
MisTTY does it already for :code:`completion-in-region`.

.. index::
   pair: function; mistty-report-long-running-command

mistty-report-long-running-command : function
    This function can be called to tell MisTTY when a long-running
    command start and end. It's typically called from hooks provided
    by the package of the long-running command.

Here's an example of code that would detect
:code:`completion-in-region-mode` if MisTTY didn't already do it:

.. code-block:: elisp

    (defun my-completion-in-region ()
      (mistty-report-long-running-command
        'my-completion-in-region completion-in-region-mode))
    (defun my-detect-completion-in-region ()
       (add-hook 'completion-in-region-mode-hook
                 #'my-completion-in-region nil t))
    (add-hook 'mistty-mode-hook #'my-detect-completion-in-region)
