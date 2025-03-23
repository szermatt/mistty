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

And then launch it with :kbd:`M-x mistty`, as described in :ref:`launching`.

You'll likely want to eventually bind that to some shortcut:

.. code-block:: elisp

    (use-package mistty
      :bind (("C-c s" . mistty)))

and, unless you're using :program:`Bash`, configure directory tracking
for your shell (:ref:`dirtrack`), but read on for more details.

.. _launching:

Launching
---------

To create a new interactive shell buffer in MisTTY mode, call
:kbd:`M-x mistty`, which either creates a new shell or goes to an
existing MisTTY buffer, or :kbd:`M-x mistty-create`, which creates a
new MisTTY buffer.

Here's a quick list of the commands defined by the MisTTY package,
their behavior and arguments:

  .. index::
     pair: command; mistty-create
     pair: command; mistty
     pair: command; mistty-create-other-window
     pair: command; mistty-other-window
     pair: variable; mistty-shell-command
     pair: variable; explicit-shell-file-name
     pair: variable; shell-file-name
     pair: variable; mistty-buffer-name

mistty-create : command
    This command launches a new interactive shell in a
    MisTTY buffer in the current buffer's ``default-directory``.

    The shell that is launched is the one that's configured with
    :kbd:`M-x configure-option mistty-shell-command`. If
    ``mistty-shell-command`` is not set, MisTTY falls back to
    ``explicit-shell-file-name``, ``shell-file-name``, then
    the environment variables :envvar:`ESHELL` and :envvar:`SHELL`.

    With a prefix argument, this command asks for a directory for the
    new shell, instead of using the current buffer's current
    directory. This is particularly useful if you want to run shell
    on a remote host. (:ref:`remote_shells`)

    By default, new buffers are called "\*mistty\*", or, if you use
    TRAMP "\*mistty\@hostname\*". You can configure this with :kbd:`M-x
    customize-option mistty-buffer-name`.

mistty : command
    This command creates a new MisTTY buffer the first time it is
    called. Afterwards, it'll try to guess what's most appropriate,
    displaying an existing MisTTY buffer or creating a new one.

    With a prefix argument, this command always creates a new buffer.

    .. index:: pair: command; mistty-other-window

mistty-other-window : command
    This command does the same as ``mistty`` but opens the buffer in
    another window.

mistty-create-other-window: command
    This command does the same as ``mistty-create``, but opens the
    buffer in another window.


If you need more control on how MisTTY windows are handled than what's
provided by the ``-other-window`` variants, you can configure it using
:kbd:`M-x customize-option display-comint-buffer-action` or :kbd:`M-x
customize-option display-buffer-alist`. In the latter case, note that
MisTTY buffers belong to the ``comint`` category, just like shell
buffers.

See the section "Window Choice" of the Emacs manual for details.

  .. index::
     pair: command; mistty-in-project
     pair: command; mistty-ssh
     pair: command; mistty-docker

mistty-in-project: command
    This command creates a new MisTTY buffer in the root directory of
    the current project the first time it is called. Afterwards, it'll
    try to guess what's most appropriate, displaying an existing
    MisTTY buffer or creating a new one.

    With a prefix argument, this command always creates a new buffer.

    Note that if you want :kbd:`M-x project-kill-buffers` to kill such
    buffers, you'll want to execute
    ``mistty-project-init-kill-buffer`` somewhere in your
    configuration or tell :kbd:`M-x configure-option
    project-kill-buffer-conditions` about MisTTY.

mistty-ssh : command
    This command creates a new MisTTY buffer connected to another host
    using SSH. This is just a shortcut that uses TRAMP to connect to a
    remote host. (:ref:`remote_shells`)

mistty-docker: command
    This command creates a new MisTTY buffer connected to
    a docker instance. This requires the docker command-line tool to
    be installed. This is just a shortcut that uses TRAMP to connect
    to a remote host. (:ref:`remote_shells`)

.. _term-vs-scroll:

Terminal vs. Scrollback
-----------------------

MisTTY buffers are divided into two distinct zones, exhibiting
different behaviors:

The **scrollback zone**, is where you can see commands that have
been executed and their output.

The **terminal zone**, marked by a purple line on the left of the
window, is where you can type command and interact with the
terminal. In this zone, :kbd:`TAB` triggers the shell completion, if
available. With some shells, you'll see autosuggestions as you type.

The scrollback zone operates like a standard Emacs buffer, allowing you to modify it freely.

The terminal zone, on the other hand, imposes certain limitations.
While a shell is attached, you can edit the command you are about to
execute, but you can't modify the prompt itself. Any changes made to
the prompt will be reverted by the shell.

The terminal zone is where the magic happens: this is where you can
use a mix of Emacs and shell key bindings to edit the command
line. The trickiest part is choosing which key bindings you want Emacs
to handle and which key bindings you want the shell to handle.

By default, Emacs intercepts most key bindings, but a few are sent directly to the terminal:

- :kbd:`RET`: Executes the command in the shell.
- :kbd:`TAB`: Initiates command completion in the shell.
- :kbd:`C-a`: Moves the cursor to the beginning of the line.
- :kbd:`C-e`: Moves the cursor to the end of the line.
- :kbd:`C-d`: Deletes the next character or exits the program.
- :kbd:`M-p`: Navigates upwards in the command history (equivalent to :kbd:`C-p` in the terminal).
- :kbd:`M-n`: Navigates downwards in the command history (equivalent to :kbd:`C-n` in the terminal).
- :kbd:`M-r`: Invokes reverse command history search (equivalent to :kbd:`C-r` in the terminal). See :ref:`bs`.
- :kbd:`M-.`: Inserts the last argument from command history.

Additionally, :kbd:`C-c C-c` sends the TERM signal to the terminal.

The program connected to the terminal determines the actual effects of
these shortcuts. While most shells and command-line editing tools
support these shortcuts by default, behavior may vary.

.. warning::

    MisTTY will not function if your shell is configured to use **VI
    mode** by default. Please **disable VI mode** before utilizing
    MisTTY. For instructions on disabling VI mode specifically for
    MisTTY buffers, while preserving it otherwise, consult see
    :ref:`shells`. VI mode must be disabled even if you plan to use VI
    commands through Evil.

    More generally, MisTTY relies on a set of common keybindings and
    behaviors to communicate with the terminal applications and will
    not behave properly if the application doesn't understand these or
    interpret them in an unusual way. For a list of these keybindings,
    see :ref:`keybindings`.

To get the most out of MisTTY, it's worth it to take the time to
configure it to send to the terminal the shell key bindings that you
actually use and keep everything else behaving as usual for your Emacs
configuration.

.. index::
   pair: map; mistty-prompt-map
   pair: map; mistty-mode-map

To bind keys only in the terminal zone, bind them to
``mistty-prompt-map``. To bind keys in both zones, bind them to
``mistty-mode-map``. Examples are provided below.

The following commands are useful for sending key sequences to the
current shell or terminal program:

.. index:: pair: command; mistty-send-key

**mistty-send-key** : command
    This command, when called interactively, forwards the key from
    which it was invoked. It is designed to be bound to the shell key
    bindings you wish to function in the terminal zone,
    `mistty-prompt-map`.

    For example, to search in the shell command history, typically
    bound to :kbd:`C-r`, MisTTY binds it to :kbd:`M-r`, similar to
    comint. However, if you'd prefer to use the original binding, you
    can do the following:

    .. code-block:: elisp

        (keymap-set mistty-prompt-map "C-r" #'mistty-send-key)

    To have the key available in both the scrollback and terminal
    zones, bind it to `mistty-mode-map` instead.

    You can also send arbitrary keys to `mistty-send-key`. For
    example:

    .. code-block:: elisp

       (defun my-mistty-M-s (n)
         (interactive "p")
         (mistty-send-key n (kbd "M-s")))
       (keymap-set mistty-prompt-map "C-c a" #'my-mistty-M-s)

.. index:: pair: command; mistty-send-last-key

**mistty-send-last-key** : command
    This command forwards the last key combination from the sequence
    it was invoked from to the terminal. For instance, :kbd:`C-c C-c`
    is bound to `mistty-send-last-key`, such that the terminal
    ultimately receives just :kbd:`C-c`.

For quick testing or for shell shortcuts you use infrequently, use
the :kbd:`C-q` prefix to bypass Emacs key bindings and send keys
directly to the terminal. For example, pressing :kbd:`C-q <right>`
sends a right-arrow key press to the terminal instead of moving the
cursor.

.. index:: pair: command; mistty-send-key-sequence

If further control is needed, try out:

**mistty-send-key-sequence** : command
  This command sends all keys you press to the terminal until you
  press :kbd:`C-g`.

  By default, it is bound to :kbd:`C-c C-q`.

.. index:: pair: command; mistty-newline

**mistty-newline** : command
    This command sends a single newline as bracketed paste, meaning
    that the newline should be inserted as-is and not execute the
    current command.

    By default, it is bound to :kbd:`S-<return>`.

.. _navigation:

Navigating the scrollback zone
------------------------------

.. index:: pair: command; mistty-end-of-line-goto-cursor

:kbd:`C-e C-e` moves the point back inside the prompt. This is handled
by the interactive function ``mistty-end-of-line-or-goto-cursor``

.. index:: pair: command; mistty-goto-cursor

:kbd:`M-x mistty-goto-cursor` also moves the point back inside the
prompt. You can bind it to a custom shortcut if you don't like
overloading C-e.

.. index:: pair: command; mistty-previous-output

:kbd:`C-c C-p` or :kbd:`M-x mistty-goto-previous-output` goes to the
beginning of the previous command output. This is useful to if the
buffer has scrolled too far and you want to see it from the beginning.

.. index:: pair: command; mistty-next-output

:kbd:`C-c C-n` or :kbd:`M-x mistty-goto-next-output` does the reverse,
that is, it goes to the next command output.

.. index:: pair: command; mistty-select-output

:kbd:`C-c C-o` or :kbd:`M-x mistty-select-output` selects the command
output at or before point. With an argument, selects the Nth previous
command output.

.. index:: pair: command; mistty-create-buffer-with-output

:kbd:`C-c C-r` or :kbd:`M-x mistty-create-buffer-with-output` creates
a new buffer containing the command output at or before point. With an
argument, creates a buffer containing the Nth previous command output.

.. index:: pair: command; mistty-goto-previous-input

:kbd:`M-x mistty-goto-previous-input` goes to the beginning of the
previous command input, that is, the previous prompt. While this is a
way of going back the command you've previously input, it's best to
use the shell native command history, as discussed in :ref:`history`.

.. index:: pair: command; mistty-goto-next-input

:kbd:`M-x mistty-goto-next-input` goes to the next command input.

.. index::
   pair: command; mistty-clear
   pair: variable; mistty-allow-clearing-scrollback

:kbd:`C-c C-l` or :kbd:`M-x mistty-clear` deletes the content of the
buffer until the current prompt. With a numeric argument N, it deletes
the content of the buffer until the Nth previous prompt.

Alternatively, if you prefer clearing the scrollback zone from the
command line instead of using an Emacs command, you can also do
:kbd:`M-x customize-option mistty-allow-clearing-scrollback` to let
the ``reset`` and ``clear`` commands clear the whole buffer instead of
just the terminal zone.

MisTTY defines an index for imenu, so that :kbd:`M-x imenu` lets you
select commands and outputs to go to. This also allows packages based
on imenu to work, such as `breadcrumbs
<https://github.com/joaotavora/breadcrumb>`_ that can display the
current command in a header, if enabled in a MisTTY buffer.

.. note::
   If imenu shows strange commands, consider adding an OSC133 B
   code ("\033]133;B\007") at the end of your prompt so MisTTY knows
   when the prompt ends and where user input begins. See :ref:`osc`

Additionally, MisTTY defines prompts as defuns, so any function that
applies on defun applies on prompts in a MisTTY buffer, so, for
example:

  - :kbd:`C-x n d` or :kbd:`M-x narrow-to-defun`, narrows the buffer
    to the current prompt and its output
  - :kbd:`C-M a` or :kbd:`M-x beginning-to-defun`, moves point
    to the beginning of the current prompt
  - :kbd:`C-M e` or :kbd:`M-x end-to-defun`, moves point to the end
    of the current prompt's output
  - :kbd:`C-M h` or :kbd:`M-x mark-defun`, marks the current prompt
    and its output

.. _fullscreen:

Fullscreen Mode
---------------

MisTTY detects when a program, such as :program:`less` or
:program:`vi`, requests full-screen mode. In response, it splits the
MisTTY buffers in two:

- The **terminal buffer**, which displays the program's output and
  allows you to interact with it. It operates in term-mode.

- The **Scrollback Buffer**, which contains the previous command lines
  along with their outputs.

.. index:: pair: command; mistty-toggle-buffers

To switch between these buffers, press :kbd:`C-c C-j` or execute
:kbd:`M-x mistty-toggle-buffers`

When the full-screen program exits, the two buffers are merged back
together. Please note that the output from the full-screen application
is not available in the scrollback region.

.. _history:

Command History
---------------

MisTTY doesn't track command history. It relies instead on being able
to access the history of the different interactive command-line tools.

The command history available in most shells and command-line editing tools is
available in MisTTY using the following shortcuts:

- :kbd:`M-p` moves up command history
- :kbd:`M-n` moves down command history
- :kbd:`M-r` triggers a backward search in command history (:ref:`bs`)
- :kbd:`M-.` insert the last argument from command history

To get the same key bindings you'd get in a normal terminal, you can
bind :kbd:`C-p`, :kbd:`C-n`, or :kbd:`C-r` to ``mistty-send-key``
in the terminal zone of the MisTTY buffer. For example:

.. code-block:: elisp

    (keymap-set mistty-prompt-map "C-p" #'mistty-send-key)
    (keymap-set mistty-prompt-map "C-n" #'mistty-send-key)
    (keymap-set mistty-prompt-map "C-r" #'mistty-send-key)

.. _bs:

Backward Search
---------------

.. index::
   pair: map; mistty-forbid-edit-map
   pair: variable; mistty-forbid-edit-regexps
   pair: variable; mistty-forbid-edit-map

Within various shells, pressing :kbd:`C-r` or :kbd:`M-r` activates a
special backward search mode, where editing options are limited.
MisTTY identifies this mode using the regular expressions set in
:kbd:`M-x customize-option mistty-forbid-edit-regexps`.

When this mode is active:

- You can append or delete text but cannot modify it. Though yanking
  text and word deletion are still possible, most Emacs editing
  won't work.

- The modeline displays "FE:run" to indicate Forbid Edit mode.

- Arrow keys are sent directly to the terminal, which is beneficial
  for shells like Fish that allow selection from multiple choices. To
  customize this functionality, adjust key bindings in
  ``mistty-forbid-edit-map``, which extends ``mistty-prompt-map``
  in this mode.

- Pressing :kbd:`C-g` sends a signal to the terminal and typically
  exits backward search mode without making a selection.

.. _cap:

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
Auto-completion is usually the main challenge, described in
:ref:`autocomplete`. Please file a bug (:ref:`reporting`) if you
encounter issues with other completion packages.

Autosuggestions
^^^^^^^^^^^^^^^
.. index::
   pair: variable; mistty-wrap-capf-functions

``completion-at-point`` completes the text *around* the point.
This is generally convenient, but gets confused by shell
autosuggestions, available in Fish or ZSH.

What if you typed "com" and the shell helpfully suggests "completion"?
The buffer would look like: "com<>pletion", with <> representing
the point. ``completion-at-point`` would then think you typed
"completion" and not suggest anything else.

To avoid that problem MisTTY modifies the functions it finds in
``completion-at-point-functions`` so that they just won't see
anything after the point when in the terminal region. In the example
above, they'd only complete "com", not "completion".

That is, ``completion-at-point`` in the MisTTY terminal region
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
Please file a bug (:ref:`reporting`) if you encounter issues with
other packages.

.. _dirtrack:

Directory Tracking
------------------

If you're using :program:`Bash` or :program:`Fish` version 4 or later,
as a shell, you'll discover that Emacs keeps track of the shell's
current directory, so commands like :kbd:`M-x find-file` know where to
start from.

If you're using any other shell, you'll need to configure it to tell
Emacs about its current directory, as described in :ref:`Directory
Tracking for Fish <fish_dirtrack>` and in :ref:`Directory Tracking for
Zsh <zsh_dirtrack>`.

:program:`Bash` out-of-the-box directory tracking also doesn't work in
shells you start using :program:`ssh` or :program:`docker`. For that
to work, the simplest solution is to start remote shells with
TRAMP. (:ref:`remote_shells`)

.. _remote_shells:

Remote Shells with TRAMP
------------------------

If the `default-directory` that is current when a new MisTTY buffer is
created contains a TRAMP path whose method supports it, MisTTY runs
the shell with the method, user and host *of that path*.

.. tip::

  :kbd:`C-u M-x mistty-create` asks for a directory instead of using
  the default one. This makes it possible to open a remote shell on a
  host that no buffer is visiting. See :ref:`launching`.

For this to work, MisTTY needs to know the shell executable to use on
that host. The value of ``mistty-shell-command`` or
``explicit-shell-file-name`` is interpreted as a local file within
that host, which might not always work.

To run different shells on different hosts, define different
connection local profiles that set ``mistty-shell-command`` and
bind them to the TRAMP host, machine or user you want, as shown in the
example below. This is described in details in the *Emacs Lisp*
manual, in the section *Connection Local Variables*.

Example:

.. code-block:: elisp

  (connection-local-set-profile-variables
   'profile-usr-local-fish
   '((mistty-shell-command . ("/usr/local/bin/fish" "-i"))))

  (connection-local-set-profiles '(:machine "myhost.example")
   'profile-usr-local-fish)

By default, the name of TRAMP shells include the user and hostname, if
different from the current one. If you don't want that, configure it
on :kbd:`M-x customize-option mistty-buffer-name`.

.. _tramp_dirtrack:

Directory tracking and TRAMP
----------------------------

.. index::
   pair: variable; mistty-allow-tramp-path
   pair: variable; mistty-host-to-tramp-path-alist

Directory tracking (:ref:`dirtrack`) normally just works in TRAMP
shells started described in the previous section.

This isn't necessarily true of shells started from a MisTTY buffers,
by calling :program:`ssh`, :program:`docker` or :program:`sudo`, but
it is possible to make that work, as described below.

.. tip::

   The simplest way to connect a host or docker instance you don't
   want to configure is to just start it as described in
   :ref:`remote_shells` and use :program:`Bash` as your shell.
   Everything then just work out of the box, at least for Bash
   4.4 and later. (:ref:`bash_dirtrack`)

If you haven't already, configure your shell to tell Emacs about
directory changes, even :program:`Bash`. This is described in
:ref:`Directory Tracking for Bash <bash_dirtrack>`, in :ref:`Directory
Tracking for Fish <fish_dirtrack>`, and in :ref:`Directory Tracking for
Zsh <zsh_dirtrack>`.

Once this is done, the shell sends out file: URLs that include the
host name. By default, MisTTY will then use that to set the default
directory to remote file paths that include that hostname using the
default TRAMP method. For example, given the file: URL
``file:/example.com/var/log`` reported by the shell, MisTTY will
set the directory of its buffer to ``/-:example.com:/var/log``.

If you always connect to hosts using SSH, this is likely all you need,
if not, you can still make it work as follows:

- If you're using some other way of connecting to your host, configure
  it in :kbd:`M-x configure-option tramp-default-method`. You can also
  configure that on a per-host basis using :kbd:`M-x configure-option
  tramp-default-method-alist`

- If you're connecting to hosts in more diverse ways, you can
  configure the TRAMP path MisTTY should generate using :kbd:`M-x
  configure-option mistty-host-to-tramp-path-alist`

- If you want to configure the TRAMP path on the hosts, you can send
  it from the prompt as Emacs-specific ``\\032/...\\n`` code
  containing a TRAMP path instead of the standard file: URL
  recommended in :ref:`Directory Tracking for Bash <bash_dirtrack>`,
  in :ref:`Directory Tracking for Fish <fish_dirtrack>`, and in
  :ref:`Directory Tracking for Zsh <zsh_dirtrack>`. Here's an example
  of such a code for :program:`Bash` that tells TRAMP to connect to
  the current docker instance:

  .. code-block:: bash

    if [ "$TERM" = "eterm-color" ]; then
        PS1='\032//docker:$HOSTNAME:/$PWD\n'$PS1
    fi


That said, if you need more than just SSH to connect to other hosts,
it might be overall just easier to start remote shells with TRAMP
(:ref:`remote_shells`) instead of the command line, because directory
tracking just works in that case.

If everything fails, if TRAMP is causing you too much trouble and you
just don't want MisTTY to generate remote paths at all, unset the
option :kbd:`M-x configure-option mistty-allow-tramp-paths`.

.. _keybindings:

Keybindings used by MisTTY
--------------------------

MisTTY relies on applications connected to the terminal to handle
keybindings in a common way, that is:

- Any printable character: Inserts the character at the cursor position.

- :kbd:`DEL` (ASCII 127): Deletes the preceding character.

- :kbd:`C-k` (ASCII 11): Deletes from the cursor to the end of the line.

- :kbd:`C-e` (ASCII 5): Moves the cursor to the end of the line.

- :kbd:`C-a` (ASCII 1): Moves the cursor to the beginning of the line.

- ``ESC [ 200 ~ ... ESC [ 201 ~`` (bracketed paste): Inserts
  characters verbatim, including tabs and newlines, allowing for
  multiline support.


These keybindings are supported by recent versions of bash, zsh, fish,
Python, and IPython.

When interacting with applications that do not support some or all of
these keybindings, operations may be limited. You will still be able
to insert characters and, when supported, delete them using backspace
or :kbd:`C-d`. However, you may not have access to Emacs-style
operations for moving the cursor or editing text.

It is important to note that in backward i-search mode within shells,
only character insertion and deletion are supported. Thus, while most
Emacs commands may not function, you can still use simple Emacs
commands that insert text after the cursor, such as ``yank`` (with no
special characters), or delete text before the cursor, such as
``backward-kill-word``. For more details, see :ref:`bs`

.. _osc:

Supported OSC Control Sequences
-------------------------------

OSC are optional “operating system command” control sequences that
programs can use to communicate with the terminal and Emacs. MisTTY
supports the following OSC control sequences:

- *OSC 0; <title> ST* and *OSC 2; <title> ST* changes the window
  title. This sets the variable ``ansi-osc-window-title`` in the
  MisTTY buffer, which can then be referred to in
  ``frame-title-format`` to set the frame title dynamically.

- *OSC 7;file://<hostname>/<path> ST* reports the shell's current
  directory to Emacs. See :ref:`dirtrack`

- *OSC 8;;<url> ST <text> OSC 8;; ST* makes text clickable.

  Example:

  .. code-block:: bash

    printf '\e]8;;http://example.com\e\\This is a link\e]8;;\e\\\n'

- *OSC 10;? ST* and *OSC 11;? ST* query the foreground or background
  color. The response is an hexadecimal 16 bit RGB value.

  Example: Querying the background color in Bash:

  .. code-block:: bash

    $ read -t 0.1 -rs -d \\ -p $'\e]11;?\e\\' bg
    $ echo "$bg" | strings
    ]11;rgb:1313/1c1c/2b2b

  Example: A Zsh function that can figure out whether you have a light
  or dark background:

  .. code-block:: zsh

    function bg_brightness {
        local bg
        if read -t 0.1 -rs -d \\ "?$(printf '\e]11;?\e\\')"  bg; then
            if [[ "$bg" =~ '11;rgb:([0-9a-f]{4})/([0-9a-f]{4})/([0-9a-f]{4})' ]]; then
                local r g b brightness
                typeset -i 10 r=16#${match[1]}
                typeset -i 10 g=16#${match[2]}
                typeset -i 10 b=16#${match[3]}
                (( brightness = ( 0.2126 * r + 0.7152 * g + 0.0722 * b ) * ( 256.0 / 0xffff ) ))
                if [[ $brightness -le 128 ]]; then
                    echo dark
                else
                    echo light
                fi
                return 0
            fi
        fi
        return 1
    }

.. _osc133:

- *OSC 133; A-D ; <options> ST*

Escape sequences that help terminals identify shell commands and their
output, originally defined by FinalTerm. Several terminals support OSC
133, such as `wezterm <https://wezterm.org/shell-integration.html>`_,
`kitty
<https://sw.kovidgoyal.net/kitty/shell-integration/#notes-for-shell-developers>`_
and `iTerm
<https://iterm2.com/documentation-shell-integration.html>`_, so you
might have it enabled already.

Without these commands, MisTTY uses heuristics to detect the beginning
of a prompt or of a command, they should normally not be necessary.
It's a good idea, however, to configure your shell to send them out.
The iTerm2 website has `ready-made scripts
<https://iterm2.com/documentation-shell-integration.html#install-by-hand>`_
to configure most shells to send out OSC 133 codes.

Starting with version 4.0.0, Fish sends out codes A, C and D
automatically, but you still need to send out B at the end of your
prompt.

*OSC 133;A ST* should be sent just before the start of a prompt,
usually from a precmd function. Sending out this code allows MisTTY to
know that user input is expected.

*OSC 133;B ST* should be sent just after having written the prompt, to
signal the start of user input. It should be sent out at the very end
of the prompt. Sending out this code allows MisTTY to mark prompts so
that commands such as `beginning-of-line` are aware of where user
input starts.

*OSC 133;C ST* should be sent just before executing the command,
usually from a preexec function. Sending out this code allows MisTTY
to know where the prompt ends.

*OSC 133;D ST* should be sent at the end of the command, or when the
command is cancelled.

.. note::

   Different terminals may interpret OSS 133 codes differently. MisTTY
   only supports codes A,B,C, and D for marking the user input and
   prompt sections and ignores any extra fields.

To extend the set of OSC codes supported by MisTTY, see :ref:`ext_osc`.
