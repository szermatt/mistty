FAQ
===

**The display is all messed up**

     First, check the value of the environment variable :code:`TERM`.
     It MUST be :code:`eterm-color` or :code:`eterm-direct`; nothing
     else will work reliably.

     If that still doesn't work, please file a bug as described in
     :ref:`reporting`.

**warning: Could not set up terminal**

     If you're getting errors such as the following from programs such
     as :program:`less` or :program:`vi`, this means that the
     :code:`TERM` environment variable is set properly, but the host
     doesn't know about the terminal :code:`eterm-color` or
     :code:`eterm-direct`.

     .. code-block:: text

       warning: Could not set up terminal.
       warning: TERM environment variable set to 'eterm-color'.
       warning: Check that this terminal type is supported on this system.

     This might easily happen if you ssh into another host from inside
     a MisTTY buffer. SSH typically forwards the value of the
     :code:`TERM` environment variable, which contains the terminal
     name, but not :code:`TERMCAP` environment variable, which contains
     the terminal definition.

     To fix this issue, you can do any one of the following:

     - Connect using TRAMP, as described in :ref:`remote_shells`. TRAMP takes
       care of setting all necessary environment variables.

     - Add the definition of :code:`eterm-color` to all hosts you
       regularly log into. To do that, follow the instructions in
       :file:`<data-directory>/e/README`, where
       :code:`<data-directory>` is the "etc" directory of your Emacs
       installation, as shown by `M-x describe-variable
       data-directory` - usually, that's
       :file:`/usr/share/emacs/<emacs version>/etc/e/README`

     - Tell ssh to forward the :code:`TERMCAP` environment variable. This
       requires changing *both* the server and client configuration. On the
       server :file:`sshd_config`, add :code:`AcceptEnv TERMCAP`. On the
       client, add :code:`SendEnv TERMCAP` to :file:`ssh_config` or to
       :file:`~/.ssh/config`

**What are all those OCOCOCO or ODODODO that appear on the screen?**

     .. index:: pair: variable; mistty-forbid-edit-regexps

     These are the terminal sequences that MisTTY sends to a program
     to move the cursor left or right. If you see these printed on the
     terminal, it means that the program that's currently controlling
     the terminal doesn't support these.

     In such situation, you can:

     - Only type self-inserting characters and :kbd:`DEL`.

     - Press :kbd:`C-c C-q` or :kbd:`M-x mistty-send-key-sequence` to
       send what you type directly to the terminal until you turn it
       off with :kbd:`C-g`.

     - Write a regexp that identifies the situation and add it to
       :kbd:`M-x customize-option mistty-forbid-edit-regexps` so MisTTY
       knows it should not attempt to move the cursor. The default value
       identifies most shell backward search prompts.

     See :ref:`term-vs-scroll` for more details.

**The shell isn't answering!**

    Press :kbd:`C-g`. If this is just a one-time thing, this will do
    the trick.

    If this keeps happening, check the modeline. Does it contain CMD?
    It might look like this: *misTTY CMD:run*. In that case, MisTTY is
    stuck in long-running command mode. This is likely due to some
    package leaving overlays to the buffer that confuse MisTTY. To fix
    that, turn off the option :kbd:`M-x customize-option
    mistty-detect-foreign-overlays` or, if you know which package is
    causing trouble, remove the corresponding property in in :kbd:`M-x
    customize-option mistty-foreign-overlay-properties`.

    For details, see :ref:`lrc`

    If this keeps happening and the modeline does not contain CMD,
    this is likely a bug. For details on filing a bug report, see
    :ref:`reporting`

**Why is the cursor jumping around when I move it?**

    MisTTY jumps over spaces which just "aren't there", such as the
    spaces between the command and the right prompt, spaces added by
    :program:`fish` for indentation in multi-line prompts.

    If it doesn't work with your shell or setup, or if you find it
    confusing, type :kbd:`M-x customize-option
    mistty-skip-empty-spaces` to turn it off.

**What's with the purple line?**

    This line indicates the region of the buffer that works as a
    terminal, meaning that it is not fully editable and that some
    shortcuts, such as :kbd:`TAB` are sent directly to the program
    controlling the terminal.

    This is covered in depth in :ref:`term-vs-scroll`.

    If you just don't want to see that line, turn it off with
    :kbd:`M-x customize-option mistty-fring-enabled`

**Why doesn't <insert package here> work in the terminal region?**

    The terminal region of MisTTY behaves very differently from a
    normal buffer; many things can go wrong with commands that do more
    than just editing text.

    One such issue is with interactivly editing the buffer over
    multiple Emacs command, which MisTTY calls a long-running command.
    There are ways of making such commands work if they don't already,
    described in :ref:`lrc`.

    Another such issue is with autocomplete, with can also be made to
    work as described in :ref:`autocomplete`.

    While this works with some packages, it might not necessarily work
    with the package you want - it might even not be possible to make
    it work, but we can always try. Please `file a bug
    <https://github.com/szermatt/mistty/issues>`_ if you encounter
    such a package you'd like to work with MisTTY.

**... but it used to work!**

    Older versions used to detect any unknown overlay as a
    long-running command, described in :ref:`lrc`. Unfortunately, this
    caused problems with many packages which, leaving overlays around
    for a long time, prevented MisTTY from working at all.

    The good news is that it's likely easy to make it work again by
    detecting the specific kind of overlays the package is using.
    Please see :ref:`lrc`, or file a bug (:ref:`reporting`) mentioning
    the package you're using, its version and how you installed it.

**Why am I getting connection errors from TRAMP?**

    MisTTY tries to track the current directory whenever possible,
    including from remote shells. You might get connection errors if
    you connect to a remote or special shell from an existing MisTTY
    that Emacs cannot access through TRAMP and then Emacs tries to
    access a nonexisting remote file.

    The best solution in such case is to configure TRAMP to connect to
    that host, adding an entry to :kbd:`M-x configure-option
    mistty-host-to-tramp-path-alist`, if that's necessary.

    If that's not possible or if you don't want to bother, you might
    find it convenient to just disable the generation of TRAMP paths
    using :kbd:`M-x customize-option mistty-allow-tramp-paths`.

    For more details, see :ref:`dirtrack`.

**The buffer is killed when the shell finishes. I didn't use to do that!**

     .. index:: pair: variable; mistty-at-end

     MisTTY now by default kills the buffer and its containing window
     when the shell ends. If you don't like that, change the option at
     :kbd:`M-x customize-option mistty-at-end` to do nothing.
