Usage
=====

.. _installation:

Installation
------------

To use MisTTY, first install its package.

.. code-block:: elisp

    (package-vc-install "https://github.com/szermatt/mistty")

You can then call it, as described in :ref:`launching`, but you'll
likely want to configure it and add some key bindings you use often in
shells, for example:

.. code-block:: elisp

    (use-package mistty
      :bind (("C-c s" . mistty)

             ;; bind here the shortcuts you'd like the
             ;; shell to handle instead of Emacs.
             :map mistty-prompt-map

             ;; all shells: go up/down in the shell history
             ("C-p" . mistty-send-key)
             ("C-r" . mistty-send-key)

             ;; bash: history-token-search-backward
             ("M-." . mistty-send-key)

             ;; fish: dir history, more history manipulation
             ("M-<up>" . mistty-send-key)
             ("M-<down>" . mistty-send-key)
             ("M-<left>" . mistty-send-key)
             ("M-<right>" . mistty-send-key)))

Read on for details on the commands and key bindings configured above.

.. _launching:

Launching
---------

    M-x mistty-create
    M-x mistty

.. _vs:

Terminal vs. Scrollback
-----------------------

.. _navigation:

Scrollback Navigation
---------------------

.. _fullscreen:

Fullscreen Mode
---------------

.. _dirtrack:

Dirtrack and TRAMP
------------------
