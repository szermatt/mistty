Contributing
============

.. _reporting:

Reporting issues
----------------

At this time, the most useful thing you can do to help is and useful
bug reports to the `Issue Tracker`_

In your report, please discuss what you wanted to happen as well as
what happened. Also, please include enough information to reproduce
the issue. Such as:

- the name and version of the program you were running - usually a shell
- the version of Emacs you're running, taken, for example, from :kbd:`M-x about-emacs`
- whether you're running Emacs in a window environment or a terminal
- what kind of prompt you're using, that is, what it looks like, what
  character it ends with, how many lines it has and whether you're
  using any kind of right prompt

.. index::
   pair: command; mistty-start-log
   pair: command; mistty-stop-log

If you can reproduce reliably, please include the content of the
buffer :code:`*mistty-log*` into your report, as follows:

- Enable logging by calling :kbd:`M-x mistty-start-log`
- Reproduce the issue
- Go to the buffer :code:`*mistty-log*`
- Call :kbd:`M-x mistty-stop-log` to avoid getting more log entries
- Copy the buffer content and paste it into the issue. The log
  includes everything that you write to the terminal and everything
  that you get back from the terminal. Please make sure you're not
  including any private information, such as password - remove them if
  necessary...

If you cannot reproduce reliably,

- go to :kbd:`M-x customize-option mistty-backlog-size` to set the
  backlog size to a large value, such as 50
- use MisTTY normally, until the issue comes back
- once it has happened again, immediately call :kbd:`M-x
  mistty-start-log`. The log will then contain entries for events that
  happened just *before* you called the command.
- call :kbd:`M-x mistty-stop-log`
- copy the content of the :code:`*mistty-log*` buffer, strip out
  anything private, and include it into the issue.

.. _Issue tracker: https://github.com/szermatt/mistty/issues

Suggesting features
-------------------

Please add feature suggestions to the `Issue Tracker`_.

Asking questions
----------------

Open an issue on the `Issue Tracker`_ with your question.

Code contributions
------------------

To contribute code to the project, open a `Pull Request`_.

Before you do that, please make sure the any new features is covered
by tests and that the tests pass.

To run the tests, first checkout the test dependencies with
:command:`git submodule update --init--recursive`, install and setup
`eldev`_, then run :command:`eldev test`.

Tests can also be run from inside of Emacs, using `M-x
ert-run-tests-interactively` but when you do so, be aware that there
might be unexpected interaction with your Emacs configurations. The
tests passing reliably when run using :command:`eldev test` is what
matters.

.. _eldev: https://github.com/emacs-eldev/eldev

Documentation contributions
---------------------------

You don't need to be a developer to contribute! Contribution to the
documentation or code comments are very welcome. Please open a `Pull
Request`_ with your proposed modifications.

The documentation is written in reStructuredText. You'll need to
install `Sphinx <https://www.sphinx-doc.org>`_ to build it:

.. code-block:: bash

   python3 -m venv venv
   . venv/bin/activate # or activate.fish on fish
   pip3 install -r docs/requirements.txt

Then run :command:`eldev html` to build the documentation.

.. _Pull Request: https://github.com/szermatt/emacs-bash-completion/pulls
