;;; Tests MisTTY's TRAMP integration -*- lexical-binding: t -*-

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see
;; `http://www.gnu.org/licenses/'.

(require 'ert)
(require 'ert-x)
(require 'tramp)
(eval-when-compile
(require 'cl-lib))

(require 'mistty)
(require 'mistty-testing)

(ert-deftest mistty-tramp-test-shell-start ()
  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (home (file-name-directory "/"))
         (default-directory (concat tramp-prefix home)))
    (mistty-with-test-buffer ()
      (should (equal (concat tramp-prefix "/")
                     (buffer-local-value 'default-directory (current-buffer))))
      (should (equal mistty-test-bash-exe (mistty-test-remote-command)))

      ;; This just makes sure the shell is functional.
      (mistty-send-text "echo hello")
      (should (equal "hello" (mistty-send-and-capture-command-output)))

      ;; TRAMP sets INSIDE_EMACS and reports its version.
      (mistty-send-string "echo $INSIDE_EMACS")
      (should (equal (format "%s,term:%s,tramp:%s" emacs-version term-protocol-version tramp-version)
                     (mistty-send-and-capture-command-output))))))

(ert-deftest mistty-tramp-test-connection-local-explicit-shell-file-name ()
  (skip-unless mistty-test-zsh-exe)
  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (default-directory (concat tramp-prefix "/"))
         (connection-local-profile-alist nil)
         (connection-local-criteria-alist nil)
         (mistty-shell-command nil)
         (explicit-shell-file-name "/bin/sh")
         buf)

    (connection-local-set-profile-variables
     'test-profile
     `((explicit-shell-file-name . ,mistty-test-zsh-exe)))
    (connection-local-set-profiles (mistty-test-tramp-protocol) 'test-profile)

    ;; This makes sure the connection-local setup above works.
    (should (equal mistty-test-zsh-exe
                   (with-connection-local-variables explicit-shell-file-name)))

    (unwind-protect
        (progn
          (setq buf (mistty-create))
          (should (process-get mistty-proc 'remote-command))
          ;; This makes sure that the connection-local value of
          ;; explicit-shell-file-name is the one that's used, and not
          ;; the global value.
          (should (equal mistty-test-zsh-exe (mistty-test-remote-command))))
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest mistty-tramp-test-connection-local-explicit-mistty-shell-command ()
  (skip-unless mistty-test-zsh-exe)
  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (default-directory (concat tramp-prefix "/"))
         (connection-local-profile-alist nil)
         (connection-local-criteria-alist nil)
         (mistty-shell-command "/bin/sh")
         buf)

    (connection-local-set-profile-variables
     'test-profile
     `((mistty-shell-command . ,mistty-test-zsh-exe)))
    (connection-local-set-profiles (mistty-test-tramp-protocol) 'test-profile)

    ;; This makes sure the connection-local setup above works.
    (should (equal mistty-test-zsh-exe
                   (with-connection-local-variables mistty-shell-command)))

    (unwind-protect
        (progn
          (setq buf (mistty-create))
          (should (process-get mistty-proc 'remote-command))
          ;; This makes sure that the connection-local value of
          ;; explicit-shell-file-name is the one that's used, and not
          ;; the global value.
          (should (equal mistty-test-zsh-exe (mistty-test-remote-command))))
      (when (buffer-live-p buf)
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest mistty-tramp-test-termcap ()
  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (default-directory (concat tramp-prefix "/"))
         (term-term-name "eterm-test"))
    (mistty-with-test-buffer ()

      ;; TERMINFO shouldn't be set in remote shells.
      (mistty-send-text "echo TERMINFO=${TERMINFO}.")
      (should (equal "TERMINFO=." (mistty-send-and-capture-command-output)))

      ;; TERMCAP should be set.
      (mistty-send-text "if [ -n \"$TERMCAP\" ]; then echo set; else echo unset; fi")
      (should (equal "set" (mistty-send-and-capture-command-output)))

      ;; captoinfo reads and processes the TERMCAP env variable. This
      ;; makes sure that the content of TERMCAP is valid.
      (mistty-send-text "captoinfo")
      (should (string-match "eterm-test,\n +am, mir.*" (mistty-send-and-capture-command-output))))))

(ert-deftest mistty-tramp-test-dirtrack-on-sg ()
  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (default-directory (concat tramp-prefix "/")))
    (mistty-with-test-buffer (:shell zsh)
      ;; Not using bash, because it sends \032 dirtrack, which would
      ;; interfere with this test.

      (should (equal (concat tramp-prefix "/") default-directory))

      (mistty--send-string mistty-proc "printf '\\032//var/log\\nok\\n'")
      (should (equal "ok" (mistty-send-and-capture-command-output)))
      (should (equal (concat tramp-prefix "/var/log/") default-directory))

      (mistty--send-string mistty-proc "printf '\\032//home\\nok\\n'")
      (should (equal "ok" (mistty-send-and-capture-command-output)))
      (should (equal (concat tramp-prefix "/home/") default-directory)))))

(turtles-ert-deftest mistty-tramp-test-window-size ()
  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (home (file-name-directory "/"))
         (default-directory (concat tramp-prefix home)))
    (delete-other-windows)
    (mistty-with-test-buffer (:selected t)
      (let ((win (selected-window))
            cols-before cols-after)
        (mistty--send-string mistty-proc "tput cols")
        (setq cols-before (string-to-number (mistty-send-and-capture-command-output)))

        (split-window-horizontally nil win)
        (mistty--window-size-change win)

        ;; Make sure the terminal has taken the size change into
        ;; account.
        (mistty-send-text "echo ok")
        (mistty-send-and-wait-for-prompt)

        ;; This makes sure the terminal is told about the window size
        ;; change.
        (mistty--send-string mistty-proc "tput cols")
        (setq cols-after (string-to-number (mistty-send-and-capture-command-output)))
        (should-not (equal cols-before cols-after))
        (should (< cols-after cols-before))))))

(ert-deftest mistty-tramp-test-buffer-name ()
  (let ((mistty-buffer-name '("mistty" mistty-buffer-name-user mistty-buffer-name-host)))
    (let ((default-directory "/ssh:someuser@test.example:/"))
      (should (equal "*mistty-someuser@test.example*" (mistty-new-buffer-name))))
    (let ((default-directory "/ssh:test.example:/"))
      (should (equal "*mistty@test.example*" (mistty-new-buffer-name))))
    (let ((default-directory "/sudo::/"))
      (should (equal "*mistty-root*" (mistty-new-buffer-name))))))

(ert-deftest mistty-tramp-test-set-EMACS ()
  (setenv "EMACS" nil) ;; Sometimes set to run Eldev

  (let* ((tramp-methods (mistty-test-tramp-methods))
         (tramp-prefix (mistty-test-tramp-prefix))
         (home (file-name-directory "/"))
         (default-directory (concat tramp-prefix home))
         (connection-local-profile-alist nil)
         (connection-local-criteria-alist nil)
         (mistty-set-EMACS nil))

    (mistty-with-test-buffer ()
      (mistty-send-text "if test -n \"$EMACS\"; then echo set; else echo unset; fi")
      (should (equal "unset" (mistty-send-and-capture-command-output))))

    (connection-local-set-profile-variables
     'test-profile
     '((mistty-set-EMACS . t)))
    (connection-local-set-profiles (mistty-test-tramp-protocol) 'test-profile)

    ;; This makes sure the connection-local setup above works.
    (should (equal t (with-connection-local-variables mistty-set-EMACS)))

    (mistty-with-test-buffer ()
      (mistty-send-text "if test -n \"$EMACS\"; then echo set; else echo unset; fi")
      (should (equal "set" (mistty-send-and-capture-command-output))))))
