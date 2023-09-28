;;; Tests compatibility with other packages -*- lexical-binding: t -*-

(require 'mistty)
(require 'mistty-testing)

(ert-deftest mistty-compat-test-and-hippie-completion ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello, hullo, hallo, hi")
    (mistty-send-and-wait-for-prompt)

    (let ((hippie-expand-try-functions-list '(try-expand-dabbrev))
          (start (point)))

      (mistty-send-text "echo h")
      (should (equal "echo h<>"
                     (mistty-test-content :start start :show (point))))

      (mistty-run-command
       (setq this-command 'hippie-expand)
       (call-interactively 'hippie-expand))
      (should (equal "echo hi<>"
                     (mistty-test-content :start start :show (point))))

      (mistty-run-command
       (setq this-command 'hippie-expand)
       (setq last-command 'hippie-expand)
       (call-interactively 'hippie-expand))
      (should (equal "echo hallo<>"
                     (mistty-test-content :start start :show (point)))))))
