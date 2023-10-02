;;; Tests compatibility with other packages -*- lexical-binding: t -*-

(require 'mistty)
(require 'mistty-testing)

(require 'yasnippet nil 'noerror)

(ert-deftest mistty-compat-test-hippie-expand ()
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

(ert-deftest mistty-compat-test-yas-expand ()
  (skip-unless (featurep 'yasnippet))
  (yas-define-snippets
   'mistty-mode
   ;; (KEY TEMPLATE NAME ...)
   '(("bif" "if ${1}; then ${0}; fi" "bif")))
  (mistty-with-test-buffer (:selected t)
    (yas-minor-mode-on)
    (keymap-local-set "C-c y" #'yas-expand)
    (mistty-send-text "bif")
    (execute-kbd-macro (kbd "C-c y t r u e TAB e c h o SPC o k TAB"))
    (mistty-wait-for-output :test (lambda () (not mistty--inhibit)))
    (mistty-wait-for-output :str "echo ok")
    (should-not mistty--inhibit)
    (should (equal "$ if true; then echo ok; fi<>"
                   (mistty-test-content :show (point))))
    (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-compat-yas-expand-multiline ()
  (skip-unless (featurep 'yasnippet))
  (yas-define-snippets
   'mistty-mode
   ;; (KEY TEMPLATE NAME ...)
   '(("bif" "if ${1}\nthen ${0}\nfi" "bif")))
  (mistty-with-test-buffer (:selected t)
    (yas-minor-mode-on)
    (keymap-local-set "C-c y" #'yas-expand)
    (mistty-send-text "bif")
    (execute-kbd-macro (kbd "C-c y t r u e TAB e c h o SPC o k TAB"))
    (mistty-wait-for-output :test (lambda () (not mistty--inhibit)))
    (mistty-wait-for-output :str "echo ok")
    (should-not mistty--inhibit)
    (should (equal "$ if true\nthen echo ok\nfi<>"
                   (mistty-test-content :show (point))))
    (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-compat-yas-expand-multiline-fish ()
  (skip-unless (featurep 'yasnippet))
  (yas-define-snippets
   'mistty-mode
   ;; (KEY TEMPLATE NAME ...)
   '(("fif" "if ${1}\n${0}\nend" "fif")))
  (mistty-with-test-buffer (:shell fish :selected t)
    (yas-minor-mode-on)
    (keymap-local-set "C-c y" #'yas-expand)
    (mistty-send-text "fif")
    (execute-kbd-macro (kbd "C-c y t r u e TAB e c h o SPC o k TAB"))
    (mistty-wait-for-output :test (lambda () (not mistty--inhibit)))
    (mistty-wait-for-output :test (lambda () (mistty--queue-empty-p mistty--queue)))
    (should (equal (concat "$ if true\n"
                           "      echo ok\n"
                           "  end<>")
                   (mistty-test-content :show (point))))
    (mistty-send-and-wait-for-prompt)
    (should (equal (concat "$ if true\n"
                           "      echo ok\n"
                           "  end\n"
                           "ok\n"
                           "$ <>")
                   (mistty-test-content :show (point))))))

(ert-deftest mistty-compat-yas-expand-multiline-fish-insert ()
  (skip-unless (featurep 'yasnippet))
  (yas-define-snippets
   'mistty-mode
   ;; (KEY TEMPLATE NAME ...)
   '(("fif" "if ${1}\n${0}\nend" "fif")))
  (mistty-with-test-buffer (:shell fish :selected t)
    (yas-minor-mode-on)
    (keymap-local-set "C-c y" #'yas-expand)
    (mistty-send-text "fif")
    (execute-kbd-macro (kbd "C-c y"))
    (mistty-run-command
     (insert "true"))
    (execute-kbd-macro (kbd "TAB"))
    (mistty-run-command
     (insert "echo ok"))
    (execute-kbd-macro (kbd "TAB"))
    (mistty-wait-for-output :test (lambda () (not mistty--inhibit)))
    (mistty-wait-for-output :test (lambda () (mistty--queue-empty-p mistty--queue)))
    (should (equal (concat "$ if true\n"
                           "      echo ok\n"
                           "  end<>")
                   (mistty-test-content :show (point))))
    (mistty-send-and-wait-for-prompt)
    (should (equal (concat "$ if true\n"
                           "      echo ok\n"
                           "  end\n"
                           "ok\n"
                           "$ <>")
                   (mistty-test-content :show (point))))))
