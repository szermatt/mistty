;;; Tests compatibility with other packages -*- lexical-binding: t -*-

(require 'mistty)
(require 'mistty-testing)
(require 'thingatpt)
(require 'minibuffer)

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

(ert-deftest mistty-compat-test-wrap-capf ()
  (let ((completion-at-point-functions (list #'mistty-compat-test-capf))
        (completion-in-region-function #'mistty-compat-test-completion-in-region)
        (mistty-wrap-capf-functions t))
  (mistty-with-test-buffer (:shell fish)
    (let (start)
      (mistty-send-text "echo hello")
      (mistty-send-and-wait-for-prompt)

      (setq start (pos-bol))

      ;; hello should be suggested
      (mistty-send-text "echo h")
      (should (equal "$ echo h<>ello"
                     (mistty-test-content
                      :start start :show (point))))
      (mistty-run-command
       (completion-at-point))

      ;; hallo doesn't start with hello, but it does start with h.
      (should (equal "$ echo hallo<>"
                     (mistty-test-content
                      :start start :show (point))))))))

(ert-deftest mistty-compat-test-wrap-capf-in-scrollback-region ()
  (let ((completion-at-point-functions (list #'mistty-compat-test-capf))
        (completion-in-region-function #'mistty-compat-test-completion-in-region)
        (mistty-wrap-capf-functions t))
  (mistty-with-test-buffer (:shell fish)
    (mistty-send-text "echo hel")
    (mistty-send-and-wait-for-prompt)

    (mistty-test-goto-after "echo h")
    (should (equal
             (concat "$ echo h<>el\n"
                     "hel\n"
                     "$")
             (mistty-test-content
              :show (point))))
    (mistty-run-command
     (completion-at-point))

    ;; completion-at-point should have seen "hel" not just h "h"
    (should (equal
             (concat "$ echo hello<>\n"
                     "hel\n"
                     "$")
             (mistty-test-content
              :show (point)))))))

(ert-deftest mistty-compat-test-wrap-capf-disabled ()
  (let ((completion-at-point-functions (list #'mistty-compat-test-capf))
        (completion-in-region-function #'mistty-compat-test-completion-in-region)
        (mistty-wrap-capf-functions nil))
  (mistty-with-test-buffer (:shell bash)
    (let (start)
      (mistty-send-text "echo hello")
      (mistty-send-and-wait-for-prompt)

      (setq start (pos-bol))

      (mistty-send-text "echo ho")
      (mistty-run-command
       (goto-char (1- (point))))
      (should (equal "$ echo h<>o"
                     (mistty-test-content
                      :start start :show (point))))

      (mistty-run-command
       (completion-at-point))

      ;; completion-at-point should have seen "ho" not just "h"
      (should (equal "$ echo ho!ho!ho!<>"
                     (mistty-test-content
                      :start start :show (point))))))))

(defun mistty-compat-test-capf ()
  "Test function for `completion-at-point-functions'."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (list (car bounds) (cdr bounds)
          (list "ho!ho!ho!" "hi" "hello" "hallo"))))

(defun mistty-compat-test-completion-in-region (start end collection &optional _)
  "Strange `completion-in-region-function'.

It overwrites START-END with the *last* element of COLLECTION
that starts with the text currently between START and END."
  (let ((text (buffer-substring-no-properties start end)))
    (when-let ((replacement (car
                             (last
                              (delq nil
                                    (mapcar (lambda (completion)
                                              (when (string-prefix-p text completion)
                                                completion))
                                            collection))))))
      (goto-char start)
      (delete-region start end)
      (insert replacement))))
