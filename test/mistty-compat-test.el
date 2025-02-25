;;; Tests compatibility with other packages -*- lexical-binding: t -*-

(require 'mistty)
(require 'mistty-testing)
(require 'thingatpt)
(require 'minibuffer)
(require 'cua-base)

(require 'yasnippet)
(require 'tempel)

(ert-deftest mistty-test-detect-foreign-overlay-cua-rectangle ()
  (let ((mistty-detect-foreign-overlays t)
        (orig-cua-mode cua-mode))
    (unwind-protect
        (mistty-with-test-buffer (:selected t :shell fish)
          ;; CUA rectangle mark mode is an example of an interactive command
          ;; that uses overlays. It has the advantage of being built-in.
          ;; Other examples would be template engines, such as yasnippet and
          ;; templ.
          (cua-mode 'on)
          (mistty-send-text "for i in a b c\necho $i\nend")
          (mistty-test-goto "in")
          (execute-kbd-macro (kbd "C-<return> <down> <right> <right> b o o SPC"))
          (mistty-wait-for-output :test (lambda ()
                                          (equal '(mistty-overlays) mistty--inhibit)))
          (should (mistty-long-running-command-p))

          (should (equal (concat "$ for i in boo a b c\n"
                                 "      echo<> boo $i\n"
                                 "  end")
                         (mistty-test-content :show (point))))
          (execute-kbd-macro (kbd "C-<return>"))
          (mistty-wait-for-output :str "boo a b c" :start (point-min))
          (mistty-wait-for-output :test (lambda ()
                                          (not mistty--inhibit)))
          (should-not (mistty-long-running-command-p))
          (should (equal (concat "$ for i in boo a b c\n"
                                 "      echo<> boo $i\n"
                                 "  end")
                         (mistty-test-content :show (point)))))
      ;; unwind
      (cua-mode (if orig-cua-mode nil -1)))))

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
      (let ((start (point)))
        (mistty-send-text "echo h")
        (mistty-wait-for-output :str "echo hello" :start start))
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

(ert-deftest mistty-compat-test-tempel-smoke ()
  ;; This makes sure that the tempel integration works at all
  (ert-with-test-buffer ()
    (let* ((mistty-test-tempel-templates '((test "THIS IS A TEST")))
           (tempel-template-sources (list (lambda () mistty-test-tempel-templates))))
      (tempel-insert 'test)
      (should (equal "THIS IS A TEST" (mistty-test-content))))))

(ert-deftest mistty-compat-test-tempel-detect-overlays ()
  (mistty-with-test-buffer (:selected t)
    (let* ((mistty-test-tempel-templates '((test "for " p " in " p "; do " p "; done")))
           (tempel-template-sources (list (lambda () mistty-test-tempel-templates))))
      (keymap-local-set "C-c n" #'tempel-next)
      (keymap-local-set "C-c d" #'tempel-done)
      (mistty-run-command
       (tempel-insert 'test))
      (mistty-wait-for-output :test (lambda () mistty--inhibit))
      (execute-kbd-macro (kbd "i C-c n a SPC b SPC c C-c n e c h o SPC $ i C-c d"))
      (mistty-wait-for-output :test (lambda () (not mistty--inhibit)))
      (mistty-wait-for-output :str "for i in a b c; do echo $i; done"))))

(turtles-ert-deftest mistty-compat-goto-address-mode (:instance 'mistty)
  (unwind-protect
      (progn
        (global-goto-address-mode)
        (mistty-with-test-buffer (:selected t)
          (mistty-send-text "echo http://www.example.com/")
          (redisplay t) ;; force fontification
          (mistty-send-and-wait-for-prompt)
          (redisplay t)
          (goto-char (point-min))
          (search-forward "http://www.example.com")
          (should (mistty-test-has-goto-address-overlay-at (match-beginning 0)))
          (search-forward "http://www.example.com")
          (should (mistty-test-has-goto-address-overlay-at (match-beginning 0)))))
    (global-goto-address-mode -1)))

(defun mistty-test-has-goto-address-overlay-at (pos)
  "Check whether POS has goto-address overlays."
  (when (delq nil (mapcar (lambda (ov) (overlay-get ov 'goto-address))
                          (overlays-at pos)))
    t))
