;;; Tests mistty.el -*- lexical-binding: t -*-

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

(require 'mistty)
(require 'term)
(require 'ert)
(require 'ert-x)
(require 'generator)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-changeset)
(require 'mistty-queue)

(eval-when-compile
   ;; defined in term
  (defvar term-width))

(defvar mistty-test-bash-exe (executable-find "bash"))
(defvar mistty-test-python-exe (or (executable-find "python3")
                                   (executable-find "python")))
(defvar mistty-test-zsh-exe (executable-find "zsh")) ;; optional
(defvar mistty-test-fish-exe (executable-find "fish"));; optional

(defvar mistty-test-timeout (if noninteractive 10 3)
  "Time, in seconds, to wait for expected output in tests.

When running tests automatically, a larger value is useful to
avoid falkey tests in case the machine running the tests is slow.

When running tests manually, a smaller value is useful to avoid
waiting for failing test results.")

(defvar mistty-test-max-try-count
  (if noninteractive 5 mistty-max-try-count)
  "Value of `mistty-max-try-count' active in tests.

Tests have by default `mistty-debug-strict' enabled, which means
that not getting the expected effect is always an error, so
timing and try counts can be increased to account for slowness in
batch tests.")

(defvar mistty-test-timeout-s
  (if noninteractive 3 mistty-test-timeout-s)
  "Value of `mistty-timeout-s' active in tests.")

(defvar mistty-test-stable-delay-s mistty-stable-delay-s
  "Value of `mistty-stable-delay-s' active in tests.")

(defvar mistty-test-prompt "$ ")

(cl-defmacro mistty-with-test-buffer
    ((&key (shell 'bash) (selected nil)) &body body)
  "Run BODY in a MisTTY buffer.

SHELL specifies the program that is run in that buffer, bash,
zsh, fish or python.

If SELECTED is non-nil, make sure the buffer is in a selected
window while BODY is running."
  (declare (indent 1))
  (let ((exec-var (concat "mistty-test-" (symbol-name shell) "-exe")))
    `(progn
       ,(if (memq shell '(bash python))
            `(should ,exec-var)
          `(skip-unless ,exec-var))
       (ert-with-test-buffer ()
         (let ((mistty-debug-strict t)
               (mistty-max-try-count mistty-test-max-try-count)
               (mistty-timeout-s mistty-test-timeout-s)
               (mistty-stable-delay-s mistty-test-stable-delay-s)
               (mistty-test-prompt ,(if (eq shell 'python)
                                        ">>> "
                                      'mistty-test-prompt)))
           (mistty-test-setup (quote ,shell))
           ,(if selected
                `(with-selected-window (display-buffer (current-buffer))
                   ,@body)
              `(progn ,@body)))))))

(defmacro mistty-run-command (&rest body)
  `(progn
     (mistty--pre-command)
     (progn ,@body)
     (mistty-test-after-command)))

(ert-deftest test-mistty-simple-command ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello")
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-simple-command-zsh ()
  (mistty-with-test-buffer (:shell zsh)
   (mistty-send-text "echo hello")
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-simple-command-fish ()
  (mistty-with-test-buffer (:shell fish)
   (mistty-send-text "echo hello")
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-keystrokes ()
  (mistty-with-test-buffer (:selected t)
   (execute-kbd-macro (kbd "e c h o SPC o k"))
   (should (equal "ok" (mistty-send-and-capture-command-output
                        (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-mistty-keystrokes-backspace ()
  (mistty-with-test-buffer (:selected t)
   (execute-kbd-macro (kbd "e c h o SPC f o o DEL DEL DEL o k"))
   (should (equal "ok" (mistty-send-and-capture-command-output
                        (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-mistty-reconcile-insert ()
  (mistty-with-test-buffer ()
   (mistty-run-command
    (insert "echo hello"))
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-delete ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello")
   
   (mistty-run-command
      (mistty-test-goto "hello")
      (delete-region (point) (+ 3 (point))))
   
   (should (equal "$ echo <>lo" (mistty-test-content :show (point))))
   (should (equal "lo" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-delete-last-word ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello world")
   (mistty-run-command
    (save-excursion
      (mistty-test-goto " world")
      (delete-region (point) (point-max))))
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-large-multiline-delete ()
  (mistty-with-test-buffer (:shell fish)
   (mistty-send-text "for i in (seq 10)\necho this is a very long string to be deleted $i\nend")

   (mistty-run-command
    (goto-char (point-min))
    (search-forward "this is a very long string to be deleted")
    (goto-char (match-beginning 0))
    (delete-region (match-beginning 0) (match-end 0))
    (insert "foo"))
   (mistty-wait-for-output :str "foo")

   (should (equal (concat "$ for i in (seq 10)\n"
                          "      echo foo<> $i\n"
                          "  end")
                  (mistty-test-content :show (point))))))

(ert-deftest test-mistty-reconcile-replace ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello")
   (mistty-run-command
    (goto-char (point-min))
    (search-forward "hello")
    (replace-match "bonjour" nil t))
   
   (should (equal "$ echo bonjour<>" (mistty-test-content :show (point))))
   (should (equal "bonjour" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-replace-with-point-outside-of-change ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello, hello")
   (mistty-run-command
    (goto-char (point-min))
    (while (search-forward "hello" nil t)
      (replace-match "bonjour" nil t))
    (mistty-test-goto "echo"))
   
   (should (equal "$ <>echo bonjour, bonjour" (mistty-test-content :show (point))))
   (should (equal "bonjour, bonjour" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-replace-with-point-after-change ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello, hello world")
   (mistty-run-command
    (goto-char (point-min))
    (while (search-forward "hello" nil t)
      (replace-match "bonjour" nil t))
    (mistty-test-goto "world"))

   (should (equal "$ echo bonjour, bonjour <>world"
                  (mistty-test-content :show (point))))))

(ert-deftest test-mistty-reconcile-multiple-replace ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo boo boo, white goat")
   (mistty-run-command
    (goto-char (point-min))
    (while (search-forward "boo" nil t)
      (replace-match "baa" nil t))
    (goto-char (point-min))
    (search-forward "white goat")
    (replace-match "black sheep" nil t)
    (mistty-test-goto "black"))
   
   (should (equal "$ echo baa baa, <>black sheep" (mistty-test-content :show (point))))
   (should (equal "baa baa, black sheep"
                  (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-change-before-prompt ()
  (mistty-with-test-buffer ()
   (let (beg end)
     (mistty-send-text "echo hello")
     (save-excursion
       (setq beg (mistty-test-goto "hello"))
       (setq end (mistty-test-goto-after "hello")))
     (mistty-send-and-wait-for-prompt)
     (mistty-send-text "echo world")  
     (should (equal "$ echo hello\nhello\n$ echo world<>" (mistty-test-content :show (point))))
     (mistty-run-command
      (delete-region beg end)
      (goto-char beg)
      (insert "bonjour"))
     ;; the modification is available and the point is after the insertion
     (should (equal "$ echo bonjour<>\nhello\n$ echo world" (mistty-test-content :show (point))))
     
     ;; the next command executes normally and doesn't revert the
     ;; modification, though it moves the point.
     (mistty-test-goto "world")
     (mistty-send-and-wait-for-prompt)
     (should (equal "$ echo bonjour\nhello\n$ echo world\nworld\n$ <>"
                    (mistty-test-content :show (point)))))))

(ert-deftest test-mistty-send-command-because-at-prompt ()
  (mistty-with-test-buffer (:selected t)
   (mistty-send-text "echo hello")
   (should (equal "hello" (mistty-send-and-capture-command-output
                           (lambda ()
                             (execute-kbd-macro (kbd "RET"))))))
   (should (equal "$ echo hello\nhello\n$" (mistty-test-content)))))

(ert-deftest test-mistty-send-newline-because-not-at-prompt ()
  (mistty-with-test-buffer (:selected t)
   (mistty-send-text "echo hello")
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (mistty-test-goto "hello"))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "$ echo\n<>hello\nhello\n$" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-send-newline-because-not-at-prompt-multiline ()
  (mistty-with-test-buffer (:selected t)
   (mistty-run-command
    (insert "echo hello\necho world"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (mistty-test-goto "hello"))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "$ echo\n<>hello\necho world\nhello\nworld\n$" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-send-tab-to-complete  ()
  (mistty-with-test-buffer ()
   (mistty-send-text "ech world")
   ;; Move the point before doing completion, to make sure that
   ;; mistty-send-if-at-prompt moves the cursor to the right position
   ;; before sending TAB.
   (mistty-run-command
    (goto-char (+ (point-min) 5)))
   (should (equal "$ ech<> world" (mistty-test-content :show (point))))
   (mistty-send-key 1 "\t")
   (mistty-wait-for-output :str "echo")
   (should (equal "$ echo<> world" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-kill-term-buffer-when-work-buffer-is-killed ()
  (let* ((buffer-and-proc (mistty-with-test-buffer ()
                           (cons mistty-term-buffer mistty-proc)))
         (term-buffer (car buffer-and-proc))
         (term-proc (cdr buffer-and-proc)))
    (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)))

(ert-deftest test-mistty-kill-term-buffer-but-keep-work-buffer ()
  (mistty-with-test-buffer ()
   (let* ((term-buffer mistty-term-buffer)
          (term-proc mistty-proc))
    (kill-buffer term-buffer)
    (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)
    (mistty-wait-for-output :str "Terminal killed." :start (point-min))
    (should (equal (point) (point-max))))))

(ert-deftest test-mistty-term-buffer-exits ()
  (mistty-with-test-buffer ()
   (let ((proc mistty-proc)
         (term-buffer mistty-term-buffer))
     (mistty-send-text "exit")
     (mistty-send-command)
     (mistty-wait-for-output :str "finished" :start (point-min))
     (mistty-wait-for-term-buffer-and-proc-to-die term-buffer proc)

     ;; Disabled because it makes the test flakey, for some
     ;; unknown reasons.
     ;;(should (equal (point) (point-max)))
     )))

(ert-deftest test-mistty-scroll-with-long-command ()
  (mistty-with-test-buffer ()
   (let ((loop-command "for i in {0..49}; do echo line $i; done"))
     (mistty-send-text loop-command)
     (should (equal (concat "$ " loop-command "<>") (mistty-test-content :show (point))))
     (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 49) "\n")
                    (mistty-send-and-capture-command-output))))))

(ert-deftest test-mistty-scroll-with-many-commands ()
  (mistty-with-test-buffer ()
   (let ((loop-command "for i in {0..4}; do echo line $i; done"))
     (dotimes (_ 10)
       (mistty-send-text loop-command)
       (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 4) "\n")
                      (mistty-send-and-capture-command-output nil nil 'nopointer)))))))

(ert-deftest test-mistty-bracketed-paste ()
  (mistty-with-test-buffer ()
   (should (equal mistty-bracketed-paste t))
   (mistty-send-text "printf '(%s/%s) ? ' y n && read yesorno && echo answer: $yesorno")
   (mistty-send-and-wait-for-prompt nil "(y/n) ? ")
   (should (equal mistty-bracketed-paste nil))
   (mistty-send-text "no")
   (should (equal "answer: no" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-eol ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello")
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))

   (mistty-run-command
    (mistty-send-beginning-of-line))
   (mistty-wait-for-output
    :test (lambda ()
            (equal "$ <>echo hello" (mistty-test-content  :show (point)))))
   
   (mistty-run-command
    (mistty-send-end-of-line))
   (mistty-wait-for-output
    :test (lambda ()
            (equal "$ echo hello<>" (mistty-test-content :show (point)))))))

(ert-deftest test-mistty-eol-empty-prompt ()
  (mistty-with-test-buffer ()
   (goto-char (point-min))
   (mistty-run-command
    (mistty-send-end-of-line))
   
   (should
    (equal "$ <>"
           (mistty-test-content :show (point))))))

(ert-deftest test-mistty-next-prompt ()
  (mistty-with-test-buffer ()
   (mistty-run-command
    (insert "echo one"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo two"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo three"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))
   
   (goto-char (point-min))
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]<>echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]<>echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]<>echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (should-error (mistty-next-prompt 1))
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (goto-char (point-min))
   (mistty-next-prompt 2)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]<>echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 2)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))))

(ert-deftest test-mistty-next-prompt-zsh ()
  (mistty-with-test-buffer (:shell zsh)
   (mistty-run-command
    (insert "echo one"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo two"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo three"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))
   
   (goto-char (point-min))
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]<>echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]<>echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))))

(ert-deftest test-mistty-next-prompt-fish ()
  (mistty-with-test-buffer (:shell fish)
   (mistty-run-command
    (insert "echo one"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo two"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo three"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))

   (goto-char (point-min))
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]<>echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]<>echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))))

(ert-deftest test-mistty-next-empty-prompt ()
  (mistty-with-test-buffer ()
   (mistty-send-and-wait-for-prompt)
   (mistty-send-and-wait-for-prompt)
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))
   
   (goto-char (point-min))
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]<>\n"
                   "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]<>\n"
                   "[$ ]\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]<>\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-next-prompt 1)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (should-error (mistty-next-prompt 1))
   
   (goto-char (point-min))
   (mistty-next-prompt 2)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]<>\n"
                   "[$ ]\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))

   (mistty-next-prompt 2)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))))

(ert-deftest test-mistty-previous-empty-prompt ()
  (mistty-with-test-buffer ()
   (mistty-send-and-wait-for-prompt)
   (mistty-send-and-wait-for-prompt)
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))
   
   (mistty-previous-prompt 1)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-previous-prompt 1)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]<>\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-previous-prompt 1)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]<>\n"
                   "[$ ]\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-previous-prompt 1)
   (should
    (equal (concat "[$ ]<>\n"
                   "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (should-error (mistty-previous-prompt 1))
   
   (goto-char (point-max))
   (mistty-previous-prompt 2)
   (should
    (equal (concat "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]<>\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))

   (mistty-previous-prompt 2)
   (should
    (equal (concat "[$ ]<>\n"
                   "[$ ]\n"
                   "[$ ]\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))))

(ert-deftest test-mistty-next-python-prompt ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "1 + 1")
   (mistty-send-and-wait-for-prompt nil ">>> ")
   
   (mistty-send-text "2 + 2")

   (goto-char (point-min))
   (mistty-next-prompt 1)
   (should (equal (concat ">>> <>1 + 1\n"
                          "2\n"
                          ">>> 2 + 2")
                  (mistty-test-content :show (point))))

   (mistty-next-prompt 1)
   (should (equal (concat ">>> 1 + 1\n"
                          "2\n"
                          ">>> <>2 + 2")
                  (mistty-test-content :show (point))))))

(ert-deftest test-mistty-previous-prompt ()
  (mistty-with-test-buffer ()
   (mistty-run-command
    (insert "echo one"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo two"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo three"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))
   
   (mistty-previous-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]<>echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-previous-prompt 1)
   (should
    (equal (concat "[$ ]echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]<>echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (mistty-previous-prompt 2)
   (should
    (equal (concat "[$ ]<>echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))
   
   (should-error (mistty-previous-prompt 1))

   (should
    (equal (concat "[$ ]<>echo one\n"
                   "one\n"
                   "[$ ]echo two\n"
                   "two\n"
                   "[$ ]echo three\n"
                   "three\n"
                   "[$ ]echo current")
           (mistty-test-content :show (point)
                                :show-property '(mistty prompt))))))

(ert-deftest test-mistty-dirtrack ()
  (mistty-with-test-buffer ()
   (mistty-send-text "cd /")
   (mistty-send-and-wait-for-prompt)
   (should (equal "/" default-directory))
   (mistty-send-text "cd ~")
   (mistty-send-and-wait-for-prompt)
   (should (equal (file-name-as-directory (getenv "HOME")) default-directory))))

(ert-deftest test-mistty-bash-backward-history-search ()
  (mistty-with-test-buffer (:selected t)
   (mistty-run-command
    (insert "echo first"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo second"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo third"))
   (mistty-send-and-wait-for-prompt)
   (narrow-to-region (mistty--bol (point)) (point-max))
   (mistty--send-string mistty-proc "?\C-r")
   (mistty-wait-for-output :str "reverse")
   (should (equal "(reverse-i-search)`': ?<>" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "e c"))
   (mistty-wait-for-output :str "`ec'")
   (should (equal "(reverse-i-search)`ec': <>echo third" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "o"))
   (mistty-wait-for-output :str "`eco'")
   (should (equal "(reverse-i-search)`eco': echo s<>econd" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "DEL"))
   (mistty-wait-for-output :str "`ec'")
   (should (equal "(reverse-i-search)`ec': echo s<>econd" (mistty-test-content :show (point))))
   (should (equal "second" (mistty-send-and-capture-command-output
                            (lambda ()
                              (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-mistty-skipped-spaces ()
  (mistty-with-test-buffer (:shell fish)
   (mistty-send-text "for i in (seq 10)\necho line $i\nend")

   (should (equal (concat "$ for i in (seq 10)\n"
                          "[      ]echo line $i\n"
                          "[  ]end")
                  (mistty-test-content
                   :strip-last-prompt t
                   :show-property '(mistty-skip t))))))

(ert-deftest test-mistty-insert-long-prompt ()
  (mistty-with-test-buffer ()
   (mistty--set-process-window-size 20 20)

   (mistty-run-command
    (insert "echo one two three four five six seven eight nine"))
   (mistty-wait-for-output :str "nine")
   (should (equal "$ echo one two three\n four five six seven\n eight nine"
                  (mistty-test-content)))))

(ert-deftest test-mistty-keep-sync-marker-on-long-prompt ()
  (mistty-with-test-buffer ()
   (mistty--set-process-window-size 20 20)

   (mistty-run-command
    (insert "echo one two three four five six seven eight nine"))
   (mistty-wait-for-output :str "nine")   

   ;; make sure that the newlines didn't confuse the sync marker
   (should (equal (marker-position mistty-sync-marker) (point-min)))
   (should (equal (marker-position mistty--cmd-start-marker) (mistty-test-goto "echo one")))))

(ert-deftest test-mistty-keep-pointer-on-long-prompt ()
  (mistty-with-test-buffer ()
   (mistty--set-process-window-size 20 20)

   (mistty-run-command
    (insert "echo one two three four five six seven eight nine"))

   ;; make sure that the newlines don't confuse mistty--post-command
   ;; moving the cursor.
   (dolist (count '("three" "nine" "four"))
     (let ((goal-pos))
       (mistty-run-command
        (setq goal-pos (mistty-test-goto count)))
       (should (equal (mistty-cursor) goal-pos))))))

(ert-deftest test-mistty-enter-fullscreen ()
  (mistty-with-test-buffer (:selected t)
    (let ((bufname (buffer-name))
          (work-buffer mistty-work-buffer)
          (term-buffer mistty-term-buffer)
          (proc mistty-proc))

      (should (executable-find "vi"))
      (execute-kbd-macro (kbd "v i RET"))
      (mistty-wait-for-output
       :proc proc
       :test
       (lambda ()
         (buffer-local-value 'mistty-fullscreen work-buffer)))
      (should (eq mistty-term-buffer (window-buffer (selected-window))))
      (should (equal (concat bufname " scrollback") (buffer-name work-buffer)))
      (should (equal bufname (buffer-name term-buffer)))
      (with-current-buffer term-buffer
        (should (eq mistty-fullscreen-map (current-local-map))))
      
      (execute-kbd-macro (kbd ": q ! RET"))
      (mistty-wait-for-output
       :proc proc
       :test
       (lambda ()
         (not (buffer-local-value 'mistty-fullscreen work-buffer))))
      (should (eq mistty-work-buffer (window-buffer (selected-window))))
      (should (equal (concat " mistty tty " bufname) (buffer-name term-buffer)))
      (should (equal bufname (buffer-name work-buffer))))))

(defun mistty-test-enter-fullscreen (on-seq off-seq)
   (let ((work-buffer mistty-work-buffer)
         (proc mistty-proc))

     (mistty-send-text
      (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off'"
              on-seq off-seq))
     (mistty-send-command)
      (mistty-wait-for-output
       :proc proc
       :test
       (lambda ()
         (buffer-local-value 'mistty-fullscreen work-buffer)))
     (should (eq mistty-term-buffer (window-buffer (selected-window))))

     (execute-kbd-macro (kbd "RET"))
      (mistty-wait-for-output
       :proc proc
       :test
       (lambda ()
         (not (buffer-local-value 'mistty-fullscreen work-buffer))))
     (should (eq mistty-work-buffer (window-buffer (selected-window))))))

(ert-deftest test-mistty-enter-fullscreen-alternative-code ()
  (mistty-with-test-buffer (:selected t)
    (mistty-test-enter-fullscreen "[?47h" "[?47l")))

(ert-deftest test-mistty-enter-fullscreen-1047 ()
  (mistty-with-test-buffer (:selected t)
    (mistty-test-enter-fullscreen "[?1047h" "[?1047l")))

(ert-deftest test-mistty-enter-fullscreen-1049 ()
  (mistty-with-test-buffer (:selected t)
    (mistty-test-enter-fullscreen "[?1049h" "[?1049l")))

(ert-deftest test-mistty-live-buffer-p ()
  (mistty-with-test-buffer ()
   (should (mistty-live-buffer-p mistty-work-buffer))
   (should (not (mistty-live-buffer-p mistty-term-buffer))))
  (with-temp-buffer
    (should (not (mistty-live-buffer-p (current-buffer))))))

(ert-deftest test-mistty-fullscreen-live-buffer-p ()
  (mistty-with-test-buffer ()
   (mistty-send-text
    (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off'"
            "[47h" "[47l"))
   (mistty-send-command)
   (mistty-wait-for-output
    :test
    (lambda ()
      (buffer-local-value 'mistty-fullscreen mistty-work-buffer)))

   (should (not (mistty-live-buffer-p mistty-work-buffer)))
   (should (mistty-live-buffer-p mistty-term-buffer))))


(ert-deftest test-mistty-kill-fullscreen-buffer-kills-scrollback ()
  (mistty-with-test-buffer (:selected t)
    (let ((work-buffer mistty-work-buffer)
          (proc mistty-proc))
      (should (executable-find "vi"))
      (execute-kbd-macro (kbd "v i RET"))
      (mistty-wait-for-output :test (lambda () mistty-fullscreen))

      (kill-buffer mistty-term-buffer)
      (mistty-wait-for-term-buffer-and-proc-to-die work-buffer proc))))

(ert-deftest test-mistty-proc-dies-during-fullscreen ()
  (mistty-with-test-buffer (:selected t)
    (let ((bufname (buffer-name))
          (work-buffer mistty-work-buffer)
          (term-buffer mistty-term-buffer)
          (proc mistty-proc))
      (should (executable-find "vi"))
      (execute-kbd-macro (kbd "v i RET"))
      (mistty-wait-for-output :test (lambda () mistty-fullscreen))

      (signal-process proc 'SIGILL)

      (mistty-wait-for-term-buffer-and-proc-to-die term-buffer proc)

      (should (buffer-live-p work-buffer))
      (should (eq work-buffer (window-buffer (selected-window))))
      (should (string-match "illegal instruction" (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal bufname (buffer-name work-buffer)))
      (should (not (buffer-local-value 'mistty-fullscreen mistty-work-buffer))))))

(ert-deftest test-mistty-collect-modifications-delete-after-replace ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 9)
    (goto-char 6)
    (insert "new-value")
    
    (delete-region 18 21)

    (should (equal "$ abcnew-valueghimno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))

    ;; The deletion is reported first, even though it was applied
    ;; last. If we did the reverse and a newline was inserted in the
    ;; middle of new-value, the deletion would not apply to the right
    ;; region.
    (should (equal '((12 "" 3) (6 "new-value" 3)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-collect-modifications-delete-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 (point-max))
    
    (should (equal "$ abc" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((6 "" -1)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-collect-modifications-insert-then-delete-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 (point-max))
    (goto-char 6)
    (insert "new-value")
    
    (should (equal "$ abcnew-value" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((6 "new-value" -1)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-collect-modifications-insert-skip-then-delete-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 15 (point-max))
    (delete-region 9 12)
    (goto-char 6)
    (insert "new-value")
    
    (should (equal "$ abcnew-valuedefjkl" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((15 "" -1) (9 "" 3) (6 "new-value" 0)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-collect-modifications-inserts ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (goto-char 12)
    (insert "NEW")
    
    (goto-char 9)
    (insert "NEW")
    
    (goto-char 6)
    (insert "NEW")
    
    (should (equal "$ abcNEWdefNEWghiNEWjklmno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((12 "NEW" 0) (9 "NEW" 0) (6 "NEW" 0)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-collect-modifications-insert-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdef")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (goto-char 9)
    (insert "NEW")
    
    (should (equal "$ abcdefNEW" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((9 "NEW" 0)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-collect-modifications-replaces ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (goto-char 12)
    (delete-region 12 15)
    (insert "NEW")
    
    (goto-char 6)
    (delete-region 6 9)
    (insert "NEW")
    
    (should (equal "$ abcNEWghiNEWmno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((12 "NEW" 3) (6 "NEW" 3)) (mistty--changeset-modifications (mistty--active-changeset)))))))

(ert-deftest test-mistty-restrict-intervals ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 9)
    (goto-char 6)
    (insert "new-value")
    
    (delete-region 18 21)

    (should (equal "$ abcnew-valueghimno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    ;;             "$ abcdefg      hijklmno<<end>>"
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted)
                       (15 shift -6)
                       (18 shift -3)) (mistty--changeset-collect cs)))
      (should (equal -6 (mistty--changeset-restrict cs 16)))
      (should (equal '((16 shift 0) (18 shift 3)) (mistty--changeset-intervals cs)))
      (should (equal '((18 "" 3)) (mistty--changeset-modifications cs)))))))

(ert-deftest test-mistty-restrict-intervals-before-changes ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcd<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (goto-char 6)
    (insert "new-value")
    
    (should (equal "$ abcnew-valued<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 shift -9)) (mistty--changeset-collect cs)))
      (should (equal 0 (mistty--changeset-restrict cs 4)))
      (should (equal '((6 inserted) (15 shift -9)) (mistty--changeset-intervals cs)))))))

(ert-deftest test-mistty-restrict-intervals-exactly-before-insert ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcd<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 7)
    (goto-char 6)
    (insert "new-value")
    
    (should (equal "$ abcnew-value<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted)
                       (15 shift -8)) (mistty--changeset-collect cs)))
      (should (equal 0 (mistty--changeset-restrict cs 6)))
      (should (equal '((6 inserted) (15 shift -8)) (mistty--changeset-intervals cs)))))))

(ert-deftest test-mistty-restrict-intervals-exactly-before-shift ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcd<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 7)
    
    (should (equal "$ abc<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 shift 1)) (mistty--changeset-collect cs)))
      (should (equal 0 (mistty--changeset-restrict cs 6)))
      (should (equal '((6 shift 1)) (mistty--changeset-intervals cs)))))))

(ert-deftest test-mistty-restrict-intervals-starts-within-insert ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 9)
    (goto-char 6)
    (insert "new-value")
    
    (delete-region 18 21)

    (should (equal "$ abcnew-valueghimno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    ;;             "$ abcdef ghijklmno<<end>>"
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 shift -6) (18 shift -3)) (mistty--changeset-collect cs)))
      (should (equal -1 (mistty--changeset-restrict cs 10)))
      (should (equal '((10 inserted) (15 shift -5) (18 shift -2)) (mistty--changeset-intervals cs)))
      (should (equal '((13 "" 3) (10 "value" 0)) (mistty--changeset-modifications cs)))))))

(ert-deftest test-mistty-restrict-intervals-starts-within-insert-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdef")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (goto-char 9)
    (insert "NEW")
    
    (should (equal "$ abcdefNEW" (buffer-substring-no-properties (point-min) (point-max))))
    ;;             "$ abcdef"
    (let ((cs (mistty--active-changeset)))
      (should (equal '((9 inserted)) (mistty--changeset-collect cs)))
      (should (equal nil (mistty--changeset-restrict cs 10)))))))

(ert-deftest test-mistty-restrict-intervals-within-insert-then-delete-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
      (insert "$ ")
      (move-overlay ov (point) (point-max-marker))
      (setq mistty-sync-marker (point))
      
      (insert "abcdefghijklmno<<end>>")
      (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
      (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))
      
      (delete-region 6 (point-max))
      (goto-char 6)
      (insert "new-value")
      
      (should (equal "$ abcnew-value" (buffer-substring-no-properties (point-min) (point-max))))
      
      (let ((cs (mistty--active-changeset)))
        (should (equal '((6 inserted) (15 deleted-to-end)) (mistty--changeset-collect cs)))
        (should (equal nil (mistty--changeset-restrict cs 10)))))))

(ert-deftest test-mistty-restrict-intervals-within-delete-at-end ()
  (ert-with-test-buffer ()
    (let ((ov (make-overlay 1 1 nil nil 'rear-advance)))
    (insert "$ ")
    (move-overlay ov (point) (point-max-marker))
    (setq mistty-sync-marker (point))
    
    (insert "abcdefghijklmno<<end>>")
    (overlay-put ov 'modification-hooks (list #'mistty--modification-hook))
    (overlay-put ov 'insert-behind-hooks (list #'mistty--modification-hook))

    (delete-region 6 (point-max))
    (goto-char 6)
    (insert "new-value")
    
    (should (equal "$ abcnew-value" (buffer-substring-no-properties (point-min) (point-max))))

    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 deleted-to-end)) (mistty--changeset-collect cs)))
      (should (equal nil (mistty--changeset-restrict cs 15)))))))

(ert-deftest test-mistty-osc ()
  (mistty-with-test-buffer ()
   (let* ((osc-list)
          (mistty-osc-handlers
           `(("8" . ,(lambda (code text)
                       (push (cons code text) osc-list))))))
      (mistty-send-text "printf '\\e]8;;http://www.example.com\\aSome OSC\\e]8;;\\a!\\n'")
      (should (equal "Some OSC!" (mistty-send-and-capture-command-output)))
      (should (equal '(("8" . ";http://www.example.com") ("8" . ";")) (nreverse osc-list))))))

(ert-deftest test-mistty-osc-standard-end ()
  (mistty-with-test-buffer ()
   (let* ((osc-list)
          (mistty-osc-handlers
           `(("8" . ,(lambda (code text)
                       (push (cons code text) osc-list))))))
      (mistty-send-text "printf '\\e]8;;http://www.example.com\\e\\\\Some OSC\\e]8;;\\e\\\\!\\n'")
      (should (equal "Some OSC!" (mistty-send-and-capture-command-output)))
      (should (equal '(("8" . ";http://www.example.com") ("8" . ";")) (nreverse osc-list))))))

(ert-deftest test-mistty-osc-add-text-properties ()
  (mistty-with-test-buffer ()
   (let* ((start nil)
          (mistty-osc-handlers
           `(("f" . ,(lambda (_ text)
                       (cond
                        ((string= "start" text)
                         (setq start (point)))
                        ((string= "end" text)
                         (put-text-property
                          start (point) 'mistty-test t))
                        (t (error "unexpected: '%s'" text))))))))
     (mistty-send-text "printf 'abc \\e]f;start\\adef\\e]f;end\\a ghi\\n'")
     (should (equal "abc def ghi" (mistty-send-and-capture-command-output)))
     (mistty-test-goto "abc def ghi")
     (should (equal "abc [def] ghi"
                    (mistty-test-content
                     :start (point)
                     :show-property '(mistty-test t)
                     :strip-last-prompt t))))))

(ert-deftest mistty-test-split-osc-sequence ()
  (mistty-with-test-buffer ()
   (let* (osc-list
          (mistty-osc-handlers
           `(("999" . ,(lambda (_ text)
                         (push text osc-list))))))
     (mistty--emulate-terminal
      mistty-proc "foo\e]999;he" mistty-work-buffer)
     (mistty--emulate-terminal
      mistty-proc "llo, w" mistty-work-buffer)
     (mistty--emulate-terminal
      mistty-proc "orld\abar" mistty-work-buffer)
     (mistty--refresh)
     (should (equal "$ foobar" (mistty-test-content)))
     (should (equal '("hello, world") osc-list)))))

(ert-deftest mistty-test-decode-osc ()
  (mistty-with-test-buffer ()
   (let* (osc-list
          (mistty-osc-handlers
           `(("999" . ,(lambda (_ text)
                         (push text osc-list))))))
     (mistty--emulate-terminal
      mistty-proc
      "foo\e]999;\xce\xb1\xce\xb2\xce\xb3\abar"
      mistty-work-buffer)
     (mistty--refresh)
     (should (equal "$ foobar" (mistty-test-content)))
     (should (equal '("αβγ") osc-list)))))

(ert-deftest test-mistty-reset ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo one")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-text "printf '\\ec'")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-text "echo two")
   (mistty-send-and-wait-for-prompt)
   (should (equal "$ echo one\none\n$ printf '\\ec'\n$ echo two\ntwo\n$"
                  (mistty-test-content)))))

(ert-deftest test-mistty-clear-screen ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo one")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-text "printf '\\e[2J'")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-text "echo two")
   (mistty-send-and-wait-for-prompt)
   (should (equal "$ echo one\none\n$ printf '\\e[2J'\n$ echo two\ntwo"
                  (mistty-test-content :strip-last-prompt t)))))
   
(ert-deftest test-mistty-hide-cursor ()
  (mistty-with-test-buffer ()
   (mistty-send-text "printf 'o\\e[?25lk\\n'")
   (should (equal "ok" (mistty-send-and-capture-command-output)))
   (should (eq nil cursor-type))
   (mistty-send-text "printf 'o\\e[?25hk\\n'")
   (should (equal "ok" (mistty-send-and-capture-command-output)))
   (should (eq t cursor-type))))
   
(ert-deftest test-mistty-show-cursor-if-moved ()
  (mistty-with-test-buffer ()
   (mistty-send-text "printf 'o\\e[?25lk\\n'")
   (should (equal "ok" (mistty-send-and-capture-command-output)))
   (should (eq nil cursor-type))
   (mistty-send-text "echo ok")
   (mistty-run-command (goto-char (1- (point))))
   (should (eq t cursor-type))))

(ert-deftest test-mistty-detect-possible-prompt ()
  (mistty-with-test-buffer ()
   (mistty-send-text
    "printf 'say %s>> ' something; read something; echo something: $something")
   (mistty-send-command)
   (mistty-wait-for-output :str "say something")
   (mistty-send-text "foo")
   (should (equal
            (list
             (mistty-test-goto "say something>> ")
             (mistty-test-goto-after "say something>> ")
             "say something>> ")
            mistty--possible-prompt))))

(ert-deftest test-mistty-python-just-type ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "1 + 1")
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   ;; the prompt was identified and labelled
   (mistty-previous-prompt 1)
   (should (looking-at (regexp-quote "1 + 1")))))

(ert-deftest test-mistty-python-move-and-type ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "10 * 10")
   (mistty-run-command
    (mistty-test-goto "10 * 10")
    (goto-char (1+ (point))))
   (mistty-run-command
    (mistty-send-key 1 "0"))
   (should (equal "1000" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   ;; the prompt was identified and labelled
   (mistty-previous-prompt 1)
   (should (looking-at (regexp-quote "100 * 10")))))

(ert-deftest test-mistty-python-eof ()
  (mistty-with-test-buffer ()
   (should mistty-test-python-exe)
   (mistty-send-text mistty-test-python-exe)
   (mistty-send-and-wait-for-prompt nil ">>> ")
   (mistty-send-and-wait-for-prompt (lambda () (mistty-send-key 1 "\C-d")))))

(ert-deftest test-mistty-python-delchar ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "11 + 1")
   (mistty-run-command
    (mistty-test-goto "11 + 1")
    (mistty-send-key 1 "\C-d"))
   ;; deleted the first 1, the command-line is now 1 + 1
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))))

(ert-deftest test-mistty-python-beginning-of-line ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "1 + 1")
   (mistty-send-beginning-of-line)
   (mistty-wait-for-output
    :test (lambda ()
            (equal ">>> <>1 + 1"
                   (mistty-test-content
                    :start (mistty--bol (point)) :show (point)))))))

(ert-deftest test-mistty-python-edit-prompt ()
  (mistty-with-test-buffer (:shell python)
   (let ((start (- (point) 4)))
     (mistty-run-command
      (insert "10 * 10"))
     
     (should (equal "100" (mistty-send-and-capture-command-output nil nil nil ">>> ")))
     
     ;; the prompt was identified and labelled
     (mistty-previous-prompt 1)
     (should (equal ">>> <>10 * 10\n100\n>>>"
                    (mistty-test-content :start start :show (point)))))))

(ert-deftest test-mistty-python-edit-before-prompt ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "1 + 1")
   (should (equal "2" (mistty-send-and-capture-command-output)))

   (mistty-send-text "3 + 3")
   (should (equal "6" (mistty-send-and-capture-command-output)))

   (mistty-send-text "5 + 5")

   (mistty-run-command
    (goto-char (point-min))
    (while (search-forward "+" nil 'noerror)
      (replace-match "*")))

   ;; The last prompt became 5 * 5
   (should (equal "25" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   ;; the text of the previous prompts was modified, too.
   (mistty-test-goto "1 * 1")
   (mistty-test-goto "3 * 3")))

(ert-deftest test-mistty-more-edit-before-prompt ()
  (mistty-with-test-buffer (:shell python)
   (mistty-send-text "1 + 1")
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   (mistty-send-text "3 + 3")
   (should (equal "6" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   (mistty-send-text "5 + 5")

   (mistty-run-command
    (goto-char (point-min))
    (while (search-forward "+" nil 'noerror)
      (replace-match "*")))

   ;; The last prompt became 5 * 5
   (should (equal "25" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   ;; the text of the previous prompts was modified, too.
   (mistty-test-goto "1 * 1")
   (mistty-test-goto "3 * 3")))

(ert-deftest test-mistty-edit-without-prompt ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo hello, world")
   (mistty-send-and-wait-for-prompt)
   (let ((before-send (point)) after-line-10)
     (mistty-send-text "for i in {1..100} ; do echo \"line $i\"; done | more")
     (mistty-send-command)
     (mistty-wait-for-output 
      :test (lambda ()
              (goto-char before-send)
              (search-forward "line 10" nil 'no-error)))
     (setq after-line-10 (point-marker))
     ;; wait for more's prompt
     (mistty-wait-for-output
      :test (lambda ()
              (goto-char after-line-10)
              (search-forward-regexp "^[^l]" nil 'no-error)))

     ;; edit from prompt to line 10
     (mistty-run-command
      (goto-char (point-min))
      (while (search-forward "line" after-line-10 'noerror)
        (replace-match "replacement"))))

   ;; lines 1-10 are now called replacement
   (mistty-test-goto "replacement 1\nreplacement 2\n")
   (mistty-test-goto "replacement 9\nreplacement 10\nline 11\nline 12\n")

   ;; quit more and go back to the normal prompt
   (mistty-send-and-wait-for-prompt
    (lambda () (mistty--send-string mistty-proc "q")))))

(ert-deftest test-mistty-python-prompt-too-long ()
  (mistty-with-test-buffer (:shell python)
   (let ((line-start (mistty--bol (point))))
     (mistty-send-text "if a > b:")
     (mistty-run-command
      (mistty-send-beginning-of-line))
     (mistty-send-text "foobar" (point-min))

     (should (equal ">>> <>foobarif a > b:"
                    (mistty-test-content :start line-start
                                         :show mistty--cmd-start-marker))))))

(ert-deftest test-mistty-and-hippie-completion ()
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

(ert-deftest mistty-test-last-non-ws ()
  (ert-with-test-buffer ()
    (insert "This is a test\t   \r\n   \n  \t ")
    (goto-char (point-min))
    (should (equal (save-excursion (mistty-test-goto-after "test"))
                   (mistty--last-non-ws)))))

(ert-deftest mistty-test-positional ()
  (let ((mistty-positional-keys "\t\C-k\C-w"))
    (should (mistty-positional-p (kbd "TAB")))
    (should (mistty-positional-p (kbd "C-k")))
    (should (mistty-positional-p (kbd "C-w")))
    (should (not (mistty-positional-p (kbd "C-a"))))
    (should (not (mistty-positional-p (kbd "C-e"))))
    (should (mistty-positional-p (kbd "a")))
    (should (mistty-positional-p (kbd "α")))
    (should (not (mistty-positional-p (kbd "C-x C-c"))))
    (should (not (mistty-positional-p (kbd "M-g"))))))

(ert-deftest mistty-test-send-key ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (mistty-send-key 1 "e"))
    (mistty-run-command
     (mistty-send-key 1 "c"))
    (mistty-run-command
     (mistty-send-key 1 "h"))
    (mistty-run-command
     (mistty-send-key 1 "o"))
    (mistty-run-command
     (mistty-send-key 1 " "))
    (mistty-run-command
     (mistty-send-key 3 "a"))

    (should (equal "aaa" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-send-key-interactive ()
  (mistty-with-test-buffer (:selected t)
   (execute-kbd-macro (kbd "e c h o SPC C-u 3 a"))
   (should (equal "aaa" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-send-key-from-term-buffer ()
  (mistty-with-test-buffer ()
   (with-current-buffer mistty-term-buffer
     (mistty-send-key 1 "e")
     (mistty-send-key 1 "c")
     (mistty-send-key 1 "h")
     (mistty-send-key 1 "o")
     (mistty-send-key 1 " ")
     (mistty-send-key 1 "o")
     (mistty-send-key 1 "k"))
   (mistty-wait-for-output :str "echo ok")
   (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-send-last-key-from-term-buffer ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo ok nok")
   (with-current-buffer mistty-term-buffer
     (with-selected-window (display-buffer (current-buffer))
       (local-set-key (kbd "C-c C-w") #'mistty-send-last-key)
       (execute-kbd-macro (kbd "C-c C-w"))))
   (mistty-wait-for-output :regexp "echo ok *$")
   (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-raw-string-from-term-buffer ()
  (mistty-with-test-buffer ()
   (with-current-buffer mistty-term-buffer
     (mistty-send-text "echo ok"))

    (should (equal "ok" (mistty-send-and-capture-command-output)))))


(ert-deftest mistty-test-send-last-key ()
  (mistty-with-test-buffer (:selected t)
   (local-set-key (kbd "C-c C-w") 'mistty-send-last-key)
   (mistty-send-text "echo abc def")
   (execute-kbd-macro (kbd "C-c C-w"))
   (mistty-wait-for-output :regexp "abc *$")
   (should (equal "abc" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-C-q ()
  (mistty-with-test-buffer (:selected t)
   (mistty-send-text "echo abc def")
   (execute-kbd-macro (kbd "C-q C-w"))
   (mistty-wait-for-output :regexp "abc *$")
   (should (equal "abc" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-revert-modification-after-prompt ()
  (mistty-with-test-buffer (:shell zsh)
   (dotimes (i 3)
     (mistty-send-text (format "function toto%d { echo %d; };" i i)))
   (mistty-send-and-wait-for-prompt)
   (narrow-to-region (mistty--bol (point)) (point-max))
   (mistty-send-text "toto\t")

   ;; This test goes outside the prompt on purpose.
   ;; mistty-debug-strict would cause this test to fail, since the
   ;; cursor cannot be moved to the point.
   (let ((mistty-debug-strict nil)
         (mistty-max-try-count 1)
         (mistty-stable-delay-s 0.05)
         (mistty-timeout-s 0.25))
     (mistty-wait-for-output
      :test (lambda ()
              (search-forward-regexp "^toto" nil 'noerror)))
     (mistty-run-command
      (insert "foobar"))
     (should (equal "$ toto\ntoto<>0  toto1  toto2"
                    (mistty-test-content :show (point)))))))

(ert-deftest mistty-reset-during-replay ()
  (mistty-with-test-buffer ()
   (mistty-send-text "echo -n 'read> '; read l; printf 'will reset\\ecreset done\\n'")
   (mistty-send-and-wait-for-prompt nil "read> ")
   (let ((start (mistty--bol (point))))
     (mistty--enqueue
      mistty--queue
      (funcall (iter-lambda ()
                 (iter-yield "hello")
                 (while (not (mistty-test-find-p "hello"))
                   (iter-yield 'continue))
                 (iter-yield "\C-m")
                 (while (not (mistty-test-find-p "will reset"))
                   (iter-yield 'continue))
                 (iter-yield "bar")
                 (while (not (mistty-test-find-p "reset done"))
                   (iter-yield 'continue))
                 (iter-yield "bar"))))
     (mistty-wait-for-output :str "$ " :start start)
     
     (should (equal (concat "read> hello\n"
                            "will reset\n"
                            "reset done\n"
                            "$")
                    (mistty-test-content :start start)))
     ;; note: bar is lost as the replay was cancelled by the reset
     ;; triggered by the \n after foo. Make sure that inhibit-refresh
     ;; was reset correctly, which could happen if the generator's
     ;; unwind form wasn't executed.
     (should (not mistty--inhibit-refresh))
     (should (null mistty--changesets))
     (should (not mistty--need-refresh)))))

(ert-deftest mistty-test-end-prompt ()
  (mistty-with-test-buffer ()
   (mistty-send-text "for i in {1..10} ; do echo line $i; done && read l")
   (mistty-send-and-wait-for-prompt nil "line 10")
   (should
    (equal
     "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n"
     (mistty--safe-bufstring mistty-sync-marker (point-max))))))

(ert-deftest mistty-test-end-prompt-multiline-pasted ()
  (mistty-with-test-buffer ()
   (mistty-run-command
    (insert "for i in {1..10} ; do \necho line $i\n done && read l"))
   (mistty-send-and-wait-for-prompt nil "line 10")
   (should
    (equal
     "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n"
     (mistty--safe-bufstring mistty-sync-marker (point-max))))))

(ert-deftest mistty-test-fish-multiline ()
  (mistty-with-test-buffer (:shell fish)
   (should mistty-bracketed-paste)
   (mistty-send-text "echo 'hello\nworld\nand all that sort of things.'")

   (mistty-run-command
    (mistty-test-goto "sort")
    (insert ":::1"))
   (mistty-wait-for-output :str ":::1" :start (point-min))
   (should (equal "$ echo 'hello\n  world\n  and all that :::1<>sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))

   (mistty-run-command
    (mistty-test-goto "llo")
    (insert ":::2"))
   (mistty-wait-for-output :str ":::2" :start (point-min))
   (should (equal "$ echo 'he:::2<>llo\n  world\n  and all that :::1sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))

   (mistty-run-command
    (mistty-test-goto "rld")
    (insert ":::3"))
   (mistty-wait-for-output :str ":::3" :start (point-min))
   (should (equal "$ echo 'he:::2llo\n  wo:::3<>rld\n  and all that :::1sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))))

(ert-deftest mistty-test-fish-multiline-indented ()
  (mistty-with-test-buffer (:shell fish)
   (should mistty-bracketed-paste)
   (mistty-send-text "while i in (seq 10)\necho line $i\nend")

   (mistty-run-command
    (mistty-test-goto "line")
    (insert ":::"))
   (mistty-wait-for-output :str ":::" :start (point-min))
   (should (equal "$ while i in (seq 10)\n      echo :::<>line $i\n  end"
                  (mistty-test-content :show (mistty-cursor))))))

(ert-deftest mistty-test-bash-multiline ()
  (mistty-with-test-buffer ()
   (should mistty-bracketed-paste)

   (mistty-run-command
    (insert "echo 'hello\n  world\n  and all that sort of things.'"))
   ;; indentation might make it think it's a fish multiline prompt.

   (mistty-run-command
    (mistty-test-goto "rld"))
   (should (equal "$ echo 'hello\n  wo<>rld\n  and all that sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))

   (mistty-run-command
    (mistty-test-goto "llo"))
   (should (equal "$ echo 'he<>llo\n  world\n  and all that sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))

   (mistty-run-command
    (mistty-test-goto "things"))
   (should (equal "$ echo 'hello\n  world\n  and all that sort of <>things.'"
                  (mistty-test-content :show (mistty-cursor))))))


(ert-deftest mistty-test-truncation ()
  (let ((mistty-buffer-maximum-size 20))
    (mistty-with-test-buffer ()
     (mistty-send-text "for i in {0..1000}; do echo line $i; done")
     (mistty-send-and-wait-for-prompt)
     (ert-run-idle-timers)
     (should (<= (count-lines (point-min) (point-max)) 30)))))

(ert-deftest mistty-test-from-pos-of ()
  (mistty-with-test-buffer ()
   (mistty--send-string mistty-proc "echo foo")
   (mistty--send-string mistty-proc "\e[200~\n\e[201~")
   (mistty--send-string mistty-proc "echo hello world")
   (mistty-wait-for-output :str "hello world")
   
   (goto-char (point-min))
   (should (equal (mistty-test-pos "foo")
                  (mistty--from-term-pos
                   (with-current-buffer mistty-term-buffer
                     (goto-char (point-min))
                     (mistty-test-pos "foo")))))

   (goto-char (point-min))
   (should (equal (mistty-test-pos "hello")
                  (mistty--from-term-pos
                   (with-current-buffer mistty-term-buffer
                     (goto-char (point-min))
                     (mistty-test-pos "hello")))))))

(ert-deftest mistty-test-ignore-new-trailing-spaces-during-replay ()
  (mistty-with-test-buffer ()
   (mistty--send-string mistty-proc "echo foo")
   (mistty--send-string mistty-proc "\e[200~\n\e[201~")
   (mistty--send-string mistty-proc "echo hello world")
   (mistty--send-string mistty-proc "\e[200~\n\e[201~")
   (mistty--send-string mistty-proc "echo bar")
   (mistty-wait-for-output :str "bar")

   ;; This attempts to simulation a situation where the cursor goes
   ;; through columns that don't exist during replay and creates fake
   ;; spaces. It very much depends on the shell, however.
   (mistty-run-command
    (mistty-test-goto "foo")
    (insert "FOO:")
    (mistty-test-goto "hello")
    (insert "HELLO:")
    (mistty-test-goto "world")
    (insert "WORLD:")
    (mistty-test-goto "bar")
    (insert "BAR:"))

   (should (equal (concat "$ echo FOO:foo\n"
                          "echo HELLO:hello WORLD:world\n"
                          "echo BAR:<>bar")
                  (mistty-test-content :show (point))))))

(ert-deftest mistty-test-ignore-new-trailing-spaces-during-replay-fish ()
  (mistty-with-test-buffer (:shell fish)
   (mistty--send-string mistty-proc "for i in (seq 10)\necho boo line $i\nend")
   (mistty-wait-for-output :str "end")

   ;; This attempts to simulation a situation where the cursor goes
   ;; through columns that don't exist during replay and creates fake
   ;; spaces. It very much depends on the shell, however.
   (mistty-run-command
    (mistty-test-goto-after "(seq 10")
    (insert "1")

    (mistty-test-goto "boo")
    (insert "BOO:")

    (mistty-test-goto "line")
    (insert "LINE:")

    (mistty-test-goto-after "end")
    (insert "#done"))

   (mistty-wait-for-output :str "seq 101" :start (point-min))

   (should (equal (concat "$ for i in (seq 101)\n"
                          "      echo BOO:boo LINE:line $i\n"
                          "      end#done<>")
                  (mistty-test-content :show (point))))))

(ert-deftest mistty-test-cursor-skip-range ()
  (ert-with-test-buffer ()
    (insert "$ for i in (seq 10)" (propertize "  " 'mistty-skip t)"\n"
            (propertize "      " 'mistty-skip t) "echo line $i  \n"
            (propertize "  " 'mistty-skip t) "end" (propertize "  " 'mistty-skip t) "\n"
            "extra stuff goes here")

    ;; skip over spaces marked with 'mistty-skip and newlines
    ;; surrounded by such spaces.
    (should (equal (mistty-test-pos "echo")
                   (mistty--cursor-skip-forward (mistty-test-pos-after "(seq 10)"))))
    (should (equal (mistty-test-pos-after "(seq 10)")
                   (mistty--cursor-skip-backward (mistty-test-pos "echo"))))

    (should (equal (mistty-test-pos-after "$i  ")
                   (mistty--cursor-skip-backward (mistty-test-pos "end"))))
    (should (equal (mistty-test-pos "end")
                   (mistty--cursor-skip-forward (mistty-test-pos-after "$i  "))))

    (should (equal (mistty-test-pos "extra")
                   (mistty--cursor-skip-forward (mistty-test-pos-after "end"))))
    (should (equal (mistty-test-pos "extra")
                   (mistty--cursor-skip-forward (1- (mistty-test-pos "extra")))))

    ;; spaces not marked with 'mistty-skip are not skipped over
    (should (equal (mistty-test-pos-after "$i")
                   (mistty--cursor-skip-forward (mistty-test-pos-after "$i"))))))

(ert-deftest mistty-test-cursor-skip-hook-go-right ()
  (ert-with-test-buffer-selected ()
    (insert "$ for i in a b c" (propertize "  " 'mistty-skip t)"\n"
            (propertize "      " 'mistty-skip t) "echo line $i\n"
            (propertize "  " 'mistty-skip t) "end" (propertize "  " 'mistty-skip t) "\n"
            "extra stuff goes here")

    ;; go right from "for" to "end"
    (let ((mistty-skip-empty-spaces t)
          (mistty-bracketed-paste t)
          (mistty-sync-marker (point-min))
          (win (selected-window)))
      (set-window-point win (mistty-test-pos "for"))
      (mistty--cursor-skip win)
      (goto-char (mistty-test-pos-after "a b c"))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos-after "a b c") (window-point win)))
      (goto-char (1+ (mistty-test-pos-after "a b c")))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "echo") (window-point win)))
      (goto-char (1+ (mistty-test-pos-after "$i")))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "end") (window-point win))))))

(ert-deftest mistty-test-cursor-skip-hook-go-left ()
  (ert-with-test-buffer-selected ()
    (insert "$ for i in a b c" (propertize "  " 'mistty-skip t)"\n"
            (propertize "      " 'mistty-skip t) "echo line $i\n"
            (propertize "  " 'mistty-skip t) "end" (propertize "  " 'mistty-skip t) "\n"
            "extra stuff goes here")

    ;; go left from "end" to "for"
    (let ((mistty-skip-empty-spaces t)
          (mistty-bracketed-paste t)
          (mistty-sync-marker (point-min))
          (win (selected-window)))
      (set-window-point win (mistty-test-pos "end"))
      (mistty--cursor-skip win)
      (goto-char (1- (mistty-test-pos "end")))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos-after "$i") (window-point win)))
      (goto-char (mistty-test-pos "echo"))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "echo") (window-point win)))
      (goto-char (1- (mistty-test-pos "echo")))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos-after "a b c") (window-point win))))))

(ert-deftest mistty-test-cursor-skip-hook-go-down ()
  (ert-with-test-buffer-selected ()
    (insert "$ for i in a b c" (propertize "  " 'mistty-skip t)"\n"
            (propertize "      " 'mistty-skip t) "echo line $i\n"
            (propertize "  " 'mistty-skip t) "end" (propertize "  " 'mistty-skip t) "\n"
            "extra stuff goes here")

    ;; go down from "for" to "end"
    (let ((mistty-skip-empty-spaces t)
          (mistty-bracketed-paste t)
          (mistty-sync-marker (point-min))
          (win (selected-window)))
      (set-window-point win (mistty-test-pos "for"))
      (mistty--cursor-skip win)
      (vertical-motion 1 win)
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "echo") (window-point win)))
      (vertical-motion 1 win)
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "end") (window-point win))))))

(ert-deftest mistty-test-cursor-skip-hook-go-up ()
  (ert-with-test-buffer-selected ()
    (insert "$ for i in a b c" (propertize "  " 'mistty-skip t)"\n"
            (propertize "      " 'mistty-skip t) "echo line $i\n"
            (propertize "  " 'mistty-skip t) "end" (propertize "  " 'mistty-skip t) "\n"
            "extra stuff goes here")

    ;; go up from "end" to "for"
    (let ((mistty-skip-empty-spaces t)
          (mistty-bracketed-paste t)
          (mistty-sync-marker (point-min))
          (win (selected-window)))
      (set-window-point win (mistty-test-pos "end"))
      (mistty--cursor-skip win)
      (call-interactively 'previous-line)
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "echo") (window-point win)))
      (call-interactively 'previous-line)
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos-after "for ") (window-point win))))))

(ert-deftest mistty-test-cursor-skip-hook-not-on-a-prompt ()
  (ert-with-test-buffer-selected ()
    (insert "$ for i in a b c" (propertize "  " 'mistty-skip t)"\n"
            (propertize "      " 'mistty-skip t) "echo line $i\n"
            (propertize "  " 'mistty-skip t) "end" (propertize "  " 'mistty-skip t) "\n"
            "extra stuff goes here")

    (let ((mistty-skip-empty-spaces t)
          (mistty-bracketed-paste t)
          (mistty-sync-marker (point-min))
          (win (selected-window)))

      ;; skip enabled
      (set-window-point win (mistty-test-pos "a b c"))
      (mistty--cursor-skip win)
      (set-window-point win (1+ (mistty-test-pos-after "a b c")))
      (mistty--cursor-skip win)
      (should (equal (mistty-test-pos "echo") (window-point win)))

      ;; skip disabled because point is before mistty-sync-marker
      (let ((mistty-sync-marker (point-max)))
        (set-window-point win (mistty-test-pos "a b c"))
        (mistty--cursor-skip win)
        (set-window-point win (1+ (mistty-test-pos-after "a b c")))
        (mistty--cursor-skip win)
        (should (equal (1+ (mistty-test-pos-after "a b c")) (window-point win))))

      ;; skip disabled because mistty-bracketed-paste is nil
      (let ((mistty-bracketed-paste nil))
        (set-window-point win (mistty-test-pos "a b c"))
        (mistty--cursor-skip win)
        (set-window-point win (1+ (mistty-test-pos-after "a b c")))
        (mistty--cursor-skip win)
        (should (equal (1+ (mistty-test-pos-after "a b c")) (window-point win)))))))

(ert-deftest mistty-test-yank-handler ()
  (mistty-with-test-buffer ()
   (mistty--set-process-window-size 20 20)

   (mistty-run-command
    (insert "echo one two three four five six seven eight nine"))
   (mistty-wait-for-output :str "nine")
   (mistty-test-goto "one")
   (copy-region-as-kill (save-excursion
                          (mistty-test-goto "one")
                          (point))
                        (save-excursion
                          (mistty-test-goto-after "nine")
                          (point))))

  (ert-with-test-buffer ()
    (yank)

    (should
     (equal "one two three four five six seven eight nine"
            (mistty-test-content)))))
   
(ert-deftest test-mistty-fish-right-prompt-simple-command ()
  (mistty-with-test-buffer (:shell fish)
     (mistty-setup-fish-right-prompt)

     ;; Make sure the right prompt doesn't interfere with normal operations
     (mistty-send-text "echo hello")
     (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-fish-right-prompt-skip-empty-spaces ()
  (mistty-with-test-buffer (:shell fish :selected t)
     (mistty-setup-fish-right-prompt)
     (let ((mistty-skip-empty-spaces t)
           (win (selected-window)))
       (mistty--cursor-skip win)
       (should (string-match "^\\$ <> +< right$"
                             (mistty-test-content :show (point))))
       (right-char)
       (mistty--cursor-skip win)
       (should (string-match "^\\$ +< right\n<>$"
                             (mistty-test-content :show (point)))))))

(ert-deftest test-mistty-fish-right-prompt-yank ()
  (mistty-with-test-buffer (:shell fish)
     (mistty-setup-fish-right-prompt)
     (mistty-send-text "echo hello")
     (copy-region-as-kill (mistty--bol (point)) (mistty--eol (point)))
     (with-temp-buffer
       (yank)
       (should (equal "$ echo hello" (mistty-test-content))))))

(ert-deftest test-mistty-fish-right-prompt-skip-empty-spaces-at-prompt ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-setup-fish-right-prompt)
    (let* ((mistty-skip-empty-spaces t)
           (win (selected-window))
           (after-refresh (lambda () (mistty--cursor-skip win))))
      (advice-add 'mistty--refresh :after after-refresh)
      (unwind-protect
          (progn
            ;; skip-empty-space sometimes skips too much; to the end of the line.
            (mistty-send-and-wait-for-prompt)
            (should (string-match "\\$ <>" (mistty-test-content :show (point))))
            (mistty-send-and-wait-for-prompt)
            (should (string-match "\\$ <>" (mistty-test-content :show (point)))))
        (advice-remove 'mistty--refresh after-refresh)))))

(ert-deftest test-mistty-distance-with-fake-nl ()
  (ert-with-test-buffer ()
   (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
     (insert "echo one tw" fake-nl
             "o three fou" fake-nl
             "r five six " fake-nl
             "seven eight" fake-nl
             "nine")

     (should (equal 4 (mistty--distance (mistty-test-pos "one")
                                        (mistty-test-pos "tw"))))
     (should (equal -4 (mistty--distance (mistty-test-pos "tw")
                                         (mistty-test-pos "one"))))

     (should (equal 4 (mistty--distance (mistty-test-pos "tw")
                                        (mistty-test-pos "three"))))
     (should (equal -4 (mistty--distance (mistty-test-pos "three")
                                         (mistty-test-pos "tw"))))

     (should (equal 19 (mistty--distance (mistty-test-pos "one")
                                         (mistty-test-pos "five"))))

     (should (equal 0 (mistty--distance (+ 3 (mistty-test-pos "fou"))
                                        (+ 4 (mistty-test-pos "fou"))))))))

(ert-deftest test-mistty-distance-skipped-spaces ()
  (ert-with-test-buffer ()
   (insert "$ for i in (seq 10)" (propertize "  " 'mistty-skip t)"\n"
           (propertize "      " 'mistty-skip t) "echo line $i\n"
           (propertize "  " 'mistty-skip t) "end")

   (should (equal 1 (mistty--distance (+ 3 (mistty-test-pos "10)"))
                                      (mistty-test-pos "echo"))))

   (should (equal 1 (mistty--distance (+ 3 (mistty-test-pos "10)"))
                                      (1- (mistty-test-pos "echo")))))

   (should (equal 1 (mistty--distance (+ 4 (mistty-test-pos "10)"))
                                      (mistty-test-pos "echo"))))
   (should (equal 1 (mistty--distance (+ 4 (mistty-test-pos "10)"))
                                        (1- (mistty-test-pos "echo")))))

   (should (equal 31 (mistty--distance (mistty-test-pos "for")
                                       (mistty-test-pos "end"))))))

;; TODO: find a way of testing non-empty modifications that are
;; ignored and require the timer to be reverted.

(defun mistty-test-find-p (str)
  "Returns non-nil if STR is found in the current buffer."
  (save-excursion
    (goto-char (point-min))
    (search-forward str nil 'noerror)))

(defun mistty-test-goto (str)
  "Search for STR, got to its beginning and return that position."
  (mistty-test-goto-after str)
  (goto-char (match-beginning 0)))

(defun mistty-test-pos (str)
  "Search for STR, returns its position."
  (save-excursion
    (mistty-test-goto-after str)
    (match-beginning 0)))

(defun mistty-test-pos-after (str)
  "Search for STR, returns its end position."
  (save-excursion
    (mistty-test-goto-after str)
    (match-end 0)))

(defun mistty-test-goto-after (str)
  "Search for STR, got to its end and return that position."
  (goto-char (point-min))
  (search-forward str))

(defun mistty-test-setup (shell)
  (cond
   ((eq shell 'bash)
    (mistty--exec mistty-test-bash-exe "--noprofile" "--norc" "-i")
    (mistty-test-set-ps1))
   
   ((eq shell 'zsh)
    (mistty--exec mistty-test-zsh-exe "-i" "--no-rcs")
    (mistty-test-set-ps1))
   
   ((eq shell 'fish)
    (mistty--exec
     mistty-test-fish-exe
     "-i" "-N" "-C"
     (concat "function fish_prompt; printf '$ '; end; "
             ;; This enables bracketed-paste; this is normally done in
             ;; __fish_config_interactive.fish but is disabled by -N.
             "function __fish_enable_bracketed_paste --on-event fish_prompt --on-event fish_read; "
             "  printf \"\\e[?2004h\";"
             "end;"
             "function __fish_disable_bracketed_paste --on-event fish_preexec --on-event fish_exit; "
             "  printf \"\\e[?2004l\";"
             "end; "
             ;; Older version of fish, such as 3.3.1, ignore termcap
             ;; entries. \eOA-\eOD is part of the default key binding,
             ;; bypassed here by -N, so it's only a problem in these
             ;; tests
             "bind \\eOA up-line; "
             "bind \\eOB down-line; "
             "bind \\eOC forward-char; "
             "bind \\eOD backward-char; "
             "bind \\ca beginning-of-line; "
             "bind \\ce end-of-line; "
             "bind \\b backward-delete-char; "))
    (mistty-send-and-wait-for-prompt (lambda ())))
   
   ((eq shell 'python)
    (mistty--exec mistty-test-python-exe)
    (mistty-wait-for-output :str ">>> ")
    (save-excursion 
      (mistty-test-goto ">>> ")
      (narrow-to-region (point) (point-max))))
   
   (t (error "Unsupported shell %s" shell))))

(defun mistty-test-set-ps1 ()
  (mistty-wait-for-output :test (lambda () (= (point-min) (point-max))))
  (mistty-send-text (concat "PS1='" mistty-test-prompt "'"))
  (narrow-to-region (mistty-send-and-wait-for-prompt) (point-max)))

(defun mistty-setup-fish-right-prompt ()
  (mistty-send-text "function fish_right_prompt; printf '< right'; end")
  (mistty-send-and-wait-for-prompt)
  (narrow-to-region (mistty--bol (point)) (point-max)))

(defun mistty-test-after-command ()
  (mistty--post-command)
  (ert-run-idle-timers)
  (mistty-wait-for-output
   :test (lambda ()
           (mistty--queue-empty-p mistty--queue))))

(cl-defun mistty-wait-for-output
    (&key (test nil) (str nil) (regexp nil) (start (point-min)) (proc mistty-proc) (on-error nil))
  "Wait for process output.

With TEST set to a function, keep waiting for process output
until the function evaluates to true.

With STR set to a string, keep waiting for process output until
the text can be found. If START is set, always go back to START
to search for the text instead of (point-min)

With REGEXP set to a string, keep waiting for process output
until the regexp matches. If START is set, always go back to
START to search for the text instead of (point-min)."
  (ert-run-idle-timers)
  (let ((condition nil)
        (condition-descr nil))
    (cond
     (regexp
      (setq condition
            (lambda ()
              (save-excursion
                (when start (goto-char start))
                (search-forward-regexp regexp nil 'noerror))))
      (setq condition-descr (format ":regexp \"%s\"" regexp)))
     (str
      (setq condition
            (lambda ()
              (save-excursion
                (when start (goto-char start))
                (search-forward str nil t))))
      (setq condition-descr (format ":str \"%s\"" str)))
     (test
      (setq condition test)
      (setq condition-descr (format ":test %s" test)))

     (t (error "mistty-wait-for-output: no condition specified")))
    
    (let ((time-limit (time-add (current-time) mistty-test-timeout)))
      (while (not (funcall condition))
        (unless (time-less-p (current-time) time-limit)
          (if on-error
              (funcall on-error)
            (error "condition not met after %ss (wait-for-output %s) buffer<<EOF\n%s\nEOF"
                   mistty-test-timeout condition-descr
                   (mistty-test-content :show (point)))))
        (if (process-live-p proc)
            (accept-process-output proc 0 100 t)
          (accept-process-output nil 0 100))
        (ert-run-idle-timers)))))
      

(defun mistty-send-and-capture-command-output (&optional send-command-func narrow nopointer prompt)
  "Send the current commanhd line with SEND-COMMAND-FUNC and return its output.

This function sends RET to the process, then waits for the next
prompt to appear. Once the prompt has appeared, it captures
everything between the two prompts, return it, and narrow the
buffer to a new region at the beginning of the new prompt."
  (let ((first-prompt-end (point))
        output-start next-prompt-start output)
    (setq next-prompt-start (mistty-send-and-wait-for-prompt send-command-func prompt))
    (setq output-start
          (save-excursion
            (goto-char first-prompt-end)
            ;; If BACKSPACE was used, there could be leftover spaces
            ;; at the end of the line when the tty overwrites intead
            ;; of deleting.
            (goto-char (line-end-position))
            (1+ (point))))
    (setq output (mistty-test-content :start output-start :end next-prompt-start
                                      :show (unless nopointer (point))
                                      :strip-last-prompt t))
    (when narrow
      (narrow-to-region next-prompt-start (point-max)))
    output))

(defun mistty-send-and-wait-for-prompt (&optional send-command-func prompt)
  "Send the current command line and wait for a prompt to appear.

Puts the point at the end of the prompt and return the position
of the beginning of the prompt."
  (let ((before-send (point-marker)))
    (funcall (or send-command-func #'mistty-send-command))
    (mistty-wait-for-output
     :regexp (concat "^" (regexp-quote (or prompt mistty-test-prompt)))
     :start before-send)
    (match-beginning 0)))

(cl-defun mistty-test-content (&key (start (point-min))
                                    (end (point-max))
                                    (show nil)
                                    (show-property '(nil nil))
                                    (strip-last-prompt nil))
  "Return buffer content, post-processed.

START and END specify the region to extract.

SHOW is a specific position to highlight with <> in the string,
often just the output of `point' or `mistty-cursor'.

STRIP-LAST-PROMPT, if t, removes the last, empty prompt from the
returned content.

SHOW-PROPERTY \=='(PROP VAL) puts section with text-property PROP
set to VAL within brackets.

Trailing newlines are always stripped out from the output."
  (interactive)
  (let* ((output (buffer-substring start end))
         (p (when show (- show start)))
         (length (- end start)))
    (when (and p (>= p 0) (<= p length))
      (setq output (concat (substring output 0 p) "<>" (substring output p))))
    (pcase-let ((`(,prop ,val) show-property))
      (when prop
        (setq output (mistty-show-property prop val output))))
    (set-text-properties 0 (length output) nil output)
    
    (when strip-last-prompt
      (setq output (replace-regexp-in-string "\\$ \\(<>\\)?\n?$" "" output)))
    (setq output (replace-regexp-in-string "[ \t\n]*$" "" output))
    output))

(defun mistty-wait-for-term-buffer-and-proc-to-die (buf proc)
  (should (not (null buf)))
  (should (not (null proc)))
  (mistty-wait-for-output
   :test
   (lambda ()
     (and
      (not (process-live-p proc))
      (not (buffer-live-p buf))))
   :on-error
   (lambda ()
     (cond ((process-live-p proc)
            (error "Process %s didn't die. Status: %s" proc (process-status proc)))
           ((buffer-live-p buf)
            (error "Buffer %s wasn't killed." buf))
           (t (error "Something else went wrong."))))))

(defun mistty-send-text (text &optional start)
  "Send TEXT and wait until it appears after START.

If START is not specified, the current cursor is used; expecting
text to be inserted there."
  (let ((start (or start (copy-marker (mistty-cursor)))))
    (mistty--send-string mistty-proc text)
    (mistty-wait-for-output
     :start start
     :regexp (mapconcat #'regexp-quote
                        (split-string text "[ \t\n\r]" 'omit-nulls)
                        "[ \t\n\r]+"))))

(defun mistty-show-property (prop val text)
  "Put section with property PROP set to VAL within []."
  (let ((idx 0)
        (len (length text))
        found output)
    (while (and
            (< idx len)
            (setq found (text-property-any idx len prop val text)))
      (push (substring text idx found) output)
      (push "[" output)
      (setq idx (next-single-property-change found prop text len))
      (push (substring text found idx) output)
      (push "]" output))
    (push (substring text idx len) output)
    (apply #'concat (nreverse output))))

(defun mistty-ert-explain-string-match (a b)
  `(string-match ,a ,b))
(put 'equal 'ert-explainer 'mistty-ert-explain-string-match)

(defun mistty-reload-all ()
  "Force a reload of all mistty .el files.

This is useful after files have changed, such as after checking
 out a new branch."
  (interactive)
  (load "mistty-util.el" nil 'nomessage 'nosuffix)
  (load "mistty-log.el" nil 'nomessage 'nosuffix)
  (load "mistty-changeset.el" nil 'nomessage 'nosuffix)
  (load "mistty-term.el" nil 'nomessage 'nosuffix)
  (load "mistty-queue.el" nil 'nomessage 'nosuffix)
  (load "mistty-osc7.el" nil 'nomessage 'nosuffix)
  (load "mistty.el"nil 'nomessage 'nosuffix))
