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
(defvar mistty-test-py-exe (or (executable-find "python3")
                               (executable-find "python")))
(defvar mistty-test-zsh-exe (executable-find "zsh")) ;; optional
(defvar mistty-test-fish-exe (executable-find "fish"));; optional

(defvar mistty-test-timeout (if noninteractive 10 1)
  "Time, in seconds, to wait for expected output in tests.

When running tests automatically, a larger value is useful to
avoid falkey tests in case the machine running the tests is slow.

When running tests manually, a smaller value is useful to avoid
waiting for failing test results.")

(defvar mistty-test-prompt "$ ")

(defmacro with-mistty-buffer (&rest body)
  `(progn
     (should mistty-test-bash-exe)
     (ert-with-test-buffer ()
     (mistty-test-setup 'bash)
     ,@body)))

(defmacro with-mistty-buffer-zsh (&rest body)
  `(progn
     (skip-unless mistty-test-zsh-exe)
     (ert-with-test-buffer ()
     (mistty-test-setup 'zsh)
     ,@body)))

(defmacro with-mistty-buffer-fish (&rest body)
  `(progn
     (skip-unless mistty-test-fish-exe)
     (ert-with-test-buffer ()
     (mistty-test-setup 'fish)
     ,@body)))

(defmacro with-mistty-buffer-python (&rest body)
  `(progn
     (should mistty-test-py-exe)
     (ert-with-test-buffer ()
     (mistty-test-setup 'python)
     ,@body)))

(defmacro with-mistty-buffer-selected (&rest body)
  `(save-window-excursion
     (with-mistty-buffer
      (with-selected-window (display-buffer (current-buffer))
        ,@body))))

(defmacro mistty-run-command (&rest body)
  `(progn
     (mistty--pre-command)
     (progn ,@body)
     (mistty-test-after-command)))

(ert-deftest test-mistty-simple-command ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello\n")
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-simple-command-zsh ()
  (with-mistty-buffer-zsh
   (mistty-send-raw-string "echo hello\n")
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-simple-command-fish ()
  (with-mistty-buffer-fish
   (mistty-send-raw-string "echo hello\n")
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-keystrokes ()
  (with-mistty-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC o k"))
   (should (equal "ok" (mistty-send-and-capture-command-output (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-mistty-keystrokes-backspace ()
  (with-mistty-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC f o o DEL DEL DEL o k"))
   (should (equal "ok" (mistty-send-and-capture-command-output (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-mistty-reconcile-insert ()
  (with-mistty-buffer
   (mistty-run-command
    (insert "echo hello"))
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-delete ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello")
   (mistty-wait-for-output)
   
   (mistty-run-command
      (mistty-test-goto "hello")
      (delete-region (point) (+ 3 (point))))
   
   (should (equal "$ echo <>lo" (mistty-test-content :show (point))))
   (should (equal "lo" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-delete-last-word ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello world")
   (mistty-wait-for-output)
   (mistty-run-command
    (save-excursion
      (mistty-test-goto " world")
      (delete-region (point) (point-max))))
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))
   (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-replace ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello")
   (mistty-wait-for-output)
   (mistty-run-command
    (goto-char (point-min))
    (search-forward "hello")
    (replace-match "bonjour" nil t))
   
   (should (equal "$ echo bonjour<>" (mistty-test-content :show (point))))
   (should (equal "bonjour" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-replace-with-point-outside-of-change ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello, hello")
   (mistty-wait-for-output)
   (mistty-run-command
    (goto-char (point-min))
    (while (search-forward "hello" nil t)
      (replace-match "bonjour" nil t))
    (mistty-test-goto "echo"))
   
   (should (equal "$ <>echo bonjour, bonjour" (mistty-test-content :show (point))))
   (should (equal "bonjour, bonjour" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-reconcile-multiple-replace ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo boo boo, white goat")
   (mistty-wait-for-output)
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
  (with-mistty-buffer
   (let (beg end)
     (mistty-send-raw-string "echo hello")
     (mistty-wait-for-output)
     (setq beg (- (point) 5))
     (setq end (point))
     (mistty-send-raw-string "\n")
     (mistty-wait-for-output)
     (mistty-send-raw-string "echo world")  
     (mistty-wait-for-output)
     (should (equal "$ echo hello\nhello\n$ echo world<>" (mistty-test-content :show (point))))
     (mistty-run-command
      (delete-region beg end)
      (goto-char beg)
      (insert "bonjour"))
     ;; the modification is available and the point is after the insertion
     (should (equal "$ echo bonjour<>\nhello\n$ echo world" (mistty-test-content :show (point))))
     
     ;; the next command executes normally and doesn't revert the
     ;; modification, though it moves the point.
     (mistty-send-command)
     (mistty-wait-for-output)
     (should (equal "$ echo bonjour\nhello\n$ echo world\nworld\n$ <>"
                    (mistty-test-content :show (point)))))))

(ert-deftest test-mistty-send-command-because-at-prompt ()
  (with-mistty-buffer-selected
   (mistty-send-raw-string "echo hello")
   (should (equal "hello" (mistty-send-and-capture-command-output
                           (lambda ()
                             (execute-kbd-macro (kbd "RET"))))))
   (should (equal "$ echo hello\nhello\n$" (mistty-test-content)))))

(ert-deftest test-mistty-send-newline-because-not-at-prompt ()
  (with-mistty-buffer-selected
   (mistty-send-raw-string "echo hello")
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (mistty-test-goto "hello"))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "$ echo\n<>hello\nhello\n$" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-send-newline-because-not-at-prompt-multiline ()
  (with-mistty-buffer-selected
   (mistty-run-command
    (insert "echo hello\necho world"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (mistty-test-goto "hello"))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "$ echo\n<>hello\necho world\nhello\nworld\n$" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-send-tab-to-complete  ()
  (with-mistty-buffer
   (mistty-send-raw-string "ech world")
   (mistty-wait-for-output)
   ;; Move the point before doing completion, to make sure that
   ;; mistty-send-if-at-prompt moves the cursor to the right position
   ;; before sending TAB.
   (mistty-run-command
    (goto-char (+ (point-min) 5)))
   (should (equal "$ ech<> world" (mistty-test-content :show (point))))
   (mistty-send-key 1 "\t")
   (mistty-wait-for-output)
   (should (equal "$ echo<> world" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-kill-term-buffer ()
  (let* ((buffer-and-proc (with-mistty-buffer
                           (cons mistty-term-buffer mistty-proc)))
         (term-buffer (car buffer-and-proc))
         (term-proc (cdr buffer-and-proc)))
    (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)))

(ert-deftest test-mistty-term-buffer-exits ()
  (with-mistty-buffer
   (mistty-send-raw-string "exit\n")
   (mistty-wait-for-term-buffer-and-proc-to-die mistty-term-buffer mistty-proc)
   (should (string-suffix-p "finished\n" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-mistty-scroll-with-long-command ()
  (with-mistty-buffer
   (let ((loop-command "for i in {0..49}; do echo line $i; done"))
     (mistty-send-raw-string loop-command)
     (mistty-wait-for-output)
     (should (equal (concat "$ " loop-command "<>") (mistty-test-content :show (point))))
     (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 49) "\n")
                    (mistty-send-and-capture-command-output))))))

(ert-deftest test-mistty-scroll-with-many-commands ()
  (with-mistty-buffer
   (let ((loop-command "for i in {0..4}; do echo line $i; done"))
     (dotimes (_ 10)
       (mistty-send-raw-string loop-command)
       (mistty-wait-for-output)
       (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 4) "\n")
                      (mistty-send-and-capture-command-output nil nil 'nopointer)))))))

(ert-deftest test-mistty-bracketed-paste ()
  (with-mistty-buffer
   (should (equal mistty-bracketed-paste t))
   (mistty-send-raw-string "read yesorno && echo answer: $yesorno\n")
   (mistty-wait-for-output)
   (should (equal mistty-bracketed-paste nil))
   (mistty-send-raw-string "no")
   (should (equal "answer: no" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-bol ()
  (with-mistty-buffer
   (mistty-run-command
    (insert "echo hello")
    
    ;; Move the point after the prompt.
    (beginning-of-line)
    (should (equal (point) (mistty-test-goto "echo")))
    
    ;; Move to the real line start.
    (let ((inhibit-field-text-motion t))
      (beginning-of-line))
    (should (equal (point) (mistty-test-goto "$ echo hello"))))))

(ert-deftest test-mistty-bol-multiline ()
  (with-mistty-buffer
   (mistty-run-command
    (insert "echo \"hello\nworld\""))
   
   ;; Point is in the 2nd line, after world, and there's no prompt
   ;; on that line, so just go there.
   (beginning-of-line)
   (should (equal (point) (mistty--bol (point))))))

(ert-deftest test-mistty-bol-outside-of-prompt ()
  (with-mistty-buffer
   (mistty-run-command
    (insert "echo one"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo two"))
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo three"))
   
   ;; (beginning-of-line) moves just after the prompt, even though
   ;; it's not the active prompt.
   (mistty-test-goto "two")
   (beginning-of-line)
   (should (equal
            (point) (save-excursion
                      (mistty-test-goto "echo two"))))))

(ert-deftest test-mistty-eol ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello")
   (mistty-wait-for-output)
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))

   (mistty-run-command
    (mistty-send-beginning-of-line))
   (mistty-wait-for-output)
   
   (should (equal "$ <>echo hello" (mistty-test-content  :show (point))))
   
   (mistty-run-command
    (mistty-send-end-of-line))
   (mistty-wait-for-output)
   
   (should (equal "$ echo hello<>" (mistty-test-content :show (point))))))

(ert-deftest test-mistty-eol-empty-prompt ()
  (with-mistty-buffer
   (goto-char (point-min))
   (mistty-run-command
    (mistty-send-end-of-line))
   
   (should
    (equal "$ <>"
           (mistty-test-content :show (point))))))

(ert-deftest test-mistty-next-prompt ()
  (with-mistty-buffer
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
    (equal "$ <>echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$ echo one\none\n$ <>echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$ echo one\none\n$ echo two\ntwo\n$ <>echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$ echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ <>echo current"
           (mistty-test-content :show (point))))
   
   (should-error (mistty-next-prompt 1))
   (should
    (equal "$ echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ <>echo current"
           (mistty-test-content :show (point))))
   
   (goto-char (point-min))
   (mistty-next-prompt 2)
   (should
    (equal "$ echo one\none\n$ <>echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 2)
   (should
    (equal "$ echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ <>echo current"
           (mistty-test-content :show (point))))))

(ert-deftest test-mistty-next-prompt-zsh ()
  (with-mistty-buffer-zsh
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
    (equal "$ <>echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$ echo one\none\n$ <>echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))))
   
(ert-deftest test-mistty-next-prompt-fish ()
  (with-mistty-buffer-fish
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
    (equal "$ <>echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$ echo one\none\n$ <>echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))))

(ert-deftest test-mistty-next-empty-prompt ()
  (with-mistty-buffer
   (mistty-send-and-wait-for-prompt)
   (mistty-send-and-wait-for-prompt)
   (mistty-send-and-wait-for-prompt)
   (mistty-run-command
    (insert "echo current"))
   
   (goto-char (point-min))
   (mistty-next-prompt 1)
   (should
    (equal "$ <>\n$\n$\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$\n$ <>\n$\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$\n$\n$ <>\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-next-prompt 1)
   (should
    (equal "$\n$\n$\n$ <>echo current"
           (mistty-test-content :show (point))))
   
   (should-error (mistty-next-prompt 1))
   
   (goto-char (point-min))
   (mistty-next-prompt 2)
   (should
    (equal "$\n$ <>\n$\n$ echo current"
           (mistty-test-content :show (point))))

   (mistty-next-prompt 2)
   (should
    (equal "$\n$\n$\n$ <>echo current"
           (mistty-test-content :show (point))))))

(ert-deftest test-mistty-next-python-prompt ()
  (with-mistty-buffer-python
   (mistty-send-raw-string "1 + 1")
   (mistty-send-and-wait-for-prompt nil ">>> ")
   (mistty-send-raw-string "2 + 2")
   (mistty-wait-for-output :str "2 + 2")

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
  (with-mistty-buffer
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
    (equal "$ echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ <>echo current"
           (mistty-test-content :show (point))))
   
   (mistty-previous-prompt 1)
   (should
    (equal "$ echo one\none\n$ echo two\ntwo\n$ <>echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (mistty-previous-prompt 2)
   (should
    (equal "$ <>echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))
   
   (should-error (mistty-previous-prompt 1))

   (should
    (equal "$ <>echo one\none\n$ echo two\ntwo\n$ echo three\nthree\n$ echo current"
           (mistty-test-content :show (point))))))

(ert-deftest test-mistty-dirtrack ()
  (with-mistty-buffer
   (mistty-send-raw-string "cd /\n")
   (mistty-send-and-wait-for-prompt)
   (should (equal "/" default-directory))
   (mistty-send-raw-string "cd ~\n")
   (mistty-send-and-wait-for-prompt)
   (should (equal (file-name-as-directory (getenv "HOME")) default-directory))))

(ert-deftest test-mistty-bash-backward-history-search ()
  (with-mistty-buffer-selected
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
   (mistty-send-raw-string "?\C-r")
   (mistty-wait-for-output)
   (should (equal "(reverse-i-search)`': ?<>" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "e c"))
   (mistty-wait-for-output)
   (should (equal "(reverse-i-search)`ec': <>echo third" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "o"))
   (mistty-wait-for-output)
   (should (equal "(reverse-i-search)`eco': echo s<>econd" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "DEL"))
   (mistty-wait-for-output)
   (should (equal "(reverse-i-search)`ec': echo s<>econd" (mistty-test-content :show (point))))
   (execute-kbd-macro (kbd "<left>"))
   (mistty-wait-for-output)
   (should (equal "second" (mistty-send-and-capture-command-output)))))

(ert-deftest test-mistty-distance-on-term ()
  (with-mistty-buffer-selected
   (mistty-send-raw-string "echo one two three four five six seven eight nine")
   (mistty-send-and-wait-for-prompt)

   (let ((two (mistty-test-goto "two"))
         (three (mistty-test-goto "three"))
         (four (mistty-test-goto "four")))
     (should (equal 4 (mistty--distance-on-term two three)))
     (should (equal 6 (mistty--distance-on-term three four)))
     (should (equal -4 (mistty--distance-on-term three two))))))

(ert-deftest test-mistty-distance-on-term-with-hard-newlines ()
  (with-mistty-buffer
   (mistty--set-process-window-size 20 20)

   (mistty-send-raw-string "echo one two three four five six seven eight nine")
   (mistty-wait-for-output)
   (mistty-send-and-wait-for-prompt)

   (should (equal (concat "$ echo one two three\n"
                          " four five six seven\n"
                          " eight nine\n"
                          "one two three four f\n"
                          "ive six seven eight\n"
                          "nine")
                  (mistty-test-content :strip-last-prompt t)))

   (let ((one (mistty-test-goto "one"))
         (six (mistty-test-goto "six"))
         (end (mistty-test-goto-after "nine\n")))
     (should (equal 24 (mistty--distance-on-term one six)))
     (should (equal -24 (mistty--distance-on-term six one)))
     (should (equal 45 (mistty--distance-on-term one end)))
     (should (equal -45 (mistty--distance-on-term end one))))))

(ert-deftest test-mistty-insert-long-prompt ()
  (with-mistty-buffer
   (mistty--set-process-window-size 20 20)

   (mistty-run-command
    (insert "echo one two three four five six seven eight nine"))
   (mistty-wait-for-output :str "nine")
   (should (equal "$ echo one two three\n four five six seven\n eight nine"
                  (mistty-test-content)))))

(ert-deftest test-mistty-keep-sync-marker-on-long-prompt ()
  (with-mistty-buffer
   (mistty--set-process-window-size 20 20)

   (mistty-run-command
    (insert "echo one two three four five six seven eight nine"))
   (mistty-wait-for-output :str "nine")   

   ;; make sure that the newlines didn't confuse the sync marker
   (should (equal (marker-position mistty-sync-marker) (point-min)))
   (should (equal (marker-position mistty--cmd-start-marker) (mistty-test-goto "echo one")))))

(ert-deftest test-mistty-keep-pointer-on-long-prompt ()
  (with-mistty-buffer
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
  (with-mistty-buffer-selected
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
  (with-mistty-buffer-selected
   (let ((work-buffer mistty-work-buffer)
         (proc mistty-proc))

     (mistty-send-raw-string
      (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off\n'"
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
     (should (eq mistty-work-buffer (window-buffer (selected-window)))))))

(ert-deftest test-mistty-enter-fullscreen-alternative-code ()
  (mistty-test-enter-fullscreen "[?47h" "[?47l"))

(ert-deftest test-mistty-enter-fullscreen-1047 ()
  (mistty-test-enter-fullscreen "[?1047h" "[?1047l"))

(ert-deftest test-mistty-enter-fullscreen-1049 ()
  (mistty-test-enter-fullscreen "[?1049h" "[?1049l"))

(ert-deftest test-mistty-live-buffer-p ()
  (with-mistty-buffer
   (should (mistty-live-buffer-p mistty-work-buffer))
   (should (not (mistty-live-buffer-p mistty-term-buffer))))
  (with-temp-buffer
    (should (not (mistty-live-buffer-p (current-buffer))))))

(ert-deftest test-mistty-fullscreen-live-buffer-p ()
  (with-mistty-buffer
   (mistty-send-raw-string
    (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off\n'"
            "[47h" "[47l"))
   (mistty-send-command)
   (mistty-wait-for-output
    :test
    (lambda ()
      (buffer-local-value 'mistty-fullscreen mistty-work-buffer)))

   (should (not (mistty-live-buffer-p mistty-work-buffer)))
   (should (mistty-live-buffer-p mistty-term-buffer))))


(ert-deftest test-mistty-kill-fullscreen-buffer-kills-scrollback ()
  (with-mistty-buffer-selected
    (let ((work-buffer mistty-work-buffer)
          (proc mistty-proc))
      (should (executable-find "vi"))
      (execute-kbd-macro (kbd "v i RET"))
      (mistty-wait-for-output :test (lambda () mistty-fullscreen))

      (kill-buffer mistty-term-buffer)
      (mistty-wait-for-term-buffer-and-proc-to-die work-buffer proc))))

(ert-deftest test-mistty-proc-dies-during-fullscreen ()
  (with-mistty-buffer-selected
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
  (with-mistty-buffer
   (let* ((osc-list)
          (mistty-osc-handlers
           `(("8" . ,(lambda (code text)
                       (push (cons code text) osc-list))))))
      (mistty-send-raw-string "printf '\\e]8;;http://www.example.com\\aSome OSC\\e]8;;\\a!\\n'")
      (should (equal "Some OSC!" (mistty-send-and-capture-command-output)))
      (should (equal '(("8" . ";http://www.example.com") ("8" . ";")) (nreverse osc-list))))))

(ert-deftest test-mistty-osc-standard-end ()
  (with-mistty-buffer
   (let* ((osc-list)
          (mistty-osc-handlers
           `(("8" . ,(lambda (code text)
                       (push (cons code text) osc-list))))))
      (mistty-send-raw-string "printf '\\e]8;;http://www.example.com\\e\\\\Some OSC\\e]8;;\\e\\\\!\\n'")
      (should (equal "Some OSC!" (mistty-send-and-capture-command-output)))
      (should (equal '(("8" . ";http://www.example.com") ("8" . ";")) (nreverse osc-list))))))

(ert-deftest test-mistty-osc-add-text-properties ()
  (with-mistty-buffer
   (let* ((start nil)
          (test-value nil)
          (mistty-osc-handlers
           `(("f" . ,(lambda (_ text)
                       (if (length> text 0)
                           (setq test-value text
                                 start (point))
                         (put-text-property start (point) 'mistty-test test-value)))))))
     (mistty-send-raw-string "printf 'abc \\e]f;foobar\\adef\\e]f;\\a ghi\\n'")
     (should (equal "abc def ghi" (mistty-send-and-capture-command-output)))
     (search-backward "def")
     (should (equal `((,(1- (point)) ,(+ 2 (point)) (mistty-test "foobar")))
                    (mistty-merge-intervals
                     (mistty-filter-intervals
                      (object-intervals (current-buffer))
                      '(mistty-test))))))))

(ert-deftest mistty-test-split-osc-sequence ()
  (with-mistty-buffer
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
  (with-mistty-buffer
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
  (with-mistty-buffer
   (mistty-send-raw-string "echo one")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-raw-string "printf '\\ec'")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-raw-string "echo two")
   (mistty-send-and-wait-for-prompt)
   (should (equal "$ echo one\none\n$ printf '\\ec'\n$ echo two\ntwo\n$"
                  (mistty-test-content)))))

(ert-deftest test-mistty-clear-screen ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo one")
   (mistty-send-and-wait-for-prompt)
   (mistty-send-raw-string "printf '\\e[2J'")
   ;; wait-for-output makes sure the printf is displayed before
   ;; sending it otherwise it might or might not be included into the
   ;; final output, depending on the command sequences are buffered.
   (mistty-wait-for-output)
   (mistty-send-and-wait-for-prompt)
   (mistty-send-raw-string "echo two")
   (mistty-send-and-wait-for-prompt)
   (should (equal "$ echo one\none\n$ printf '\\e[2J'\n$ echo two\ntwo"
                  (mistty-test-content :strip-last-prompt t)))))
   
(ert-deftest test-mistty-hide-cursor ()
  (with-mistty-buffer
   (mistty-send-raw-string "printf 'o\\e[?25lk\\n'")
   (should (equal "ok" (mistty-send-and-capture-command-output)))
   (should (eq nil cursor-type))
   (mistty-send-raw-string "printf 'o\\e[?25hk\\n'")
   (should (equal "ok" (mistty-send-and-capture-command-output)))
   (should (eq t cursor-type))))
   
(ert-deftest test-mistty-show-cursor-if-moved ()
  (with-mistty-buffer
   (mistty-send-raw-string "printf 'o\\e[?25lk\\n'")
   (should (equal "ok" (mistty-send-and-capture-command-output)))
   (should (eq nil cursor-type))
   (mistty-run-command (goto-char (1- (point))))
   (should (eq t cursor-type))))

(ert-deftest test-mistty-detect-possible-prompt ()
  (with-mistty-buffer
   (mistty-send-raw-string
    "printf 'say %s>> ' something; read something; echo something: $something")
   (mistty-send-command)
   (mistty-wait-for-output :str "say something")
   (mistty-send-raw-string "foo")
   (mistty-wait-for-output)
   (should (equal
            (list
             (mistty-test-goto "say something>> ")
             (mistty-test-goto-after "say something>> ")
             "say something>> ")
            mistty--possible-prompt))))

(ert-deftest test-mistty-python-just-type ()
  (with-mistty-buffer-python
   (mistty-send-raw-string "1 + 1")
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   ;; the prompt was identified and labelled
   (mistty-previous-prompt 1)
   (should (looking-at (regexp-quote "1 + 1")))))

(ert-deftest test-mistty-python-move-and-type ()
  (with-mistty-buffer-python
   (mistty-send-raw-string "10 * 10")
   (mistty-wait-for-output)
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
  (with-mistty-buffer
   (should mistty-test-py-exe)
   (mistty-send-raw-string mistty-test-py-exe)
   (mistty-send-and-wait-for-prompt nil ">>> ")
   (mistty-send-and-wait-for-prompt (lambda () (mistty-send-key 1 "\C-d")))))

(ert-deftest test-mistty-python-delchar ()
  (with-mistty-buffer-python
   (mistty-send-raw-string "11 + 1")
   (mistty-wait-for-output)
   (mistty-run-command
    (mistty-test-goto "11 + 1")
    (mistty-send-key 1 "\C-d"))
   ;; deleted the first 1, the command-line is now 1 + 1
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))))

(ert-deftest test-mistty-python-beginnig-of-line ()
  (with-mistty-buffer-python
   (mistty-send-raw-string "1 + 1")
   (mistty-wait-for-output)
   (mistty-send-beginning-of-line)
   (mistty-wait-for-output)
   (should (equal ">>> <>1 + 1"
                  (mistty-test-content
                   :start (mistty--bol (point)) :show (point))))))

(ert-deftest test-mistty-python-edit-prompt ()
  (with-mistty-buffer-python
   (let ((start (- (point) 4)))
     (mistty-run-command
      (insert "10 * 10"))
     
     (should (equal "100" (mistty-send-and-capture-command-output nil nil nil ">>> ")))
     
     ;; the prompt was identified and labelled
     (mistty-previous-prompt 1)
     (should (equal ">>> <>10 * 10\n100\n>>>"
                    (mistty-test-content :start start :show (point)))))))

(ert-deftest test-mistty-python-edit-before-prompt ()
  (with-mistty-buffer-python
   (mistty-send-raw-string "1 + 1")
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   (mistty-send-raw-string "3 + 3")
   (should (equal "6" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   (mistty-send-raw-string "5 + 5")
   (mistty-wait-for-output)

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
  (with-mistty-buffer-python
   (mistty-send-raw-string "1 + 1")
   (should (equal "2" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   (mistty-send-raw-string "3 + 3")
   (should (equal "6" (mistty-send-and-capture-command-output nil nil nil ">>> ")))

   (mistty-send-raw-string "5 + 5")
   (mistty-wait-for-output)

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
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello, world")
   (mistty-send-and-wait-for-prompt)
   (let ((before-send (point)) after-line-10)
     (mistty-send-raw-string "for i in {1..100} ; do echo \"line $i\"; done | more")
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
    (lambda () (mistty-send-raw-string "q")))))

(ert-deftest test-mistty-python-prompt-too-long ()
  (with-mistty-buffer-python
   (let ((line-start (mistty--bol (point))))
     (mistty-send-raw-string "if a > b:")
     (mistty-wait-for-output)
     (mistty-run-command
      (mistty-send-beginning-of-line))
     (mistty-send-raw-string "el")
     (mistty-wait-for-output)

     (should (equal ">>> <>elif a > b:"
                    (mistty-test-content :start line-start
                                         :show mistty--cmd-start-marker))))))

(ert-deftest test-mistty-and-hippie-completion ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo hello, hullo, hallo, hi")
   (mistty-send-and-wait-for-prompt)

   (let ((hippie-expand-try-functions-list '(try-expand-dabbrev))
         (start (point)))

     (mistty-send-raw-string "echo h")
     (mistty-wait-for-output)
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
  (with-mistty-buffer
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
  (with-mistty-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC C-u 3 a"))
   (should (equal "aaa" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-send-key-from-term-buffer ()
  (with-mistty-buffer
   (with-current-buffer mistty-term-buffer
     (mistty-send-key 1 "e")
     (mistty-send-key 1 "c")
     (mistty-send-key 1 "h")
     (mistty-send-key 1 "o")
     (mistty-send-key 1 " ")
     (mistty-send-key 1 "o")
     (mistty-send-key 1 "k"))
   (mistty-wait-for-output)
   (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-send-last-key-from-term-buffer ()
  (with-mistty-buffer
   (mistty-send-raw-string "echo ok nok")
   (mistty-wait-for-output)
   (with-current-buffer mistty-term-buffer
     (with-selected-window (display-buffer (current-buffer))
       (local-set-key (kbd "C-c C-w") #'mistty-send-last-key)
       (execute-kbd-macro (kbd "C-c C-w"))))
   (mistty-wait-for-output)
   (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-raw-string-from-term-buffer ()
  (with-mistty-buffer
   (with-current-buffer mistty-term-buffer
     (mistty-send-raw-string "echo ok"))

    (should (equal "ok" (mistty-send-and-capture-command-output)))))


(ert-deftest mistty-test-send-last-key ()
  (with-mistty-buffer-selected
   (local-set-key (kbd "C-c C-w") 'mistty-send-last-key)
   (mistty-send-raw-string "echo abc def")
   (execute-kbd-macro (kbd "C-c C-w"))
   (should (equal "abc" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-C-q ()
  (with-mistty-buffer-selected
   (mistty-send-raw-string "echo abc def")
   (execute-kbd-macro (kbd "C-q C-w"))
   (should (equal "abc" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-revert-modification-after-prompt ()
  (with-mistty-buffer-zsh
   (dotimes (i 3)
     (mistty-send-raw-string (format "function toto%d { echo %d; };" i i)))
   (mistty-send-and-wait-for-prompt)
   (narrow-to-region (mistty--bol (point)) (point-max))
   (mistty-send-raw-string "toto\t")
   (mistty-wait-for-output
    :test (lambda ()
            (search-forward-regexp "^toto" nil 'noerror)))
   (mistty-run-command
    (insert "foobar"))
   (should (equal "$ toto<>\ntoto0  toto1  toto2"
                  (mistty-test-content :show (point))))))

(ert-deftest mistty-queue-timeout ()
  (with-mistty-buffer-zsh
   (let* ((answers nil)
          (lambda (iter-lambda ()
                   ;; zsh doesn't answer anything when the left arrow is sent, but
                   ;; the cursor cannot go left, like here, at the beginning of a
                   ;; prompt.
                   (push (iter-yield mistty-left-str) answers)
                   ;; sending empty sequences is a no-op, not a timeout
                   (push (iter-yield nil) answers)
                   (push (iter-yield "") answers)
                   ;; this is actually displayed
                   (push (iter-yield "done") answers))))
     (mistty--enqueue mistty--queue (funcall lambda))
     (mistty-wait-for-output :test (lambda () (length= answers 4)))
     (should (equal '(timeout nil nil nil) (nreverse answers)))
     (should (equal "$ done" (mistty-test-content))))))

(ert-deftest mistty-reset-during-replay ()
  (with-mistty-buffer
   (setq mistty-log t)
   (mistty-send-raw-string "echo -n 'read> '; read l; printf 'will reset\\ecreset done\\n'")
   (mistty-wait-for-output)
   (mistty-send-and-wait-for-prompt nil "read> ")
   (let ((start (mistty--bol (point))))
     (mistty-send-raw-string "I say: hello")
     (mistty-wait-for-output :str "hello")
     (mistty-run-command
      (insert "foo\n")
      (mistty-test-goto "hello")
      (insert "bar"))
     (mistty-log "done")
     (mistty-wait-for-output :str "$ " :start start)
     
     (should (equal (concat "read> I say:\n"
                            "hellofoo\n"
                            "will resetreset done\n"
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
  (with-mistty-buffer
   (mistty-send-raw-string "for i in {1..10} ; do echo line $i; done && read l")
   (mistty-send-and-wait-for-prompt nil "line 10")
   (should
    (equal
     "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n"
     (mistty--safe-bufstring mistty-sync-marker (point-max))))))

(ert-deftest mistty-test-end-prompt-multiline ()
  (with-mistty-buffer
   (mistty-run-command
    (insert "for i in {1..10} ; do \necho line $i\n done && read l")
    (mistty-test-goto "echo line"))
   (mistty-send-and-wait-for-prompt nil "line 10")
   (should
    (equal
     "line 1\nline 2\nline 3\nline 4\nline 5\nline 6\nline 7\nline 8\nline 9\nline 10\n"
     (mistty--safe-bufstring mistty-sync-marker (point-max))))))

(ert-deftest mistty-test-fish-multiline-on-term ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello                 \n")
    (insert "        world                       \n")
    (insert "        and all that sort of things.\n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "hell"))
          (to (search-forward "sort ")))
      (should (equal
               (concat
                (mistty--repeat-string 5 mistty-left-str)
                (mistty--repeat-string 2 mistty-down-str)
                (mistty--repeat-string 13 mistty-right-str))
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-on-term-straightforward ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello\n")
    (insert "        world'\n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "echo"))
          (to (search-forward "world")))
      (should (equal
               (concat
                (mistty--repeat-string 1 mistty-down-str)
                (mistty--repeat-string 1 mistty-right-str))
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-on-term-back ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello\n")
    (insert "        world'\n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "world"))
          (to (progn
                (goto-char (point-min))
                (search-forward "echo"))))
      (should (equal
               (concat
                (mistty--repeat-string 1 mistty-left-str)
                (mistty--repeat-string 1 mistty-up-str))
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-on-term-bad-indent ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello\n")
    (insert "prompt> world'\n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "echo"))
          (to (search-forward "world")))
      (should (null
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-on-term-single-line ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello'\n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "echo"))
          (to (search-forward "hello")))
      (should (null
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-on-term-ignore-fake-nl ()
  (ert-with-test-buffer ()
    (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
      (insert "prompt> echo '" fake-nl "hello                 \n")
      (insert "        world " fake-nl "                      \n")
      (insert "        and al" fake-nl "l that sort of things.\n")
      (goto-char (point-min))
      (let ((prompt-start (search-forward "prompt> "))
            (prompt-length 8)
            (from (search-forward "hell"))
            (to (search-forward "sort ")))
        (should (equal
                 (concat
                  (mistty--repeat-string 5 mistty-left-str)
                  (mistty--repeat-string 2 mistty-down-str)
                  (mistty--repeat-string 13 mistty-right-str))
                 (mistty--fish-multiline-move-str-on-term
                  from to prompt-start prompt-length)))))))


(ert-deftest mistty-test-fish-multiline-on-term-through-empty-line ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello\n")
    (insert "                   \n")
    (insert "        world'     \n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "hello"))
          (to (search-forward "world")))
      (should (equal
               (concat
                (mistty--repeat-string 10 mistty-left-str)
                (mistty--repeat-string 2 mistty-down-str)
                (mistty--repeat-string 4 mistty-right-str))
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-on-term-from-ws-to-ws ()
  (ert-with-test-buffer ()
    (insert "prompt> echo 'hello   \n")
    (insert "        world'        \n")
    (goto-char (point-min))
    (let ((prompt-start (search-forward "prompt> "))
          (prompt-length 8)
          (from (search-forward "hello  "))
          (to (search-forward "world'  ")))
      (should (equal
               (concat
                (mistty--repeat-string 7 mistty-left-str)
                (mistty--repeat-string 1 mistty-down-str)
                (mistty--repeat-string 2 mistty-right-str))
               (mistty--fish-multiline-move-str-on-term
                from to prompt-start prompt-length))))))

(ert-deftest mistty-test-fish-multiline-for-real ()
  (with-mistty-buffer-fish
   (should mistty-bracketed-paste)
   (mistty-send-raw-string "echo 'hello\nworld\nand all that sort of things.'")
   (mistty-wait-for-output)

   (mistty-run-command
    (mistty-test-goto "sort"))
   (should (equal "$ echo 'hello\n  world\n  and all that <>sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))

   (mistty-run-command
    (mistty-test-goto "llo"))
   (should (equal "$ echo 'he<>llo\n  world\n  and all that sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))

   (mistty-run-command
    (mistty-test-goto "rld"))
   (should (equal "$ echo 'hello\n  wo<>rld\n  and all that sort of things.'"
                  (mistty-test-content :show (mistty-cursor))))))

(ert-deftest mistty-test-truncation ()
  (let ((mistty-buffer-maximum-size 20))
    (with-mistty-buffer
     (dotimes (_ 100)
       (mistty-send-raw-string "for i in {0..10}; do echo line $i; done")
       (mistty-send-and-wait-for-prompt))
     (ert-run-idle-timers)
     (should (<= (count-lines (point-min) (point-max)) 30)))))

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
             "end"))
    (setq mistty-log t)
    (mistty-send-and-wait-for-prompt (lambda ())))
   
   ((eq shell 'python)
    (setq-local mistty-test-prompt ">>> ")
    (mistty--exec mistty-test-py-exe)
    (mistty-wait-for-output :str ">>> ")
    (save-excursion 
      (mistty-test-goto ">>> ")
      (narrow-to-region (point) (point-max))))
   
   (t (error "Unsupported shell %s" shell))))

(defun mistty-test-set-ps1 ()
  (mistty-wait-for-output :test (lambda () (= (point-min) (point-max))))
  (mistty-send-raw-string (concat "PS1='" mistty-test-prompt "'"))
  (mistty-wait-for-output :str "PS1=")
  (narrow-to-region (mistty-send-and-wait-for-prompt) (point-max)))

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
START to search for the text instead of (point-min)

With all of these nil, wait for any process output. This could be
anything: it could be output reacting to some old event, it could
be incomplete output. This makes tests unstable."
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
      (setq condition-descr (format ":test %s" test))))
    
    (if condition
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
            (ert-run-idle-timers)))
      
      ;; waiting for any process output (makes test unstable)
      (unless (accept-process-output proc 0 500 t)
        (if on-error
            (funcall on-error)
          (error "no output (wait-for-output with no conditions)"))))))

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

(cl-defun mistty-test-content (&key (start (point-min)) (end (point-max)) show (strip-last-prompt nil))
  (interactive)
  (let* ((output (buffer-substring-no-properties start end))
         (p (when show (- show start)))
         (length (- end start)))
    (when (and p (>= p 0) (<= p length))
      (setq output (concat (substring output 0 p) "<>" (substring output p))))
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

(defun mistty-filter-plist (options allowed)
  "Filter a symbol and values list OPTIONS to online include ALLOWED symbols.

For example, filtering (:key value :other-key value) with allowed
list of (:key) will return (:key value)."
  (let ((filtered-list))
    (dolist (key allowed)
      (when (plist-member options key)
        (setq filtered-list
              (plist-put filtered-list key (plist-get options key)))))
    filtered-list))

(defun mistty-filter-intervals (intervals allowed)
  (delq nil (mapcar
             (lambda (c)
               (pcase c
                 (`(,beg ,end ,props)
                  (let ((filtered (mistty-filter-plist props allowed)))
                    (when filtered
                      `(,beg ,end ,filtered))))))
             intervals)))
  
(defun mistty-merge-intervals (intervals)
  (let ((c intervals))
    (while c
      (pcase c
        ((and `((,beg1 ,end1 ,props1) (,beg2 ,end2 ,props2) . ,tail)
              (guard (and (= end1 beg2)
                          (equal props1 props2))))
         (setcar c `(,beg1 ,end2 ,props1))
         (setcdr c tail))
        (_ (setq c (cdr c)))))
    intervals))
