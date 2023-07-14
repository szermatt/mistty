;;; Tests my-term.el -*- lexical-binding: t -*-

(require 'my-term)
(require 'ert)
(require 'ert-x)

(defmacro with-my-term-buffer (&rest body)
  `(ert-with-test-buffer ()
     (my-term--setup "bash" "--noprofile" "--norc")
     (let ((proc (get-buffer-process (current-buffer))))
       (while (eq (point-min) (point-max))
         (my-term-wait-for-output))
       (term-send-raw-string "PS1='$ '\n")
       (my-term-wait-for-output)
       (narrow-to-region (- (point) 2) (point-max))
       ,@body)))

(defmacro with-my-term-buffer-selected (&rest body)
  `(save-window-excursion
     (with-my-term-buffer
      (with-selected-window (display-buffer (current-buffer))
        ,@body))))
  
(defun my-term-wait-for-output ()
  (accept-process-output (get-buffer-process (current-buffer)) 0 100 t))

(defun my-term-test-content  ()
  (interactive)
  (let ((output (buffer-substring-no-properties (point-min) (point-max))))
    (setq output (replace-regexp-in-string "\\$ \n?$" "" output))
    (setq output (replace-regexp-in-string "[ \t]*$" "" output))
    output))

(ert-deftest test-my-term-simple-command ()
  (with-my-term-buffer
   (term-send-raw-string "echo hello\n")
   (my-term-wait-for-output)
   (should (equal "$ echo hello\nhello\n" (my-term-test-content)))))

(ert-deftest test-my-term-keystrokes ()
  (with-my-term-buffer-selected
   (should (equal 'my-term-self-insert-command (key-binding (kbd "e"))))
   (execute-kbd-macro (kbd "e c h o SPC o k RET"))
   (my-term-wait-for-output)
   (should (equal "$ echo ok\nok\n" (my-term-test-content)))))

(ert-deftest test-my-term-reconcile-position ()
  (with-my-term-buffer
   (term-send-raw-string "echo world")
   (my-term-wait-for-output)
   (run-hooks 'pre-command-hook)
   (goto-char (- (point) 5))
   (run-hooks 'post-command-hook)
   (term-send-raw-string "hello ")
   (my-term-wait-for-output)
   (should (equal "$ echo hello world" (my-term-test-content)))))
