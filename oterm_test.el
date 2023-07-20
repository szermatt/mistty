;;; Tests oterm.el -*- lexical-binding: t -*-

(require 'oterm)
(require 'ert)
(require 'ert-x)

(defconst oterm-test-prompt "$ ")

(cl-defmacro with-oterm-buffer (&rest body)
  `(ert-with-test-buffer ()
     (oterm-test-setup 'bash)
     ,@body))

(cl-defmacro with-oterm-buffer-zsh (&rest body)
  `(ert-with-test-buffer ()
     (oterm-test-setup 'zsh)
     ,@body))

(defmacro with-oterm-buffer-selected (&rest body)
  `(save-window-excursion
     (with-oterm-buffer
      (with-selected-window (display-buffer (current-buffer))
        ,@body))))
  
(ert-deftest test-oterm-simple-command ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello\n")
   (oterm-wait-for-output)
   (should (equal "$ echo hello\nhello" (oterm-test-content)))))

(ert-deftest test-oterm-keystrokes ()
  (with-oterm-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC e r r DEL DEL DEL o k"))
   (should (equal "ok" (oterm-send-and-capture-command-output (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-oterm-reconcile-insert ()
  (with-oterm-buffer
   (insert "echo hello")
   (should (equal "$ echo hello<>" (oterm-test-content)))
   (should (equal "hello" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-delete ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (delete-region (- (point) 5) (- (point) 2))
   (should (equal "$ echo lo<>" (oterm-test-content)))
   (should (equal "lo" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-replace ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (goto-char (point-min))
   (replace-string "hello" "bonjour")
   (should (equal "$ echo bonjour<>" (oterm-test-content)))
   (should (equal "bonjour" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-recover-from-deleted-prompt ()
  (with-oterm-buffer
   (delete-region (point-min) (point-max))
   (insert "echo hello")
   (should (equal "echo hello<>" (oterm-test-content)))
   (should (equal "hello" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-change-before-prompt ()
  (with-oterm-buffer
   (let (beg end)
     (oterm-send-raw-string "echo hello")
     (oterm-wait-for-output)
     (setq beg (- (point) 5))
     (setq end (point))
     (oterm-send-raw-string "\n")
     (oterm-wait-for-output)
     (oterm-send-raw-string "echo world")  
     (oterm-wait-for-output)
     (should (equal "$ echo hello\nhello\n$ echo world<>" (oterm-test-content)))
     (delete-region beg end)
     (goto-char beg)
     (insert "bonjour")
     ;; the modification is available and the point is after the insertion
     (should (equal "$ echo bonjour<>\nhello\n$ echo world" (oterm-test-content)))
     
     ;; the next command executes normally and doesn't revert the
     ;; modification, though it moves the point.
     (oterm-send-raw-string "\n")
     (oterm-wait-for-output)
     (should (equal "$ echo bonjour\nhello\n$ echo world\nworld" (oterm-test-content))))))

(ert-deftest test-oterm-send-command-because-at-prompt ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (oterm-send-command-if-at-prompt)
   (oterm-wait-for-output)
   (should (equal "$ echo hello\nhello" (oterm-test-content)))))

(ert-deftest test-oterm-send-newline-because-not-at-prompt ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello\n")
   (oterm-wait-for-output)
   (goto-char (+ (point-min) 7))
   (oterm-send-command-if-at-prompt)
   (should (equal "$ echo\n<>hello\nhello" (oterm-test-content)))))

(ert-deftest test-oterm-send-tab-to-complete  ()
  (with-oterm-buffer
   (oterm-send-raw-string "ech world")
   (oterm-wait-for-output)
   ;; Move the point before doing completion, to make sure that
   ;; oterm-send-if-at-prompt moves the pmark to the right position
   ;; before sending TAB.
   (goto-char (+ (point-min) 5))
   (should (equal "$ ech<> world" (oterm-test-content)))
   (oterm-send-tab-if-at-prompt)
   (oterm-wait-for-output)
   (should (equal "$ echo<> world" (oterm-test-content)))))

(ert-deftest test-oterm-send-newline-because-not-at-prompt-multiline ()
  (with-oterm-buffer
   (insert "echo hello\necho world")
   (goto-char (point-min))
   ;; pmark is on the second line, at the end of "world" and point is
   ;; on the line above, on the prompt.
   ;; oterm-send-command-if-at-prompt needs to be able to figure out
   ;; that the line it's on is still part of the prompt.
   (oterm-send-command-if-at-prompt)
   (oterm-wait-for-output)
   (should (equal "$ echo hello\necho world\nhello\nworld" (oterm-test-content)))))

(ert-deftest test-oterm-kill-term-buffer ()
  (let* ((buffer-and-proc (with-oterm-buffer
                           (cons oterm-term-buffer oterm-term-proc)))
         (term-buffer (car buffer-and-proc))
         (term-proc (cdr buffer-and-proc)))
    (oterm-wait-for-term-buffer-and-proc-to-die term-buffer term-proc 2)))

(ert-deftest test-oterm-term-buffer-exits ()
  (with-oterm-buffer
   (oterm-send-raw-string "exit\n")
   (oterm-wait-for-term-buffer-and-proc-to-die oterm-term-buffer oterm-term-proc 2)
   (should (string-suffix-p "finished\n" (buffer-substring-no-properties (point-min) (point-max))))))

(ert-deftest test-oterm-scroll-with-long-command ()
  (with-oterm-buffer
   (let ((loop-command "for i in {0..49}; do echo line $i; done"))
     (oterm-send-raw-string loop-command)
     (oterm-wait-for-output)
     (should (equal (concat "$ " loop-command "<>") (oterm-test-content)))
     (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 49) "\n")
                    (oterm-send-and-capture-command-output))))))

(ert-deftest test-oterm-scroll-with-many-commands ()
  (with-oterm-buffer
   (let ((loop-command "for i in {0..4}; do echo line $i; done"))
     (dotimes (_ 10)
       (oterm-send-raw-string loop-command)
       (oterm-wait-for-output)
       (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 4) "\n")
                      (oterm-send-and-capture-command-output)))))))

(ert-deftest test-oterm-bracketed-paste ()
  (with-oterm-buffer
   (should (equal oterm-bracketed-paste t))
   (oterm-send-raw-string "read yesorno && echo answer: $yesorno\n")
   (oterm-wait-for-output)
   (should (equal oterm-bracketed-paste nil))
   (should (equal "answer: no" (oterm-send-and-capture-command-output
                                (lambda ()
                                  (insert "no\n")))))))

(defun oterm-test-setup (shell)
  (cond
   ((eq shell 'bash)
    (oterm--exec "/usr/local/bin/bash" "--noprofile" "--norc" "-i"))
   ((eq shell 'zsh)
    (oterm--exec "/usr/local/bin/zsh" "-i" "--no-rcs"))
   (t (error "Unsupported shell %s" shell)))
  (while (eq (point-min) (point-max))
    (accept-process-output oterm-term-proc 0 100 t))
  (oterm-send-raw-string (concat "PS1='" oterm-test-prompt "'"))
  (oterm-wait-for-output)
  (narrow-to-region (oterm-send-and-wait-for-prompt) (point-max)))

(defun oterm-wait-for-output ()
  "Wait for process output, which should be short and immediate."
  (unless (accept-process-output oterm-term-proc 0 500 t)
    (error "no output")))

(defun oterm-send-and-capture-command-output (&optional send-command-func narrow)
  "Send the current commanhd line with SEND-COMMAND-FUNC and return its output.

This function sends RET to the process, then waits for the next
prompt to appear. Once the prompt has appeared, it captures
everything between the two prompts, return it, and narrow the
buffer to a new region at the beginning of the new prompt."
  (let ((first-prompt-end (point))
        output-start next-prompt-start output)
    (setq next-prompt-start (oterm-send-and-wait-for-prompt send-command-func))
    (setq output-start
          (save-excursion
            (goto-char first-prompt-end)
            ;; If BACKSPACE was used, there could be leftover spaces
            ;; at the end of the line when the tty overwrites intead
            ;; of deleting.
            (goto-char (line-end-position))
            (1+ (point))))
    (setq output (oterm-test-content output-start next-prompt-start))
    (when narrow
      (narrow-to-region next-prompt-start (point-max)))
    output))

(defun oterm-send-and-wait-for-prompt (&optional send-command-func)
  "Send the current command line with SEND-COMMAND-FUNC and wait for a prompt to appear.

Puts the point at the end of the prompt and return the position
of the beginning of the prompt."
  (let ((before-send (point)))
    (funcall (or send-command-func #'oterm-send-command))
    (while (not (save-excursion
                  (goto-char before-send)
                  (search-forward-regexp (concat "^" (regexp-quote oterm-test-prompt)) nil 'noerror)))
      (unless (accept-process-output oterm-term-proc 1 nil t)
        (error "no output")))
    (match-beginning 0)))

(defun oterm-test-content  (&optional start end nopointer)
  (interactive)
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (output (buffer-substring-no-properties start end))
         (p (- (point) start))
         (length (- end start)))
    (when (and (not nopointer) (>= p 0) (<= p length))
      (setq output (concat (substring output 0 p) "<>" (substring output p))))
    (setq output (replace-regexp-in-string "\\$ \\(<>\\)?\n?$" "" output))
    (setq output (replace-regexp-in-string "[ \t\n]*$" "" output))
    output))

(defun oterm-wait-for-term-buffer-and-proc-to-die (buf proc deadline)
  (should (not (null buf)))
  (should (not (null proc)))
  (let ((tstart (current-time)))
    (while (or (process-live-p proc) (buffer-live-p buf))
      (accept-process-output proc 0 100)
      (when (> (float-time (time-subtract (current-time) tstart)) deadline)
        (cond ((process-live-p proc)
               (error "Process %s didn't die. Status: %s" proc (process-status proc)))
              ((buffer-live-p buf)
               (error "Buffer %s wasn't killed." buf))
              (t (error "Something else went wrong.")))))))
