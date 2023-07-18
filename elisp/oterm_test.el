;;; Tests oterm.el -*- lexical-binding: t -*-

(require 'oterm)
(require 'ert)
(require 'ert-x)

(defconst oterm-test-prompt "$ ")
(defmacro with-oterm-buffer (&rest body)
  `(ert-with-test-buffer ()
     (oterm--exec "/usr/local/bin/bash" "--noprofile" "--norc")
     (while (eq (point-min) (point-max))
       (accept-process-output oterm-term-proc 0 100 t))
     (oterm-send-raw-string (concat "PS1='" oterm-test-prompt "'\n"))
     (oterm-wait-for-output)
     (narrow-to-region (- (point) 2) (point-max))
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
   (should (equal "$ echo hello\nhello\n" (oterm-test-content)))))

(ert-deftest test-oterm-keystrokes ()
  (with-oterm-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC e r r DEL DEL DEL o k RET"))
   (oterm-wait-for-output)
   (should (equal "$ echo ok\nok\n" (oterm-test-content)))))

(ert-deftest test-oterm-reconcile-insert ()
  (with-oterm-buffer
   (insert "echo hello")
   (should (equal "$ echo hello<>" (oterm-test-content)))
   (should (equal "hello\n" (oterm-run-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-delete ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (delete-region (- (point) 5) (- (point) 2))
   (should (equal "$ echo lo<>\n" (oterm-test-content)))
   (should (equal "lo\n" (oterm-run-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-replace ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (goto-char (point-min))
   (replace-string "hello" "bonjour")
   (should (equal "$ echo bonjour<>" (oterm-test-content)))
   (should (equal "bonjour\n" (oterm-run-and-capture-command-output)))))

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
     (should (equal "$ echo bonjour\nhello\n$ echo world\nworld\n" (oterm-test-content))))))

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
     (should (equal (mapconcat (lambda (i) (format "line %d\n" i)) (number-sequence 0 49) "")
                    (oterm-run-and-capture-command-output))))))

(ert-deftest test-oterm-scroll-with-many-commands ()
  (with-oterm-buffer
   (let ((loop-command "for i in {0..4}; do echo line $i; done"))
     (dotimes (i 10)
       (oterm-send-raw-string loop-command)
       (oterm-wait-for-output)
       (should (equal (mapconcat (lambda (i) (format "line %d\n" i)) (number-sequence 0 4) "")
                      (oterm-run-and-capture-command-output)))))))

(defun oterm-wait-for-output ()
  "Wait for process output, which should be short and immediate."
  (unless (accept-process-output oterm-term-proc 0 500 t)
    (error "no output")))

(defun oterm-run-and-capture-command-output ()
  "Return everything between two prompts.

This function sends RET to the process, then waits for the next
prompt to appear. Once the prompt has appeared, it captures
everything between the two prompts, return it, and narrow the
buffer to a new region at the beginning of the new prompt."
  (let ((first-prompt-end (point))
        next-prompt-start output)
    (oterm-send-input)
    (while (not (save-excursion
                  (goto-char first-prompt-end)
                  (search-forward-regexp (concat "^" (regexp-quote oterm-test-prompt)) nil 'noerror)))
      (unless (accept-process-output oterm-term-proc 1 nil t)
        (error "no output")))
    (setq next-prompt-start (match-beginning 0))
    (setq output (oterm-test-content (1+ first-prompt-end) next-prompt-start))
    (narrow-to-region next-prompt-start (point-max))
    output))

(defun oterm-test-content  (&optional start end nopointer)
  (interactive)
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (output (buffer-substring-no-properties start end))
         (p (- (point) start))
         (pmark (- (oterm--pmark) start))
         (length (- end start)))
    (when (and (not nopointer) (>= p 0) (<= p length))
      (setq output (concat (substring output 0 p) "<>" (substring output p))))
    (setq output (replace-regexp-in-string "\\$ \\(<>\\)?\n?$" "" output))
    (setq output (replace-regexp-in-string "[ \t]*$" "" output))
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
