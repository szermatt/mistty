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
   (should (equal 'oterm-self-insert-command (key-binding (kbd "e"))))
   (execute-kbd-macro (kbd "e c h o SPC o k RET"))
   (oterm-wait-for-output)
   (should (equal "$ echo ok\nok\n" (oterm-test-content)))))

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

(defun oterm-test-content  (&optional start end nopointers)
  (interactive)
  (let* ((start (or start (point-min)))
         (end (or end (point-max)))
         (output (buffer-substring-no-properties start end))
         (p (- (point) start))
         (pmark (- (oterm--work-pmark) start))
         (length (- end start)))
    (unless nopointers 
      (setq output
            (cond 
             ((and (> p pmark) (>= p 0) (< p length) (>= pmark 0) (< pmark length))
              (concat (substring output 0 pmark) "<pmark>" (substring output pmark p) "<>" (substring output p)))
             ((and (< p pmark) (>= p 0) (< p length) (>= pmark 0) (< pmark length))
              (concat (substring output 0 p) "<>" (substring output p pmark) "<pmark>" (substring output pmark)))
             ((and (>= p 0) (<= p length))
              (concat (substring output 0 p) "<>" (substring output p)))
             ((and (>= pmark 0) (<= pmark length))
              (concat (substring output 0 p) "<pmark>" (substring output p)))
             (t output))))
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
