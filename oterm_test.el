;;; Tests oterm.el -*- lexical-binding: t -*-

(require 'oterm)
(require 'ert)
(require 'ert-x)

(defconst oterm-test-prompt "$ ")

(defmacro with-oterm-buffer (&rest body)
  `(ert-with-test-buffer ()
     (oterm-test-setup 'bash)
     ,@body))

(defmacro with-oterm-buffer-zsh (&rest body)
  `(ert-with-test-buffer ()
     (oterm-test-setup 'zsh)
     ,@body))

(defmacro with-oterm-buffer-selected (&rest body)
  `(save-window-excursion
     (with-oterm-buffer
      (with-selected-window (display-buffer (current-buffer))
        ,@body))))

(defmacro oterm-run-command (&rest body)
  `(progn
     (oterm-pre-command)
     (progn ,@body)
     (let ((timer (oterm-post-command)))
       (while (memq timer timer-list)
         (timer-event-handler timer)))))

(ert-deftest test-oterm-simple-command ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello\n")
   (should (equal "hello" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-simple-command-zsh ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello\n")
   (should (equal "hello" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-keystrokes ()
  (with-oterm-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC o k"))
   (should (equal "ok" (oterm-send-and-capture-command-output (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-oterm-keystrokes-backspace ()
  (with-oterm-buffer-selected
   (execute-kbd-macro (kbd "e c h o SPC f o o DEL DEL DEL o k"))
   (should (equal "ok" (oterm-send-and-capture-command-output (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest test-oterm-reconcile-insert ()
  (with-oterm-buffer
   (oterm-run-command
    (insert "echo hello"))
   (should (equal "$ echo hello<>" (oterm-test-content)))
   (should (equal "hello" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-delete ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (oterm-run-command
    (save-excursion
      (oterm-test-goto "hello")
      (delete-region (point) (+ 3 (point)))))
   (should (equal "$ echo lo<>" (oterm-test-content)))
   (should (equal "lo" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-delete-last-word ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello world")
   (oterm-wait-for-output)
   (oterm-run-command
    (save-excursion
      (oterm-test-goto " world")
      (delete-region (point) (point-max))))
   (should (equal "$ echo hello<>" (oterm-test-content)))
   (should (equal "hello" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-reconcile-replace ()
  (with-oterm-buffer
   (oterm-send-raw-string "echo hello")
   (oterm-wait-for-output)
   (oterm-run-command
    (goto-char (point-min))
    (replace-string "hello" "bonjour"))
   (should (equal "$ echo bonjour<>" (oterm-test-content)))
   (should (equal "bonjour" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-prevent-deleting-prompt ()
  (with-oterm-buffer
   (should-error (backward-delete-char))
   (should-error (delete-region (point-min) (point-max)))))

(ert-deftest test-oterm-prevent-deleting-prompt-zsh ()
  (with-oterm-buffer-zsh
   (should-error (backward-delete-char))
   (should-error (delete-region (point-min) (point-max)))))

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
     (oterm-run-command
      (delete-region beg end)
      (goto-char beg)
      (insert "bonjour"))
     ;; the modification is available and the point is after the insertion
     (should (equal "$ echo bonjour<>\nhello\n$ echo world" (oterm-test-content)))
     
     ;; the next command executes normally and doesn't revert the
     ;; modification, though it moves the point.
     (oterm-send-command)
     (oterm-wait-for-output)
     (should (equal "$ echo bonjour\nhello\n$ echo world\nworld" (oterm-test-content))))))

(ert-deftest test-oterm-send-command-because-at-prompt ()
  (with-oterm-buffer-selected
   (oterm-send-raw-string "echo hello")
   (should (equal "hello" (oterm-send-and-capture-command-output
                           (lambda ()
                             (execute-kbd-macro (kbd "RET"))))))
   (should (equal "$ echo hello\nhello" (oterm-test-content)))))

(ert-deftest test-oterm-send-newline-because-not-at-prompt ()
  (with-oterm-buffer-selected
   (oterm-send-raw-string "echo hello")
   (oterm-send-and-wait-for-prompt)
   (oterm-run-command
    (oterm-test-goto "hello"))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "$ echo\n<>hello\nhello" (oterm-test-content)))))

(ert-deftest test-oterm-send-newline-because-not-at-prompt-multiline ()
  (with-oterm-buffer-selected
   (oterm-run-command
    (insert "echo hello\necho world"))
   (oterm-send-and-wait-for-prompt)
   (oterm-run-command
    (oterm-test-goto "hello"))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "$ echo\n<>hello\necho world\nhello\nworld" (oterm-test-content)))))

(ert-deftest test-oterm-send-tab-to-complete  ()
  (with-oterm-buffer
   (oterm-send-raw-string "ech world")
   (oterm-wait-for-output)
   ;; Move the point before doing completion, to make sure that
   ;; oterm-send-if-at-prompt moves the pmark to the right position
   ;; before sending TAB.
   (oterm-run-command
    (goto-char (+ (point-min) 5)))
   (oterm-wait-for-output)
   (should (equal "$ ech<> world" (oterm-test-content)))
   (oterm-send-tab)
   (oterm-wait-for-output)
   (should (equal "$ echo<> world" (oterm-test-content)))))

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
   (oterm-run-command (insert "no"))
   (should (equal "answer: no" (oterm-send-and-capture-command-output)))))

(ert-deftest test-oterm-bol ()
  (with-oterm-buffer
   (let ((initial-pos (point)))
     (oterm-run-command
      (insert "echo hello"))

     ;; The first time, move the point after the prompt.
     (beginning-of-line)
     (should (equal (point) initial-pos))

     ;; The first time, move to the real line start.
     (let ((inhibit-field-text-motion t))
       (beginning-of-line))
     (should (equal (point) (point-min))))))

(ert-deftest test-oterm-bol-multiline ()
  (with-oterm-buffer
   (oterm-run-command
    (insert "echo \"hello\nworld\""))
   
   ;; Point is in the 2nd line, after world, and there's no prompt
   ;; on that line, so just go there.
   (beginning-of-line)
   (should (equal (point) (oterm--bol-pos-from (point))))))

(ert-deftest test-oterm-bol-outside-of-prompt ()
  (with-oterm-buffer
   (let (prompt-start)
     (oterm-run-command
      (insert "echo one"))
     (oterm-send-and-wait-for-prompt)
     (setq prompt-start (point))
     (oterm-run-command
      (insert "echo two"))
     (oterm-send-and-wait-for-prompt)
     (oterm-run-command
      (insert "echo three"))

     ;; This is before the prompt; just go to the real line beginning,
     ;; even though there's an old prompt on that line.
     (goto-char (+ 3 prompt-start))
     (beginning-of-line)
     (should (equal (point) prompt-start)))))

(ert-deftest test-oterm-next-prompt ()
  (with-oterm-buffer
   (let (one two three current)
     (setq one (point))
     (oterm-run-command
      (insert "echo one"))
     (oterm-send-and-wait-for-prompt)
     (setq two (point))
     (oterm-run-command
      (insert "echo two"))
     (oterm-send-and-wait-for-prompt)
     (setq three (point))
     (oterm-run-command
      (insert "echo three"))
     (oterm-send-and-wait-for-prompt)
     (setq current (point))
     (oterm-run-command
      (insert "echo current"))

     (goto-char (point-min))
     (oterm-next-prompt 1)
     (should (equal one (point)))

     (oterm-next-prompt 1)
     (should (equal two (point)))

     (oterm-next-prompt 1)
     (should (equal three (point)))

     (oterm-next-prompt 1)
     (should (equal current (point)))

     (should-error (oterm-next-prompt 1))

     (goto-char (point-min))
     (oterm-next-prompt 2)
     (should (equal two (point)))
     
     (oterm-next-prompt 2)
     (should (equal current (point))))))

(ert-deftest test-oterm-previous-prompt ()
  (with-oterm-buffer
   (let (one three current)
     (setq one (point))
     (oterm-run-command
      (insert "echo one"))
     (oterm-send-and-wait-for-prompt)
     (oterm-run-command
      (insert "echo two"))
     (oterm-send-and-wait-for-prompt)
     (setq three (point))
     (oterm-run-command
      (insert "echo three"))
     (oterm-send-and-wait-for-prompt)
     (setq current (point))
     (oterm-run-command
      (insert "echo current"))

     (oterm-previous-prompt 1)
     (should (equal current (point)))
     
     (oterm-previous-prompt 1)
     (should (equal three (point)))

     (oterm-previous-prompt 2)
     (should (equal one (point)))

     (should-error (oterm-previous-prompt 1)))))

(ert-deftest test-oterm-dirtrack ()
  (with-oterm-buffer
   (oterm-send-raw-string "cd /\n")
   (oterm-send-and-wait-for-prompt)
   (should (equal "/" default-directory))
   (oterm-send-raw-string "cd ~\n")
   (oterm-send-and-wait-for-prompt)
   (should (equal (file-name-as-directory (getenv "HOME")) default-directory))))

(ert-deftest test-oterm-bash-backward-history-search ()
  (with-oterm-buffer-selected
   (oterm-run-command
    (insert "echo first"))
   (oterm-send-and-wait-for-prompt)
   (oterm-run-command
    (insert "echo second"))
   (oterm-send-and-wait-for-prompt)
   (oterm-run-command
    (insert "echo third"))
   (oterm-send-and-wait-for-prompt)
   (narrow-to-region (oterm--bol-pos-from (point)) (point-max))
   (oterm-send-raw-string "?\C-r")
   (oterm-wait-for-output)
   (should (equal "(reverse-i-search)`': ?<>" (oterm-test-content)))
   (execute-kbd-macro (kbd "e c"))
   (oterm-wait-for-output)
   (should (equal "(reverse-i-search)`ec': <>echo third" (oterm-test-content)))
   (execute-kbd-macro (kbd "o"))
   (oterm-wait-for-output)
   (should (equal "(reverse-i-search)`eco': echo s<>econd" (oterm-test-content)))
   (execute-kbd-macro (kbd "DEL"))
   (oterm-wait-for-output)
   (should (equal "(reverse-i-search)`ec': echo s<>econd" (oterm-test-content)))
   (execute-kbd-macro (kbd "RET"))
   (should (equal "second" (oterm-send-and-capture-command-output)))))

(ert-deftest test-term-buffer-vertical-motion ()
  ;; term-buffer-vertical-motion defined in term.el seems to behave
  ;; differently from buffer-vertical-motion in this specific case.
  ;; This causes issues with oterm and large prompts. Let's make sure
  ;; this is fixed.
  ;;
  ;; proposed fix:
  ;; - (todo (+ count (/ (current-column) term-width)))
  ;; + (todo count)
  (ert-with-test-buffer ()
    (setq term-width 5)
    (insert "hello\nworld\n")
    (goto-char (1- (point)))
    (should (equal 1 (term-buffer-vertical-motion 1)))
    (should (equal (point-max) (point)))))

(ert-deftest test-oterm-distance-on-term ()
  (with-oterm-buffer-selected
   (oterm-send-raw-string "echo one two three four five six seven eight nine")
   (oterm-send-and-wait-for-prompt)

   (let ((two (oterm-test-goto "two"))
         (three (oterm-test-goto "three"))
         (four (oterm-test-goto "four")))
     (should (equal 4 (oterm--distance-on-term two three)))
     (should (equal 6 (oterm--distance-on-term three four)))
     (should (equal -4 (oterm--distance-on-term three two))))))

(ert-deftest test-oterm-distance-on-term-with-hard-newlines ()
  (with-oterm-buffer-selected
   (oterm--set-process-window-width 20)

   (oterm-send-raw-string "echo one two three four five six seven eight nine")
   (oterm-send-and-wait-for-prompt)

   (should (equal (concat "$ echo one two three\n"
                          " four five six seven\n"
                          " eight nine\n"
                          "one two three four f\n"
                          "ive six seven eight\n"
                          "nine")
                  (oterm-test-content)))

   (let ((one (oterm-test-goto "one"))
         (six (oterm-test-goto "six"))
         (end (oterm-test-goto-after "nine\n")))
     (should (equal 24 (oterm--distance-on-term one six)))
     (should (equal -24 (oterm--distance-on-term six one)))
     (should (equal 45 (oterm--distance-on-term one end)))
     (should (equal -45 (oterm--distance-on-term end one))))))

(ert-deftest test-oterm-insert-long-prompt ()
  (with-oterm-buffer-selected
   (oterm--set-process-window-width 20)

   (oterm-run-command
    (insert "echo one two three four five six seven eight nine"))
   (while (length= (oterm-test-content) 0)
     (accept-process-output oterm-term-proc 0 500 t))
   (should (equal "$ echo one two three\n four five six seven\n eight nine<>"
                  (oterm-test-content)))))

(ert-deftest test-oterm-keep-sync-marker-on-long-prompt ()
  (with-oterm-buffer-selected
   (oterm--set-process-window-width 20)

   (oterm-run-command
    (insert "echo one two three four five six seven eight nine"))
   (while (length= (oterm-test-content) 0)
     (accept-process-output oterm-term-proc 0 500 t))

   ;; make sure that the newlines didn't confuse the sync marker
   (should (equal (marker-position oterm-sync-marker) (point-min)))
   (should (equal (marker-position oterm-cmd-start-marker) (oterm-test-goto "echo one")))))

(ert-deftest test-oterm-keep-track-pointer-on-long-prompt ()
  (with-oterm-buffer-selected
   (oterm--set-process-window-width 20)

   (oterm-run-command
    (insert "echo one two three four five six seven eight nine"))
   (while (length= (oterm-test-content) 0)
     (accept-process-output oterm-term-proc 0 500 t))

   ;; make sure that the newlines don't confuse oterm-post-command
   ;; moving the cursor.
   (dolist (count '("three" "nine" "four"))
     (let ((goal-pos))
       (oterm-pre-command)
       (setq goal-pos (oterm-test-goto count))
       (oterm-post-command)
       (oterm-wait-for-output)
       (should (equal (oterm-pmark) goal-pos))))))

(defun oterm-test-goto (str)
  "Search for STR, got to its beginning and return that position."
  (oterm-test-goto-after str)
  (goto-char (match-beginning 0)))

(defun oterm-test-goto-after (str)
  "Search for STR, got to its end and return that position."
  (goto-char (point-min))
  (search-forward str))

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

(defun oterm-filter-plist (options allowed)
  "Filter a symbol and values list OPTIONS to online include ALLOWED symbols.

For example, filtering (:key value :other-key value) with allowed
list of (:key) will return (:key value)."
  (let ((filtered-list))
    (dolist (key allowed)
      (when (plist-member options key)
        (setq filtered-list
              (plist-put filtered-list key (plist-get options key)))))
    filtered-list))

(defun oterm-filter-intervals (intervals allowed)
  (delq nil (mapcar
             (lambda (c)
               (pcase c
                 (`(,beg ,end ,props)
                  (let ((filtered (oterm-filter-plist props allowed)))
                    (when filtered
                      `(,beg ,end ,filtered))))))
             intervals)))
  
(defun oterm-merge-intervals (intervals)
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
