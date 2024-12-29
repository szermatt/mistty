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

(require 'ert)
(require 'ert-x)
(require 'term)
(defvar term-height) ;; term.el
(defvar term-home-marker) ;; term.el
(require 'turtles)

(eval-when-compile
  (require 'cl-lib))

(require 'mistty)
(require 'mistty-testing)
(require 'mistty-changeset)
(require 'mistty-log)
(require 'mistty-queue)


(ert-deftest mistty-test-simple-command ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-simple-command-zsh ()
  (mistty-with-test-buffer (:shell zsh)
    (mistty-send-text "echo hello")
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-simple-command-fish ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-send-text "echo hello")
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-keystrokes ()
  (mistty-with-test-buffer (:selected t)
    (execute-kbd-macro (kbd "e c h o SPC o k"))
    (should (equal "ok" (mistty-send-and-capture-command-output
                         (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest mistty-test-keystrokes-backspace ()
  (mistty-with-test-buffer (:selected t)
    (execute-kbd-macro (kbd "e c h o SPC f o o DEL DEL DEL o k"))
    (should (equal "ok" (mistty-send-and-capture-command-output
                         (lambda () (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest mistty-test-reconcile-insert ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo hello"))
    (should (equal "$ echo hello<>" (mistty-test-content :show (point))))
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-delete ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")

    (mistty-run-command
     (mistty-test-goto "hello")
     (delete-region (point) (+ 3 (point))))

    (should (equal "$ echo <>lo" (mistty-test-content :show (point))))
    (should (equal "lo" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-delete-last-word ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello world")
    (mistty-run-command
     (save-excursion
       (mistty-test-goto " world")
       (delete-region (point) (point-max))))
    (should (equal "$ echo hello<>" (mistty-test-content :show (point))))
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-delete-on-long-prompt ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 20 20)

    (mistty-run-command
     (insert "echo one two three four five six seven eight nine"))
    (mistty-wait-for-output :str "nine" :cursor-at-end t)

    (mistty-run-command
     (mistty-test-goto-after "nine"))
    (mistty-run-command
     (backward-kill-word 1))
    (should (equal (concat "$ echo one two three\n"
                           " four five six seven\n"
                           " eight <>")
                   (mistty-test-content :show (point))))

    (mistty-run-command
     (backward-kill-word 1))
    (should (equal (concat "$ echo one two three\n"
                           " four five six seven\n"
                           " <>")
                   (mistty-test-content :show (point))))))

(ert-deftest mistty-test-reconcile-delete-nobracketed-paste ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-send-text "echo")

    (mistty-run-command
     (delete-region (mistty-test-pos "echo")
                    (mistty-test-pos-after "echo")))

    (should (equal "$ <>" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-reconcile-large-multiline-delete ()
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
                           "      echo foo $i\n"
                           "  end")
                   (mistty-test-content)))))

(ert-deftest mistty-test-reconcile-replace ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (mistty-run-command
     (goto-char (point-min))
     (search-forward "hello")
     (replace-match "bonjour" nil t))

    (should (equal "$ echo bonjour<>" (mistty-test-content :show (point))))
    (should (equal "bonjour" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-replace-with-point-outside-of-change ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello, hello")
    (mistty-run-command
     (goto-char (point-min))
     (while (search-forward "hello" nil t)
       (replace-match "bonjour" nil t))
     (mistty-test-goto "echo"))

    (should (equal "$ <>echo bonjour, bonjour" (mistty-test-content :show (point))))
    (should (equal "bonjour, bonjour" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-replace-with-point-after-change ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello, hello world")
    (mistty-run-command
     (goto-char (point-min))
     (while (search-forward "hello" nil t)
       (replace-match "bonjour" nil t))
     (mistty-test-goto "world"))

    (should (equal "$ echo bonjour, bonjour <>world"
                   (mistty-test-content :show (point))))))

(ert-deftest mistty-test-reconcile-multiple-replace ()
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

(ert-deftest mistty-test-reconcile-multiple-replace-keep-pointer ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo boo boo, black sheep")

    (mistty-run-command
     (mistty-test-goto "echo"))
    (should (equal (point) (mistty-cursor)))

    (mistty-run-command
     (goto-char (point-min))
     (while (search-forward "boo" nil t)
       (replace-match "baa" nil t))
     (mistty-test-goto "echo"))
    ;; At the end of this command, replay runs. The point is at cursor
    ;; at the beginning, but it doesn't mean that it the point must be
    ;; moved to the cursor at the end.

    (should (equal "$ <>echo baa baa, black sheep"
                   (mistty-test-content :show (point))))))

(ert-deftest mistty-test-reconcile-with-autosuggestions ()
  (mistty-with-test-buffer (:shell fish)
    ;; seed autosuggestions
    (mistty-send-text "echo hello, world")
    (mistty-send-and-wait-for-prompt)
    (let ((start (mistty--bol (point))))
      ;; The suggestions could confuse the algorithm that detects what
      ;; was inserted, causing timeout. This is the worst case: insertions
      ;; look just like cursor movements.
      (mistty-run-command
       (insert "echo h"))
      (mistty-wait-for-output :str "world" :start start)
      (should (equal "$ echo h<>ello, world"
                     (mistty-test-content :show (point) :start start)))
      (mistty-run-command
       (insert "el"))
      (should (equal "$ echo hel<>lo, world"
                     (mistty-test-content :show (point) :start start)))
      (mistty-run-command
       (insert "lo"))
      (should (equal "$ echo hello<>, world"
                     (mistty-test-content :show (point) :start start))))))

(ert-deftest mistty-test-reconcile-quick-single-char-changes ()
  (mistty-with-test-buffer ()
    ;; nowait lets mistty join the replays together.
    (mistty-run-command-nowait
     (goto-char (mistty-cursor))
     (self-insert-command 1 ?e))
    (mistty-run-command-nowait
     (self-insert-command 1 ?c))
    (mistty-run-command-nowait
     (self-insert-command 1 ?h))
    (mistty-run-command-nowait
     (self-insert-command 1 ?o))
    (mistty-run-command-nowait
     (self-insert-command 1 ?\ ))
    (mistty-run-command-nowait
     (self-insert-command 1 ?o))
    (mistty-run-command
     (self-insert-command 1 ?k))
    (should (equal "$ echo ok<>" (mistty-test-content :show (point))))
    (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-quick-single-insert-changes ()
  (mistty-with-test-buffer ()
    ;; nowait lets mistty join the replays together.
    (mistty-run-command-nowait
     (goto-char (mistty-cursor))
     (insert "echo"))
    (mistty-run-command-nowait
     (insert " baa"))
    (mistty-run-command-nowait
     (insert " baa"))
    (mistty-run-command-nowait
     (insert " black"))
    (mistty-run-command
     (insert " sheep"))
    (should (equal "$ echo baa baa black sheep<>" (mistty-test-content :show (point))))
    (should (equal "baa baa black sheep" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-reconcile-quick-bad-single-insert-changes ()
  (mistty-with-test-buffer ()
    ;; nowait lets mistty join and queue the replays.
    (mistty-run-command-nowait
     (goto-char (mistty-cursor))
     (insert "echo "))
    (mistty-run-command-nowait
     (insert "baa "))
    ;; This replay is made in such a way that it cannot be appended on
    ;; purpose, to test that this case is handled properly.
    (mistty-run-command-nowait
     (delete-region (- (point) 3) (point))
     (insert "baa "))
    (mistty-run-command-nowait
     (insert "black "))
    (mistty-run-command
     (insert "sheep"))
    (should (equal "$ echo bbaa black sheep<>" (mistty-test-content :show (point))))
    (should (equal "bbaa black sheep" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-change-before-prompt ()
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

(ert-deftest mistty-test-send-command-because-at-prompt ()
  (mistty-with-test-buffer (:selected t)
    (mistty-send-text "echo hello")
    (should (equal "hello" (mistty-send-and-capture-command-output
                            (lambda ()
                              (execute-kbd-macro (kbd "RET"))))))
    (should (equal "$ echo hello\nhello\n$" (mistty-test-content)))))

(ert-deftest mistty-test-send-command-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-simulate-scrollback-buffer
     (should-error (call-interactively 'mistty-send-command)))))

(ert-deftest mistty-test-send-command-is-queued ()
  (mistty-with-test-buffer ()
    (mistty--enqueue mistty--queue (mistty--stuck-interaction "echo ok"))
    (mistty-send-and-wait-for-prompt
     (lambda ()
       (mistty-send-command)
       ;; send-command is added to the queue. It must not execute, not
       ;; even partially, until 'echo ok' has appeared.
       (should-not mistty-goto-cursor-next-time)
       (should-not mistty--end-prompt)
       (should (equal "$ <>" (mistty-test-content :show (point))))
       (mistty--send-string mistty-proc "echo ok")))
    (should (equal "$ echo ok\nok\n$ <>" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-send-newline-because-not-at-prompt ()
  (mistty-with-test-buffer (:selected t)
    (mistty-send-text "echo hello")
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (mistty-test-goto "hello"))
    (execute-kbd-macro (kbd "RET"))
    (should (equal "$ echo\n<>hello\nhello\n$" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-send-newline-because-not-at-prompt-multiline ()
  (mistty-with-test-buffer (:selected t)
    (mistty-run-command
     (insert "echo hello\necho world"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (mistty-test-goto "hello"))
    (execute-kbd-macro (kbd "RET"))
    (should (equal "$ echo\n<>hello\necho world\nhello\nworld\n$" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-send-tab-to-complete  ()
  (mistty-with-test-buffer ()
    (mistty-send-text "ech world")
    ;; Move the point before doing completion, to make sure that
    ;; mistty-send-if-at-prompt moves the cursor to the right position
    ;; before sending TAB.
    (mistty-run-command
     (mistty-test-goto-after "ech"))
    (should (equal "$ ech<> world" (mistty-test-content :show (point))))
    (mistty-send-key 1 "\t")
    (mistty-wait-for-output :str "echo")
    (should (equal "$ echo<> world" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-tab-command ()
  (mistty-with-test-buffer ()
    (mistty-send-text "ech world")
    (mistty-run-command
     (mistty-test-goto-after "ech"))
    (should (equal "$ ech<> world" (mistty-test-content :show (point))))
    (mistty-run-command
     (mistty-tab-command))
    (mistty-wait-for-output :str "echo")
    (should (equal "$ echo<> world" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-tab-command-inhibited ()
  (mistty-with-test-buffer ()
    (mistty-send-text "ech world")
    (mistty-run-command
     (mistty-test-goto-after "ech"))
    (should (equal "$ ech<> world" (mistty-test-content :show (point))))
    (mistty-report-long-running-command 'test t)
    (mistty-run-command
     (mistty-tab-command))
    (mistty-wait-for-output :str "ech\t")
    (should (equal "$ ech\t  <> world" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-process-hooks-with-normal-exit ()
  (let (calls
        mistty-after-process-start-hook
        mistty-after-process-end-hook
        term-buffer
        term-proc)
    (add-hook 'mistty-after-process-start-hook
              (lambda () (push `(start
                                 ,(eq (current-buffer) mistty-work-buffer)
                                 ,(and (process-live-p mistty-proc) t)
                                 ,(buffer-live-p mistty-term-buffer))
                               calls)))
    (add-hook 'mistty-after-process-end-hook
              (lambda (proc) (push `(end
                                     ,(eq (current-buffer) mistty-work-buffer)
                                     ,(not (and (process-live-p proc) t))
                                     ,(eq 'exit (and (processp proc) (process-status proc)))
                                     ,(null mistty-proc)
                                     ,(not (buffer-live-p mistty-term-buffer)))
                                   calls)))
    (mistty-with-test-buffer ()
      (setq term-proc mistty-proc)
      (setq term-buffer mistty-term-buffer)
      (should (equal (length calls) 1))
      (mistty-send-text "exit")
      (mistty-send-command)
      (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)
      (should (equal '((start t t t) (end t t t t t)) (nreverse calls))))))

(ert-deftest mistty-test-kill-term-buffer-when-work-buffer-is-killed ()
  (let* ((buffer-and-proc (mistty-with-test-buffer ()
                            (cons mistty-term-buffer mistty-proc)))
         (term-buffer (car buffer-and-proc))
         (term-proc (cdr buffer-and-proc)))
    (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)))

(ert-deftest mistty-test-kill-term-buffer-but-keep-work-buffer ()
  (let* ((calls (list))
         (mistty-after-process-start-hook nil)
         (mistty-after-process-end-hook nil))
    (add-hook 'mistty-after-process-end-hook
              (lambda (proc)
                (push `(end ,(and (processp proc) (process-status proc)))
                      calls)))

    (mistty-with-test-buffer ()
      (let* ((term-buffer mistty-term-buffer)
             (term-proc mistty-proc))
        (kill-buffer term-buffer)
        (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)
        (mistty-wait-for-output :str "Terminal killed." :start (point-min))
        (should (equal (point) (point-max)))))
    (should (equal '((end signal)) calls))))

(ert-deftest mistty-test-term-buffer-exits ()
  (mistty-with-test-buffer ()
    (let ((proc mistty-proc)
          (term-buffer mistty-term-buffer))
      (mistty-send-text "exit")
      (mistty-send-command)
      (mistty-wait-for-output :str "finished" :start (point-min))
      (mistty-wait-for-term-buffer-and-proc-to-die term-buffer proc))))

(ert-deftest mistty-test-scroll-with-long-command ()
  (mistty-with-test-buffer ()
    (let ((loop-command "for i in {0..49}; do echo line $i; done"))
      (mistty-send-text loop-command)
      (should (equal (concat "$ " loop-command "<>") (mistty-test-content :show (point))))
      (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 49) "\n")
                     (mistty-send-and-capture-command-output))))))

(ert-deftest mistty-test-scroll-with-many-commands ()
  (mistty-with-test-buffer ()
    (let ((loop-command "for i in {0..4}; do echo line $i; done"))
      (dotimes (_ 10)
        (mistty-send-text loop-command)
        (should (equal (mapconcat (lambda (i) (format "line %d" i)) (number-sequence 0 4) "\n")
                       (mistty-send-and-capture-command-output nil nil 'nopointer)))))))

(ert-deftest mistty-test-bracketed-paste ()
  (mistty-with-test-buffer ()
    (should (equal mistty-bracketed-paste t))
    (mistty-send-text "printf '(%s/%s) ? ' y n && read yesorno && echo answer: $yesorno")
    (mistty-send-and-wait-for-prompt nil "(y/n) ? ")
    (should (equal mistty-bracketed-paste nil))
    (mistty-send-text "no")
    (should (equal "answer: no" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-bol-eol-outside-of-prompt ()
  (mistty-with-test-buffer ()
    (mistty-send-text "for i in a b c; do echo line $i; done")
    (mistty-send-and-wait-for-prompt)

    ;; outside of a prompt, just call beginning of line/end of line
    (mistty-test-goto "line b")
    (goto-char (+ 4 (point)))
    (mistty-run-command
     (mistty-beginning-of-line))
    (should (equal (point) (mistty-test-goto "line b")))

    (mistty-run-command
     (mistty-end-of-line-or-goto-cursor))
    (should (equal (point) (mistty-test-goto-after "line b")))

    ;; the second time it's called, mistty-end-of-line-or-goto-cursor goes
    ;; to the cursor
    (mistty-run-command
     (let ((this-command 'mistty-end-of-line-or-goto-cursor)
           (last-command 'mistty-end-of-line-or-goto-cursor))
       (mistty-end-of-line-or-goto-cursor)))
    (should (equal (point) (mistty-cursor)))))

(ert-deftest mistty-test-bol-eol-in-prompt ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello, world")

    (mistty-test-goto "hello")
    (mistty-run-command
     (call-interactively #'mistty-beginning-of-line))
    (should (equal (point) (mistty-test-goto "echo")))

    (mistty-run-command
     (call-interactively #'mistty-end-of-line-or-goto-cursor))
    (should (equal (point) (mistty-test-goto-after "world")))))

(ert-deftest mistty-test-no-bracketed-paste-bol-eol-in-possible-prompt ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)

    (mistty-send-text "txt=world; echo hello $txt")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo foo")

    ;; on an old prompt
    (mistty-test-goto "hello $txt")
    (should (equal
             "$ txt=world; echo <>hello $txt\nhello world\n$ echo foo"
             (mistty-test-content :show (point))))
    (mistty-run-command
     (let ((last-command nil)
           (this-command 'mistty-end-of-line-or-goto-cursor))
       (call-interactively #'mistty-end-of-line-or-goto-cursor)))
    (should (equal
             "$ txt=world; echo hello $txt<>\nhello world\n$ echo foo"
             (mistty-test-content :show (point))))

    (mistty-run-command
     (let ((last-command nil)
           (this-command 'mistty-beginning-of-line))
       (call-interactively #'mistty-beginning-of-line)))
    (should (equal
             "<>$ txt=world; echo hello $txt\nhello world\n$ echo foo"
             (mistty-test-content :show (point))))

    ;; not on any prompt
    (mistty-test-goto "hello world")
    (mistty-run-command
     (let ((last-command nil)
           (this-command 'mistty-beginning-of-line))
       (call-interactively #'mistty-beginning-of-line)))
    (should (equal
             "$ txt=world; echo hello $txt\n<>hello world\n$ echo foo"
             (mistty-test-content :show (point))))

    (mistty-test-goto "hello world")
    (mistty-run-command
     (let ((last-command nil)
           (this-command 'mistty-end-of-line-or-goto-cursor))
       (call-interactively #'mistty-end-of-line-or-goto-cursor)))
    (should (equal
             "$ txt=world; echo hello $txt\nhello world<>\n$ echo foo"
             (mistty-test-content :show (point))))

    ;; on the (new) prompt (sending C-a/C-e, so the BOL position
    ;; doesn't include the prompt and we must use wait-for-output.)
    (mistty-test-goto "foo")
    (mistty-run-command
     (let ((last-command nil)
           (this-command 'mistty-beginning-of-line))
       (call-interactively #'mistty-beginning-of-line)))
    (mistty-wait-for-output
     :test (lambda ()
             (equal (point) (mistty-test-goto "echo foo"))))

    (mistty-test-goto "foo")
    (mistty-run-command
     (let ((last-command nil)
           (this-command 'mistty-beginning-of-line))
       (call-interactively #'mistty-end-of-line-or-goto-cursor)))
    (mistty-wait-for-output
     :test (lambda ()
             (equal (point) (mistty-test-goto-after "echo foo"))))))

(ert-deftest mistty-test-eol-empty-prompt ()
  (mistty-with-test-buffer ()
    (goto-char (point-min))
    (mistty-run-command
     (mistty-end-of-line-or-goto-cursor))

    (should
     (equal "$ <>"
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-bol-is-queued ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (goto-char (point-min))
    (mistty--enqueue mistty--queue (mistty--stuck-interaction " world"))
    (mistty-beginning-of-line)
    ;; insert # at the beginning of the line
    (mistty--enqueue-str mistty--queue "#")
    ;; mistty-beginning-of-line should be queued should not have an
    ;; effect until "world' has been written.
    (should (equal "<>$ echo hello" (mistty-test-content :show (point))))
    (should-not mistty-goto-cursor-next-time)
    (mistty--send-string mistty-proc " world")
    (mistty-wait-for-output :str "#echo hello world" :start (point-min))
    (should (equal "$ #<>echo hello world" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-eol-is-queued ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (goto-char (point-min))
    (mistty--enqueue mistty--queue (mistty--stuck-interaction " world"))
    (mistty-end-of-line)
    ;; insert . at the end of the line
    (mistty--enqueue-str mistty--queue ".")
    ;; mistty-end-of-line should be queued should not have an
    ;; effect until "world' has been written.
    (should (equal "<>$ echo hello" (mistty-test-content :show (point))))
    (should-not mistty-goto-cursor-next-time)
    (mistty--send-string mistty-proc " world")
    (mistty-wait-for-output :str "echo hello world." :start (point-min))
    (should (equal "$ echo hello world.<>" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-bol-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (mistty-simulate-scrollback-buffer
     (call-interactively 'mistty-beginning-of-line))))

(ert-deftest mistty-test-eol-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (goto-char (mistty--bol (point)))
    (mistty-simulate-scrollback-buffer
     (call-interactively 'mistty-end-of-line)

     (should (equal "$ echo hello<>"
                    (mistty-test-content :show (point))))

     (let ((this-command 'mistty-end-of-line)
           (last-command 'mistty-end-of-line))
       (call-interactively 'mistty-end-of-line))

     (should (equal "$ echo hello<>"
                    (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-eol-or-goto-cursor-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (goto-char (mistty--bol (point)))
    (mistty-simulate-scrollback-buffer
     (call-interactively 'mistty-end-of-line-or-goto-cursor)

     (should (equal "$ echo hello<>"
                    (mistty-test-content :show (point))))

     (let ((this-command 'mistty-end-of-line)
           (last-command 'mistty-end-of-line))
       (call-interactively 'mistty-end-of-line-or-goto-cursor))

     (should (equal "$ echo hello<>"
                    (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-goto-cursor ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (goto-char (point-min))
    (mistty-run-command
     (call-interactively 'mistty-goto-cursor))
    (should (equal (point) (mistty-cursor)))))

(ert-deftest mistty-test-goto-cursor-in-scrollback-buffer ()
  (mistty-with-test-buffer ()
    (mistty-simulate-scrollback-buffer
     (should-error (call-interactively 'mistty-goto-cursor)))))

(ert-deftest mistty-test-next-input ()
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
    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "<>$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "<>$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "<>$ echo current")
            (mistty-test-content :show (point))))

    (should-error (mistty-next-input 1))
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "<>$ echo current")
            (mistty-test-content :show (point))))

    (goto-char (point-min))
    (mistty-next-input 2)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "<>$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (goto-char (point-min))
    (mistty-next-input 3)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "<>$ echo current")
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-previous-input ()
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

    (mistty-test-goto "current")

    (mistty-previous-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "<>$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-previous-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "<>$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-previous-input 1)
    (should
     (equal (concat "<>$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (should-error (mistty-previous-input 1))
    (should
     (equal (concat "<>$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-test-goto "current")
    (mistty-previous-input 2)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "<>$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-next-input-zsh ()
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
    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "<>$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "<>$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-next-input-fish ()
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
    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "<>$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "<>$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-next-input-empty-prompt ()
  (mistty-with-test-buffer ()
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (goto-char (point-min))
    (mistty-next-input 1)
    (should
     (equal (concat "$\n"
                    "<>$\n"
                    "$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$\n"
                    "$\n"
                    "<>$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$\n"
                    "$\n"
                    "$\n"
                    "<>$ echo current")
            (mistty-test-content :show (point))))

    (should-error (mistty-next-input 1))

    (goto-char (point-min))
    (mistty-next-input 2)
    (should
     (equal (concat "$\n"
                    "$\n"
                    "<>$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (goto-char (point-min))
    (mistty-next-input 3)
    (should
     (equal (concat "$\n"
                    "$\n"
                    "$\n"
                    "<>$ echo current")
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-previous-input-empty-prompt ()
  (mistty-with-test-buffer ()
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (mistty-test-goto "current")
    (mistty-previous-input 1)
    (should
     (equal (concat "$\n"
                    "$\n"
                    "<>$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-previous-input 1)
    (should
     (equal (concat "$\n"
                    "<>$\n"
                    "$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-previous-input 1)
    (should
     (equal (concat "<>$\n"
                    "$\n"
                    "$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (should-error (mistty-previous-input 1))

    (mistty-test-goto "current")
    (mistty-previous-input 2)
    (should
     (equal (concat "$\n"
                    "<>$\n"
                    "$\n"
                    "$ echo current")
            (mistty-test-content :show (point))))))

(ert-deftest mistty-test-next-input-nobracketed-paste ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-send-text "echo $((1 + 1))")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo $((2 + 2))")
    (goto-char mistty-test-content-start)
    (mistty-next-input 1)
    (should (equal (concat "$ echo $((1 + 1))\n"
                           "2\n"
                           "<>$ echo $((2 + 2))")
                   (mistty-test-content :show (point))))
    (mistty-previous-input 1)
    (should (equal (concat "<>$ echo $((1 + 1))\n"
                           "2\n"
                           "$ echo $((2 + 2))")
                   (mistty-test-content :show (point))))))

(ert-deftest mistty-test-next-output ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo two"))
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo three"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (let ((range))
      (goto-char (point-min))
      (setq range (mistty-next-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "<>one\n<1>"
                      "$ echo two\n"
                      "two\n"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (list (point) (cdr range)))))
      (should (equal (point) (car range)))

      (setq range (mistty-next-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "<>two\n<1>"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (list (point) (cdr range)))))
      (should (equal (point) (car range)))

      (setq range (mistty-next-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "two\n"
                      "$\n"
                      "$ echo three\n"
                      "<>three\n<1>"
                      "$ echo current")
              (mistty-test-content :show (list (point) (cdr range)))))
      (should (equal (point) (car range)))

      (should-error (mistty-next-output 1))

      (goto-char (point-min))
      (mistty-next-output 2)
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "<>two\n"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (point))))

      (goto-char (point-min))
      (mistty-next-output 3)
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "two\n"
                      "$\n"
                      "$ echo three\n"
                      "<>three\n"
                      "$ echo current")
              (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-previous-output ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo two"))
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo three"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (mistty-test-goto "current")

    (let (range)

      (setq range (mistty-previous-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "two\n"
                      "$\n"
                      "$ echo three\n"
                      "<>three\n<1>"
                      "$ echo current")
              (mistty-test-content :show (list (point) (cdr range)))))
      (should (equal (point) (car range)))

      (setq range (mistty-previous-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "<>two\n<1>"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (list (point) (cdr range)))))
      (should (equal (point) (car range)))

      (setq range (mistty-previous-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "<>one\n<1>"
                      "$ echo two\n"
                      "two\n"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (list (point) (cdr range)))))
      (should (equal (point) (car range)))

      (should-error (mistty-previous-output 1))
      (should
       (equal (concat "$ echo one\n"
                      "<>one\n"
                      "$ echo two\n"
                      "two\n"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (point))))

      (mistty-test-goto "current")
      (mistty-previous-output 2)
      (should
       (equal (concat "$ echo one\n"
                      "one\n"
                      "$ echo two\n"
                      "<>two\n"
                      "$\n"
                      "$ echo three\n"
                      "three\n"
                      "$ echo current")
              (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-select-output ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo two"))
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo three"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (mistty-select-output)
    (should (equal (concat "$ echo one\n"
                   "one\n"
                   "$ echo two\n"
                   "two\n"
                   "$\n"
                   "$ echo three\n"
                   "<>three\n<1>"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (mistty-test-goto "echo two")
    (forward-line 1)
    (mistty-select-output)
    (should (equal (concat "$ echo one\n"
                   "one\n"
                   "$ echo two\n"
                   "<>two\n<1>"
                   "$\n"
                   "$ echo three\n"
                   "three\n"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (goto-char (1+ (point)))
    (mistty-select-output)
    (should (equal (concat "$ echo one\n"
                   "one\n"
                   "$ echo two\n"
                   "<>two\n<1>"
                   "$\n"
                   "$ echo three\n"
                   "three\n"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (mistty-select-output 1)
    (should (equal (concat "$ echo one\n"
                   "<>one\n<1>"
                   "$ echo two\n"
                   "two\n"
                   "$\n"
                   "$ echo three\n"
                   "three\n"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (mistty-test-goto "current")
    (mistty-select-output 2)
    (should (equal (concat "$ echo one\n"
                   "one\n"
                   "$ echo two\n"
                   "<>two\n<1>"
                   "$\n"
                   "$ echo three\n"
                   "three\n"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (mistty-test-goto "echo one")
    (should-error (mistty-select-output))))

(ert-deftest mistty-test-select-output-eob ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (goto-char (point-max))

    (mistty-select-output)
    (equal (concat "$ echo one\n"
                   "<>one\n<1>"
                   "$ echo current")
           (mistty-test-content :show (list (point) (mark))))))

(ert-deftest mistty-test-select-output-bob ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (mistty-test-goto "echo one")
    (forward-line)
    (delete-region (point-min) (point))

    (mistty-select-output)
    (should (equal (concat "<>one\n<1>"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (goto-char (1+ (point)))
    (mistty-select-output)
    (should (equal (concat "<>one\n<1>"
                   "$ echo current")
                   (mistty-test-content :show (list (point) (mark)))))

    (goto-char (point-max))
    (mistty-select-output)
    (equal (concat "<>one\n<1>"
                   "$ echo current")
           (mistty-test-content :show (list (point) (mark))))))

(ert-deftest mistty-test-mistty-clear ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo two"))
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo three"))
    (mistty-send-and-wait-for-prompt)
    (mistty-clear 2)

    (setq mistty-test-content-start 1)
    (should (equal "$\n$ echo three\nthree\n$" (mistty-test-content)))

    (mistty-clear 1)
    (should (equal "$" (mistty-test-content)))))

(ert-deftest mistty-test-dirtrack ()
  (mistty-with-test-buffer ()
    (mistty-send-text "cd /")
    (mistty-send-and-wait-for-prompt)
    (should (equal "/" default-directory))
    (mistty-send-text "cd ~")
    (mistty-send-and-wait-for-prompt)
    (should (equal (file-name-as-directory (getenv "HOME")) default-directory))))

(ert-deftest mistty-test-bash-backward-history-search ()
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
    (mistty-test-narrow (mistty--bol (point)))
    (mistty--send-string mistty-proc "\C-r")
    (mistty-wait-for-output :str "reverse")
    (should (equal "(reverse-i-search)`': <>" (mistty-test-content :show (mistty-cursor))))
    (execute-kbd-macro (kbd "e c"))
    (mistty-wait-for-output :str "`ec'")
    (should (equal "(reverse-i-search)`ec': <>echo third" (mistty-test-content :show (mistty-cursor))))
    (execute-kbd-macro (kbd "o"))
    (mistty-wait-for-output :str "`eco'")
    (should (equal "(reverse-i-search)`eco': echo s<>econd" (mistty-test-content :show (mistty-cursor))))
    (execute-kbd-macro (kbd "DEL"))
    (mistty-wait-for-output :str "`ec'")
    (should (equal "(reverse-i-search)`ec': echo s<>econd" (mistty-test-content :show (mistty-cursor))))
    (should (equal "second" (mistty-send-and-capture-command-output
                             (lambda ()
                               (execute-kbd-macro (kbd "RET"))))))))

(ert-deftest mistty-test-skipped-spaces ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-send-text "for i in (seq 10)\necho line $i\nend")

    (should (equal (concat "$ for i in (seq 10)\n"
                           "[      ]echo line $i\n"
                           "[  ]end")
                   (mistty-test-content
                    :strip-last-prompt t
                    :show-property '(mistty-skip indent))))))

(ert-deftest mistty-test-insert-long-prompt ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 20 20)

    (mistty-run-command
     (insert "echo one two three four five six seven eight nine"))
    (mistty-wait-for-output :str "nine" :cursor-at-end t)
    (should (equal "$ echo one two three\n four five six seven\n eight nine"
                   (mistty-test-content)))))

(ert-deftest mistty-test-keep-sync-marker-on-long-prompt ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 20 20)

    (mistty-run-command
     (insert "echo one two three four five six seven eight nine"))
    (mistty-wait-for-output :str "nine" :cursor-at-end t)

    ;; make sure that the newlines didn't confuse the sync marker
    (should (equal (marker-position mistty-sync-marker) (point-min)))))

(ert-deftest mistty-test-keep-pointer-on-long-prompt ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 20 20)

    (mistty-run-command
     (insert "echo one two three four five six seven eight nine"))
    (mistty-wait-for-output :str "nine" :cursor-at-end t)

    ;; make sure that the newlines don't confuse mistty--post-command
    ;; moving the cursor.
    (dolist (count '("three" "nine" "four"))
      (let ((goal-pos))
        (mistty-run-command
         (setq goal-pos (mistty-test-goto count)))
        (should (equal (mistty-cursor) goal-pos))))))

(ert-deftest mistty-test-delete-fake-nl-after-long-prompts ()
  (mistty-with-test-buffer ()
    (let ((mistty--inhibit-fake-nl-cleanup nil))
      (mistty--set-process-window-size 20 20)

      (mistty-run-command
       (insert "echo one two three four five six seven eight nine"))
      (mistty-wait-for-output :str "nine" :cursor-at-end t)
      (mistty-send-and-wait-for-prompt)

      (should (equal (concat "$ echo one two three four five six seven eight nine\n"
                             "one two three four five six seven eight nine\n"
                             "$")
                     (mistty-test-content))))))

(ert-deftest mistty-test-display-long-prompt ()
  (turtles-ert-test :instance 'mistty)

  (mistty-with-test-buffer (:selected t)
    (delete-other-windows)

    (dolist (word '("echo" "one" "two" "three" "four" "five" "six" "seven"
                    "eight" "nine" "ten" "eleven" "twelve" "thirteen"
                    "fourteen" "fifteen" "sixteen" "seventeen" "eighteen"
                    "nineteen"))
      (mistty--send-string mistty-proc (concat word " ")))
    (mistty-send-text "twenty")
    (mistty-send-and-wait-for-prompt)

    ;; Term adds fake newlines, but mistty makes them invisible and
    ;; relies on window continuation. The effect of the above setup is
    ;; visible below as Emacs adds "\" at then end of some lines.
    (turtles-with-grab-buffer ()
      (should
       (equal
        (concat
         "$ echo one two three four five six seven eight nine ten eleven twelve thirteen\\\n"
         " fourteen fifteen sixteen seventeen eighteen nineteen twenty\n"
         "one two three four five six seven eight nine ten eleven twelve thirteen fourte\\\n"
         "en fifteen sixteen seventeen eighteen nineteen twenty\n"
         "$")
        (buffer-string))))))

(ert-deftest mistty-test-enter-fullscreen ()
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

(ert-deftest mistty-test-enter-fullscreen-alternative-code ()
  (mistty-with-test-buffer (:selected t)
    (mistty-test-enter-fullscreen "[?47h" "[?47l")))

(ert-deftest mistty-test-enter-fullscreen-1047 ()
  (mistty-with-test-buffer (:selected t)
    (mistty-test-enter-fullscreen "[?1047h" "[?1047l")))

(ert-deftest mistty-test-enter-fullscreen-1049 ()
  (mistty-with-test-buffer (:selected t)
    (mistty-test-enter-fullscreen "[?1049h" "[?1049l")))

(ert-deftest mistty-test-call-fullscreen-hooks ()
  (let (calls
        mistty-after-process-end-hook
        mistty-entered-fullscreen-hook
        mistty-left-fullscreen-hook)
    (add-hook 'mistty-after-process-end-hook
              (lambda (proc)
                (push `(end
                        ,(not mistty-fullscreen)
                        ,(not (process-live-p proc)))
                      calls)))
    (add-hook 'mistty-entered-fullscreen-hook
              (lambda ()
                (push `(enter ,mistty-fullscreen) calls)))
    (add-hook 'mistty-left-fullscreen-hook
              (lambda ()
                (push `(leave ,(not mistty-fullscreen)) calls)))

    (mistty-with-test-buffer (:selected t)
      (mistty-test-enter-fullscreen "[?47h" "[?47l"))

    (should (equal '((enter t) (leave t)) (nreverse calls)))))

(ert-deftest mistty-test-killed-while-fullscreen ()
  (let (calls
        mistty-after-process-end-hook
        mistty-entered-fullscreen-hook
        mistty-left-fullscreen-hook)
    (add-hook 'mistty-after-process-end-hook
              (lambda (proc)
                (push `(end
                        ,(not mistty-fullscreen)
                        ,(not (process-live-p proc)))
                      calls)))
    (add-hook 'mistty-entered-fullscreen-hook
              (lambda ()
                (push `(enter ,mistty-fullscreen) calls)))
    (add-hook 'mistty-left-fullscreen-hook
              (lambda ()
                (push `(leave ,(not mistty-fullscreen)) calls)))

    (mistty-with-test-buffer ()
      (let ((term-buffer mistty-term-buffer)
            (term-proc mistty-proc))
        (mistty-send-text "printf '\\e[?47h'; echo fullscreen on; exit")
        (mistty-send-command)
        (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)))

    (should (equal '((enter t) (leave t) (end t t)) (nreverse calls)))))

(ert-deftest mistty-test-live-buffer-p ()
  (mistty-with-test-buffer ()
    (should (mistty-live-buffer-p mistty-work-buffer))
    (should (not (mistty-live-buffer-p mistty-term-buffer))))
  (with-temp-buffer
    (should (not (mistty-live-buffer-p (current-buffer))))))

(ert-deftest mistty-test-fullscreen-live-buffer-p ()
  (mistty-with-test-buffer ()
    (let ((proc mistty-proc))
      (mistty-send-text
       (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off'"
               "[47h" "[47l"))
      (mistty-send-command)
      (mistty-wait-for-output
       :test
       (lambda ()
         (buffer-local-value 'mistty-fullscreen mistty-work-buffer)))

      (should (not (mistty-live-buffer-p mistty-work-buffer)))
      (should (mistty-live-buffer-p mistty-term-buffer))

      ;; Cleanup. Without this, the term buffer would not be killed
      ;; when the work buffer is killed since it's in fullscreen mode,
      ;; and so considered the main buffer.
      (mistty--send-string proc "\C-m")
      (mistty-wait-for-output
       :test
       (lambda ()
         (not (buffer-local-value 'mistty-fullscreen mistty-work-buffer)))))))

(ert-deftest mistty-test-toggle-buffers ()
  (mistty-with-test-buffer (:selected t)
    (let ((proc mistty-proc)
          (work-buffer mistty-work-buffer)
          (term-buffer mistty-term-buffer)
          (win (selected-window)))
      (should (equal mistty-work-buffer (window-buffer win)))
      (mistty-send-text
       (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off'"
               "[47h" "[47l"))
      (mistty-send-command)
      (mistty-wait-for-output
       :test
       (lambda ()
         (buffer-local-value 'mistty-fullscreen work-buffer)))

      (should (equal term-buffer (window-buffer win)))
      (with-current-buffer (window-buffer win)
        (mistty-toggle-buffers))
      (should (equal work-buffer (window-buffer win)))
      (with-current-buffer (window-buffer win)
        (mistty-toggle-buffers))
      (should (equal term-buffer (window-buffer win)))

      ;; Cleanup.
      (mistty--send-string proc "\C-m")
      (mistty-wait-for-output
       :test
       (lambda ()
         (not (buffer-local-value 'mistty-fullscreen work-buffer)))))))

(ert-deftest mistty-test-toggle-buffers-not-fullscreen ()
  (mistty-with-test-buffer (:selected t)
    (let ((work-buffer mistty-work-buffer)
          (win (selected-window)))
      (should (equal work-buffer (window-buffer win)))
      (should-error (mistty-toggle-buffers))
      (should (equal work-buffer (window-buffer win))))))

(ert-deftest mistty-test-fullscreen-swap-buffers ()
  (mistty-with-test-buffer (:selected t)
    (let ((proc mistty-proc)
          (work-buffer mistty-work-buffer)
          (term-buffer mistty-term-buffer)
          (scratch (get-buffer-create "*scratch*"))
          (winA (selected-window))
          winA-prevs
          winB-prevs
          winB)
      (delete-other-windows)
      (set-window-buffer winA scratch)
      (set-window-buffer winA work-buffer)
      (split-window-vertically)
      (other-window 1)
      (setq winB (selected-window))
      (set-window-buffer winB scratch)
      (set-window-buffer winB term-buffer)
      (setq winA-prevs (mapcar #'car (window-prev-buffers winA)))
      (setq winB-prevs (mapcar #'car (window-prev-buffers winB)))

      (should (equal work-buffer (window-buffer winA)))
      (should (equal term-buffer (window-buffer winB)))

      ;; Enter fullscreen
      (with-current-buffer work-buffer
        (mistty-send-text
         (format "printf '\\e%sPress ENTER: ' && read && printf '\\e%sfullscreen off'"
                 "[47h" "[47l"))
        (mistty-send-command)
        (mistty-wait-for-output
         :test
         (lambda ()
           (buffer-local-value 'mistty-fullscreen work-buffer))))

      ;; The buffers have been swapped; prev-buffers doesn't register
      ;; that there was a change, though.
      (should (equal term-buffer (window-buffer winA)))
      (should (equal work-buffer (window-buffer winB)))
      (should (equal winA-prevs (mapcar #'car (window-prev-buffers winA))))
      (should (equal winB-prevs (mapcar #'car (window-prev-buffers winB))))

      ;; Leave fullscreen
      (with-current-buffer work-buffer
        (mistty--send-string proc "\C-m")
        (mistty-wait-for-output
         :test
         (lambda ()
           (not (buffer-local-value 'mistty-fullscreen work-buffer)))))

      ;; Back to the initial setup
      (should (equal work-buffer (window-buffer winA)))
      (should (equal term-buffer (window-buffer winB))))))

(ert-deftest mistty-test-kill-fullscreen-buffer-kills-scrollback ()
  (mistty-with-test-buffer (:selected t)
    (let ((work-buffer mistty-work-buffer)
          (proc mistty-proc))
      (should (executable-find "vi"))
      (execute-kbd-macro (kbd "v i RET"))
      (mistty-wait-for-output :test (lambda () mistty-fullscreen))

      (kill-buffer mistty-term-buffer)
      (mistty-wait-for-term-buffer-and-proc-to-die work-buffer proc))))

(ert-deftest mistty-test-proc-dies-during-fullscreen ()
  (mistty-with-test-buffer (:selected t)
    (let ((bufname (buffer-name))
          (work-buffer mistty-work-buffer)
          (term-buffer mistty-term-buffer)
          (proc mistty-proc))

      (mistty-send-text "printf '\\e[47hFullscreen' && exit 99")
      (mistty-send-command)

      (mistty-wait-for-term-buffer-and-proc-to-die term-buffer proc)

      (should (buffer-live-p work-buffer))
      (should (eq work-buffer (window-buffer (selected-window))))
      (should (string-match "exited abnormally" (buffer-substring-no-properties (point-min) (point-max))))
      (should (equal bufname (buffer-name work-buffer)))
      (should (not (buffer-local-value 'mistty-fullscreen mistty-work-buffer))))))

(ert-deftest mistty-test-osc ()
  (mistty-with-test-buffer ()
    (let* ((osc-list)
           (mistty-osc-handlers
            `(("8" . ,(lambda (code text)
                        (push (cons code text) osc-list))))))
      (mistty-send-text "printf '\\e]8;;http://www.example.com\\aSome OSC\\e]8;;\\a!\\n'")
      (should (equal "Some OSC!" (mistty-send-and-capture-command-output)))
      (should (equal '(("8" . ";http://www.example.com") ("8" . ";")) (nreverse osc-list))))))

(ert-deftest mistty-test-osc-standard-end ()
  (mistty-with-test-buffer ()
    (let* ((osc-list)
           (mistty-osc-handlers
            `(("8" . ,(lambda (code text)
                        (push (cons code text) osc-list))))))
      (mistty-send-text "printf '\\e]8;;http://www.example.com\\e\\\\Some OSC\\e]8;;\\e\\\\!\\n'")
      (should (equal "Some OSC!" (mistty-send-and-capture-command-output)))
      (should (equal '(("8" . ";http://www.example.com") ("8" . ";")) (nreverse osc-list))))))

(ert-deftest mistty-test-osc-add-text-properties ()
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

(ert-deftest mistty-test-reset ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "printf '\\ec'")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)
    (should (equal "$ echo one\none\n$ printf '\\ec'\n$ echo two\ntwo\n$"
                   (mistty-test-content)))))

(ert-deftest mistty-test-clear ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "clear")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)
    (should (equal "$ echo one\none\n$ clear\n$ echo two\ntwo\n$"
                   (mistty-test-content)))))

(ert-deftest mistty-test-scrolls-window-after-clear ()
  (turtles-ert-test :instance 'mistty)

  (mistty-with-test-buffer (:shell zsh :selected t)
    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "clear")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)
    (should (equal "$ echo one\none\n$ clear\n$ echo two\ntwo\n$"
                   (mistty-test-content)))
    (turtles-with-grab-buffer (:win (selected-window) :point "<>")
      (should (equal "$ echo two\ntwo\n$ <>" (buffer-string))))))

(ert-deftest mistty-test-scrolls-window-after-reset ()
  (turtles-ert-test :instance 'mistty)

  (mistty-with-test-buffer (:shell zsh :selected t)
    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "reset -Q")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)
    (should (equal "$ echo one\none\n$ reset -Q\n$ echo two\ntwo\n$"
                   (mistty-test-content)))
    (turtles-with-grab-buffer (:win (selected-window) :point "<>")
      (should (equal "$ echo two\ntwo\n$ <>" (buffer-string))))))

(ert-deftest mistty-test-clear-screen ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "printf '\\e[2J'")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)
    (should (equal "$ echo one\none\n$ printf '\\e[2J'\n$ echo two\ntwo"
                   (mistty-test-content :strip-last-prompt t)))))

(ert-deftest mistty-test-hide-cursor ()
  (mistty-with-test-buffer ()
    (mistty-send-text "printf 'o\\e[?25lk\\n'")
    (should (equal "ok" (mistty-send-and-capture-command-output)))
    (should (eq nil cursor-type))
    (mistty-send-text "printf 'o\\e[?25hk\\n'")
    (should (equal "ok" (mistty-send-and-capture-command-output)))
    (should (eq t cursor-type))))

(ert-deftest mistty-test-restore-local-cursor ()
  (let ((cursor-type 'box))
    (mistty-with-test-buffer ()
      (setq cursor-type '(bar . 2))
      (mistty-send-text "printf 'o\\e[?25lk\\n'")
      (should (equal "ok" (mistty-send-and-capture-command-output)))
      (should (eq nil cursor-type))
      (mistty-send-text "printf 'o\\e[?25hk\\n'")
      (should (equal "ok" (mistty-send-and-capture-command-output)))
      (should (equal '(bar . 2) cursor-type)))))

(ert-deftest mistty-test-restore-global-cursor ()
  (let ((cursor-type 'box))
    (mistty-with-test-buffer ()
      (mistty-send-text "printf 'o\\e[?25lk\\n'")
      (should (equal "ok" (mistty-send-and-capture-command-output)))
      (should (eq nil cursor-type))
      (mistty-send-text "printf 'o\\e[?25hk\\n'")
      (should (equal "ok" (mistty-send-and-capture-command-output)))
      (should (equal 'box cursor-type))
      (should-not (local-variable-p 'cursor-type)))))

(ert-deftest mistty-test-show-cursor-if-moved ()
  (mistty-with-test-buffer ()
    (mistty-send-text "printf 'o\\e[?25lk\\n'")
    (should (equal "ok" (mistty-send-and-capture-command-output)))
    (should (eq nil cursor-type))
    (mistty-send-text "echo ok")
    (mistty-run-command (goto-char (1- (point))))
    (should (eq t cursor-type))))

(ert-deftest mistty-test-detect-possible-prompt ()
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

(ert-deftest mistty-test-nobracketed-paste-just-type ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-send-text "echo ok")
    (should (equal "ok" (mistty-send-and-capture-command-output)))

    ;; the input was identified and labelled
    (mistty-previous-input 1)
    (should (looking-at (regexp-quote "$ echo ok")))))

(ert-deftest mistty-test-nobracketed-paste-move-and-type ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-send-text "echo ack")
    (mistty-run-command
     (mistty-test-goto "ack"))
    (mistty-run-command
     (mistty-send-key 1 "n"))
    (should (equal "nack" (mistty-send-and-capture-command-output)))

    ;; the input was identified and labelled
    (mistty-previous-input 1)
    (should (looking-at (regexp-quote "$ echo nack")))))

(ert-deftest mistty-test-eof ()
  (mistty-with-test-buffer ()
    (let ((buf mistty-term-buffer)
          (proc mistty-proc))
      (mistty-send-key 1 "\C-d")
      (mistty-wait-for-term-buffer-and-proc-to-die buf proc))))

(ert-deftest mistty-test-nobracketed-paste-delchar ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-send-text "echo nok")
    (mistty-run-command
     (mistty-test-goto "nok")
     (mistty-send-key 1 "\C-d"))
    ;; deleted the first 1, the command-line is now 1 + 1
    (should (equal "ok" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-nobracketed-paste-edit-prompt ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-run-command
     (insert "echo ok"))

    (should (equal "ok" (mistty-send-and-capture-command-output)))

    ;; the input was identified and labelled
    (mistty-previous-input 1)
    (should (equal "<>$ echo ok\nok\n$"
                   (mistty-test-content :show (point))))))

(ert-deftest mistty-test-nobracketed-paste-edit-before-prompt ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)

    (mistty-send-text "echo $((1 + 1))")
    (should (equal "2" (mistty-send-and-capture-command-output)))

    (mistty-send-text "echo $((3 + 3))")
    (should (equal "6" (mistty-send-and-capture-command-output)))

    (mistty-send-text "echo $((5 + 5))")

    (mistty-run-command
     (goto-char (point-min))
     (while (search-forward "+" nil 'noerror)
       (replace-match "*")))

    ;; The last prompt became 5 * 5
    (should (equal "25" (mistty-send-and-capture-command-output)))

    ;; the text of the previous prompts was modified, too.
    (mistty-test-goto "1 * 1")
    (mistty-test-goto "3 * 3")))

(ert-deftest mistty-test-edit-without-prompt ()
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

(ert-deftest mistty-test-send-key-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-simulate-scrollback-buffer
     (should-error (mistty-send-key 1 (kbd "a"))))))

(ert-deftest mistty-test-send-key-is-queued ()
  (mistty-with-test-buffer ()
    (goto-char (point-min)) ;; should be moved by mistty-send-key
    (mistty--enqueue mistty--queue (mistty--stuck-interaction "ok."))
    (mistty-send-key 1 (kbd "a"))
    (should-not mistty-goto-cursor-next-time)
    (should-not buffer-undo-list)
    (mistty--send-string mistty-proc "ok.")
    (mistty-wait-for-output :str "ok.a" :start (point-min))
    (should buffer-undo-list)
    (should (equal "$ ok.a<>" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-self-insert ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (mistty-self-insert 1 ?e))
    (mistty-run-command
     (mistty-self-insert 1 ?c))
    (mistty-run-command
     (mistty-self-insert 1 ?h))
    (mistty-run-command
     (mistty-self-insert 1 ?o))

    (mistty-wait-for-output :str "echo")))

(ert-deftest mistty-test-delete-char ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello world")
    (mistty-run-command
     (mistty-test-goto-after "hell"))

    (mistty-delete-char)
    (mistty-wait-for-output :str "echo hell world" :start (point-min))

    (mistty-delete-char -3)
    (mistty-wait-for-output :str "echo h world" :start (point-min))

    (mistty-backward-delete-char 2)
    (mistty-wait-for-output :str "echo world" :start (point-min))

    (mistty-backward-delete-char -3)
    (mistty-wait-for-output :str "echorld" :start (point-min))))

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

(ert-deftest mistty-test-send-key-sequence  ()
  (mistty-with-test-buffer (:selected t)
    (mistty-send-text "echo abc def")
    (ert-simulate-keys '(?\C-w ?g ?i ?j ?\C-b ?\C-b ?h ?\C-e ?\C-g)
      (mistty-send-key-sequence))
    (mistty-wait-for-output :str "ghij" :cursor-at-end t)
    (should (equal "abc ghij" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-send-key-sequence-paste  ()
  (mistty-with-test-buffer (:selected t)
    (mistty-send-text "echo abc def")
    (ert-simulate-keys '((xterm-paste "hello\necho worl") ?d ?\C-g)
      (mistty-send-key-sequence))
    (mistty-wait-for-output :str "world" :cursor-at-end t)
    (should (equal
             (concat "$ echo abc defhello\n"
                     "echo world")
             (mistty-test-content)))))

(ert-deftest mistty-test-send-key-sequence-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-simulate-scrollback-buffer
     (should-error (call-interactively 'mistty-send-key-sequence)))))

(ert-deftest mistty-test-revert-insert-after-prompt ()
  (mistty-with-test-buffer (:shell zsh)
    (dotimes (i 3)
      (mistty-send-text (format "function toto%d { echo %d; };" i i)))
    (mistty-send-and-wait-for-prompt)
    (mistty-test-narrow (mistty--bol (point)))
    (mistty-send-text "toto\t")

    ;; This test goes outside the prompt on purpose, which is why a
    ;; timeout is expected.
    (let ((mistty-expected-issues '(hard-timeout)))
      (mistty-wait-for-output
       :test (lambda ()
               (search-forward-regexp "^toto" nil 'noerror)))
      (mistty-run-command
       (insert "foobar")
       (mistty-test-goto-after "$ toto"))
      (should (equal "$ toto<>\ntoto0  toto1  toto2"
                     (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-revert-replace-after-prompt ()
  (mistty-with-test-buffer (:shell zsh)
    (dotimes (i 10)
      (mistty-send-text (format "function toto%d { echo %d; };" i i))
      (mistty-send-and-wait-for-prompt))
    (let ((start (pos-bol))
          (mistty-expected-issues '(hard-timeout)))
      (mistty-send-text "toto")
      (mistty-run-command
       (mistty-tab-command))
      (mistty-run-command
       (save-excursion
         (goto-char (mistty-cursor))
         (while (search-forward "toto" nil t)
           (replace-match "tata" nil t))))

      (should (equal
               "$ toto\ntoto0  toto1  toto2  toto3  toto4  toto5  toto6  toto7  toto8  toto9"
               (mistty-test-content :start start))))))

(ert-deftest mistty-test-insert-at-prompt ()
  (mistty-with-test-buffer (:shell zsh)
    (mistty-send-text "world")
    (let ((mistty-expected-issues '(hard-timeout)))
      (mistty-run-command
       (mistty-test-goto "$ world")
       (insert "echo hello ")))
    (should (equal "$ echo hello world"
                   (mistty-test-content)))))

(ert-deftest mistty-test-revert-replace-at-prompt ()
  (mistty-with-test-buffer (:shell zsh)
    (mistty-send-text "echo ok")
    (let ((mistty-expected-issues '(hard-timeout)))
      (mistty-run-command
       (mistty-test-goto "$ echo ok")
       (delete-char 1)
       (insert "command>")
       (mistty-test-goto "ok")))
    (should (equal "$ echo ok"
                   (mistty-test-content)))))

(ert-deftest mistty-test-revert-delete-at-prompt ()
  (mistty-with-test-buffer (:shell zsh)
    (mistty-send-text "echo ok")
    (let ((mistty-expected-issues '(hard-timeout)))
      (mistty-run-command
       (mistty-test-goto "$ echo ok")
       (delete-char 2)
       (mistty-test-goto "ok")))
    (should (equal "$ echo ok"
                   (mistty-test-content)))))

(ert-deftest mistty-test-replace-prompt ()
  (mistty-with-test-buffer (:shell zsh)
    (mistty-send-text "echo ok")
    (let ((mistty-expected-issues '(hard-timeout)))
      (mistty-run-command
       (mistty-test-goto "$ echo ok")
       (delete-region
        (point)
        (mistty-test-pos-after "$ echo ok"))
       (insert "echo hello, world"))
    (should (equal "$ echo hello, world"
                   (mistty-test-content))))))

(ert-deftest mistty-test-multiple-replace-at-prompt ()
  (mistty-with-test-buffer ()
    (mistty-test-set-ps1 "cmd-cmd-cmd> ")

    (mistty-send-text "echo cmd")

    ;; Replace "cmd" with "command". There's "cmd" in the scrollback
    ;; area, at prompt and in the prompt. Replacements at prompt get
    ;; reverted, replacement in the prompt gets replayed. There's
    ;; only one hard-timeout, for the first cmd at prompt, after
    ;; that replay remembers the limit.
    (let ((mistty-expected-issues '(hard-timeout)))
      (mistty-run-command
       (goto-char (point-min))
       (while (search-forward "cmd" nil t)
         (replace-match "command" nil t))
       (mistty-test-goto "echo command")))

    (should
     (equal
      (concat "$ PS1='command-command-command> '\n"
              "cmd-cmd-cmd> echo command")
      (mistty-test-content)))))

(ert-deftest mistty-reset-during-replay ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo -n 'read> '; read l; printf 'will reset\\ecreset done\\n'")
    (mistty-send-and-wait-for-prompt nil "read> ")
    (let ((start (mistty--bol (point))))
      (mistty--enqueue
       mistty--queue
       (let (enter-f bar-f)
         (mistty--interact test (interact)
           (setq enter-f
                 (lambda ()
                   (mistty--interact-return
                    interact "\C-m"
                    :wait-until (lambda ()
                                  (mistty-test-find-p "reset done" start))
                    :then bar-f)))

           (setq bar-f
                 (lambda ()
                   (mistty--interact-return
                    interact "bar" :then #'mistty--interact-done)))

           ;; start test interaction
           (mistty--interact-return
            interact "hello"
            :wait-until (lambda ()
                          (mistty-test-find-p "hello" start))
            :then enter-f))))
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

(ert-deftest mistty-error-in-interaction ()
  (let ((debug-on-error nil))
    (mistty-with-test-buffer ()
      (mistty--enqueue
       mistty--queue
       (mistty--interact test (interact)
         (mistty-log "foo-f")
         (mistty--interact-return
          interact "foo"
          :then
          (lambda (val)
            (mistty-log "error-f %s" val)
            (error "fake")))))
    ;; mistty-queue.el should discard the failed interaction and move
    ;; on to the next one.
    (mistty--enqueue-str mistty--queue "bar")
    (mistty-wait-for-output :str "foobar" :start (point-min)))))

(ert-deftest mistty-test-stuck-interaction ()
  (mistty-with-test-buffer ()
    (mistty--enqueue mistty--queue (mistty--stuck-interaction "ok."))
    (mistty--enqueue-str mistty--queue "done")
    (should (not (mistty--queue-empty-p mistty--queue)))
    (mistty--send-string mistty-proc "ok.")
    (mistty-wait-for-output :str "ok.done" :start (point-min))
    (should (mistty--queue-empty-p mistty--queue))))

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

(ert-deftest mistty-test-end-prompt-nobracketed-paste ()
  (mistty-with-test-buffer (:shell bash)
    (mistty-test-nobracketed-paste)
    (mistty-send-text "echo 'hello, world'")
    (mistty-send-and-wait-for-prompt)
    (should (equal
             (concat
              "$ echo 'hello, world'\n"
              "<>hello, world\n"
              "$")
             (mistty-test-content
              :show mistty-sync-marker)))))

(ert-deftest mistty-test-fish-multiline ()
  (mistty-with-test-buffer (:shell fish)
    (should mistty-bracketed-paste)
    (mistty-send-text "echo 'hello\nworld\nand all that sort of things.'")

    (mistty-run-command
     (mistty-test-goto "sort")
     (insert ":::1"))
    (mistty-wait-for-output :str ":::1" :start (point-min))
    (should (equal "$ echo 'hello\n  world\n  and all that :::1sort of things.'"
                   (mistty-test-content)))

    (mistty-run-command
     (mistty-test-goto "llo")
     (insert ":::2"))
    (mistty-wait-for-output :str ":::2" :start (point-min))
    (should (equal "$ echo 'he:::2llo\n  world\n  and all that :::1sort of things.'"
                   (mistty-test-content)))

    (mistty-run-command
     (mistty-test-goto "rld")
     (insert ":::3"))
    (mistty-wait-for-output :str ":::3" :start (point-min))
    (should (equal "$ echo 'he:::2llo\n  wo:::3rld\n  and all that :::1sort of things.'"
                   (mistty-test-content)))))

(ert-deftest mistty-test-fish-multiline-indented ()
  (mistty-with-test-buffer (:shell fish)
    (should mistty-bracketed-paste)
    (mistty-send-text "while i in (seq 10)\necho line $i\nend")

    (mistty-run-command
     (mistty-test-goto "line")
     (insert ":::"))
    (mistty-wait-for-output :str ":::" :start (point-min))
    (should (equal "$ while i in (seq 10)\n      echo :::line $i\n  end"
                   (mistty-test-content)))))

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

(ert-deftest mistty-test-cursor-skip-hook-go-right ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\necho line $i\nend")

    ;; go right from "for" to "end"
    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))
      (mistty-test-goto-after "a b c")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c<>\n"
                             "      echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))
      (right-char)
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c\n"
                             "      <>echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))
      (mistty-test-goto-after "$i")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (right-char)
      (set-window-point win (point))
      (mistty--cursor-skip win)

      (should (equal (concat "$ for i in a b c\n"
                             "      echo line $i\n"
                             "  <>end")
                     (mistty-test-content
                      :show (window-point)))))))

(ert-deftest mistty-test-cursor-skip-hook-go-left ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\necho line $i\nend")

    ;; go left from "end" to "for"
    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))
      (mistty-test-goto "end")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c\n"
                             "      echo line $i\n"
                             "  <>end")
                     (mistty-test-content
                      :show (window-point))))

      (left-char)
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c\n"
                             "      echo line $i<>\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))

      (mistty-test-goto "echo")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c\n"
                             "      <>echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))

      (left-char)
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c<>\n"
                             "      echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point)))))))

(ert-deftest mistty-test-cursor-skip-hook-go-down ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\necho line $i\nend")

    ;; go down from "for" to "end"
    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))

      (mistty-test-goto "for")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ <>for i in a b c\n"
                             "      echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))

      (vertical-motion 1 win)
      (set-window-point win (point))
      (mistty--cursor-skip win)

      (should (equal (concat "$ for i in a b c\n"
                             "      <>echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))

      (vertical-motion 1 win)
      (set-window-point win (point))
      (mistty--cursor-skip win)

      (should (equal (concat "$ for i in a b c\n"
                             "      echo line $i\n"
                             "  <>end")
                     (mistty-test-content
                      :show (window-point)))))))

(ert-deftest mistty-test-cursor-skip-hook-go-up ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\necho line $i\nend")

    ;; go up from "end" to "for"
    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))
      (mistty-test-goto "end")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c\n"
                             "      echo line $i\n"
                             "  <>end")
                     (mistty-test-content
                      :show (window-point))))

      (vertical-motion -1 win)
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in a b c\n"
                             "      <>echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point))))

      (vertical-motion -1 win)
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (should (equal (concat "<>$ for i in a b c\n"
                             "      echo line $i\n"
                             "  end")
                     (mistty-test-content
                      :show (window-point)))))))

(ert-deftest mistty-test-cursor-skip-hook-not-on-a-prompt ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\necho line $i\nend")
    (mistty-send-and-wait-for-prompt)
    (mistty-wait-for-output :str "line c" :start (point-min))

    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))
      (mistty-test-goto "echo")
      (set-window-point win (point))
      (mistty--cursor-skip win)
      (left-char)
      (mistty--cursor-skip win)

      (should (equal (concat "$ for i in a b c\n"
                             "     <> echo line $i\n"
                             "  end\n"
                             "line a\n"
                             "line b\n"
                             "line c\n"
                             "$")
                     (mistty-test-content
                      :show (window-point)))))))

(ert-deftest mistty-test-yank-handler ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 20 20)

    (mistty-run-command
     (insert "echo one two three four five six seven eight nine"))
    (mistty-wait-for-output :str "nine" :cursor-at-end t)

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

(ert-deftest mistty-test-fish-right-prompt-simple-command ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-setup-fish-right-prompt)

    ;; Make sure the right prompt doesn't interfere with normal operations
    (mistty-send-text "echo hello")
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-fish-right-prompt-skip-empty-spaces ()
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

(ert-deftest mistty-test-fish-right-prompt-insert-newlines ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-setup-fish-right-prompt)

    ;; This test makes sure that there's no timeout here, as right
    ;; prompts used to cause issues when detecting text with newlines
    ;; that was just replayed.
    (mistty-run-command
     (insert "for i in a b c\necho $i\nend"))
    (mistty-wait-for-output :str "end")
    (should (string-match (concat "^\\$ for i in a b c +< right\n"
                                  " *echo \\$i\n"
                                  " *end$")
                          (mistty-test-content)))))

(ert-deftest mistty-test-fish-right-prompt-reconcile ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-setup-fish-right-prompt)

    (mistty-test-pre-command)
    (insert "echo hello\necho world")

    ;; There shouldn't be a right prompt after world or even spaces
    ;; after hello.
    (should (equal "$ echo hello\necho world<>\n"
                   (mistty-test-content :show (point) :trim nil)))
    (mistty-test-after-command)

    ;; The shell has put the right prompt back at the right position.
    (should (string-match "^\\$ echo hello +< right\n  echo world<>"
                          (mistty-test-content :show (point))))))

(ert-deftest mistty-test-fish-multiline-dont-skip-empty-lines-forward ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in (seq 10)\n\necho first\n\n\nend")
    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))

      (mistty-test-goto-after "(seq 10)")
      (mistty--cursor-skip win)
      (right-char)
      (mistty--cursor-skip win)
      (should (string-match
               (concat "\\$ for i in (seq 10)\n"
                       " *<>\n"
                       "      echo first\n"
                       "\n"
                       "\n"
                       "  end")
               (mistty-test-content
                :show (point))))

      (mistty-test-goto-after "first")
      (mistty--cursor-skip win)
      (right-char)
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in (seq 10)\n"
                             "\n"
                             "      echo first\n"
                             "      <>\n"
                             "\n"
                             "  end")
                     (mistty-test-content
                      :show (point))))

      (right-char)
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in (seq 10)\n"
                             "\n"
                             "      echo first\n"
                             "\n"
                             "      <>\n"
                             "  end")
                     (mistty-test-content
                      :show (point))))

      (right-char)
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in (seq 10)\n"
                             "\n"
                             "      echo first\n"
                             "\n"
                             "\n"
                             "  <>end")
                     (mistty-test-content
                      :show (point)))))))

(ert-deftest mistty-test-fish-multiline-dont-skip-empty-lines-backward ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in (seq 10)\necho first\n\n\nend")
    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))

      (mistty-test-goto "end")
      (mistty--cursor-skip win)
      (left-char)
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in (seq 10)\n"
                             "      echo first\n"
                             "\n"
                             "      <>\n"
                             "  end")
                     (mistty-test-content
                      :show (point))))

      (left-char)
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in (seq 10)\n"
                             "      echo first\n"
                             "      <>\n"
                             "\n"
                             "  end")
                     (mistty-test-content
                      :show (point))))

      (left-char)
      (mistty--cursor-skip win)
      (should (equal (concat "$ for i in (seq 10)\n"
                             "      echo first<>\n"
                             "\n"
                             "\n"
                             "  end")
                     (mistty-test-content
                      :show (point)))))))

(ert-deftest mistty-test-fish-right-prompt-yank ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-setup-fish-right-prompt)
    (mistty-send-text "echo hello")
    (copy-region-as-kill (mistty--bol (point)) (mistty--eol (point)))
    (with-temp-buffer
      (yank)
      (should (equal "$ echo hello" (mistty-test-content))))))

(ert-deftest mistty-test-not-right-prompt-yank-in-output ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-send-string "printf 'foo\\t") ; \t would confuse mistty-send-text
    (mistty-send-text "bar\n'")
    (let ((start (point)))
      (mistty-send-and-wait-for-prompt)
      (copy-region-as-kill (mistty--bol start 2) (mistty--eol start 2)))
    (with-temp-buffer
      (yank)
      (should (equal "foo     bar" (mistty-test-content))))))

(ert-deftest mistty-test-fish-right-prompt-skip-empty-spaces-at-prompt ()
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

(ert-deftest mistty-test-fish-right-prompt-delete-whole-line ()
  (mistty-with-test-buffer (:shell fish)
    (mistty-setup-fish-right-prompt)

    (mistty-send-text "echo hello")
    (mistty-run-command
     (mistty-beginning-of-line))
    (should (string-match "^\$ <>echo hello +< right$" (mistty-test-content :show (mistty-cursor))))
    (mistty-run-command
     (kill-line))
    (should (string-match "^\$ <> +< right$" (mistty-test-content :show (mistty-cursor))))))

(ert-deftest mistty-test-vertical-distance ()
  (ert-with-test-buffer ()
    (insert "echo one tw\n"
            "o three fou\n"
            "r five six \n"
            "seven eight\n"
            "nine")
    (should (equal 0 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "echo"))))
    (should (equal 1 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "three"))))
    (should (equal 2 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "five"))))
    (should (equal 3 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "seven"))))
    (should (equal 4 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "nine"))))
    (should (equal -4 (mistty--vertical-distance (mistty-test-pos "nine")
                                                 (mistty-test-pos "one"))))))

(ert-deftest mistty-test-vertical-distance-fake-nl ()
  (ert-with-test-buffer ()
    (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
    (insert "echo one tw" fake-nl "o three four\n"
            "five six se" fake-nl "ven eight\n"
            "nine")
    (should (equal 0 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "four"))))
    (should (equal 1 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "six"))))
    (should (equal 1 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "eight"))))
    (should (equal 2 (mistty--vertical-distance (mistty-test-pos "one")
                                                (mistty-test-pos "nine"))))
    (should (equal -2 (mistty--vertical-distance (mistty-test-pos "nine")
                                                 (mistty-test-pos "one")))))))

(ert-deftest mistty-test-distance-with-fake-nl ()
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

(ert-deftest mistty-test-distance-skipped-spaces ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\necho line $i\nend")

    (should (equal 1 (mistty--distance (mistty-test-pos-after "a b c")
                                       (mistty-test-pos "echo"))))
    (should (equal -1 (mistty--distance (mistty-test-pos "echo")
                                        (mistty-test-pos-after "a b c"))))

    (should (equal 10 (mistty--distance (mistty-test-pos "a b c")
                                        (mistty-test-pos-after "echo"))))

    (should (equal 0 (mistty--distance
                      (- (mistty-test-pos "end") 2)
                      (mistty-test-pos "end"))))

    (should (equal 0 (mistty--distance (mistty-test-pos-after "a b c")
                                       (1- (mistty-test-pos "echo")))))

    (should (equal 0 (mistty--distance (1+ (mistty-test-pos-after "a b c"))
                                       (mistty-test-pos "echo"))))

    (should (equal 28 (mistty--distance (mistty-test-pos "for")
                                        (mistty-test-pos "end"))))))

(ert-deftest mistty-test-distance-empty-lines ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\n\n\nend")

    (should (equal 3 (mistty--distance (mistty-test-pos-after "a b c")
                                       (mistty-test-pos "end"))))

    (should (equal -3 (mistty--distance (mistty-test-pos "end")
                                        (mistty-test-pos-after "a b c"))))))

(ert-deftest mistty-test-distance-empty-lines-unreachable-beg-or-end ()
  (mistty-with-test-buffer (:shell fish :selected t)
    (mistty-send-text "for i in a b c\n\necho hello\n\nend")

    (let ((mistty-skip-empty-spaces t)
          (win (selected-window)))

      (mistty-run-command
       (mistty-test-goto-after "a b c")
       (mistty--cursor-skip win))

      (should (equal (concat "$ for i in a b c<>\n"
                             "\n"
                             "      echo hello\n"
                             "\n"
                             "  end")
                     (mistty-test-content :show (mistty-cursor))))

      (should (equal 2 (mistty--distance
                        (mistty-cursor) (mistty-test-pos "echo"))))
      (should (equal 1 (mistty--distance
                        (1+ (mistty-cursor)) (mistty-test-pos "echo"))))

      (mistty-run-command
       (right-char)
       (mistty--cursor-skip win))

      (should (equal (concat "$ for i in a b c\n"
                             "  <>\n"
                             "      echo hello\n"
                             "\n"
                             "  end")
                     (mistty-test-content :show (mistty-cursor))))

      (should (equal 1 (mistty--distance
                        (mistty-cursor) (mistty-test-pos "echo"))))
      (should (equal 1 (mistty--distance
                        (- (mistty-cursor) 1) (mistty-test-pos "echo"))))
      (should (equal 1 (mistty--distance
                        (- (mistty-cursor) 2) (mistty-test-pos "echo")))))))

(ert-deftest mistty-test-quit ()
  (mistty-with-test-buffer ()
    (let ((killed nil))
      (mistty--enqueue
       mistty--queue
       (mistty--interact test (interact)
         (setf (mistty--interact-cleanup interact)
               (lambda () (setq killed t)))
         (mistty--interact-return interact ".")))

      (should (not (mistty--queue-empty-p mistty--queue)))

      ;; C-g
      (let ((this-command 'keyboard-quit))
        (mistty--pre-command)
        (mistty--post-command))

      (should (mistty--queue-empty-p mistty--queue))
      (should killed))))

(ert-deftest mistty-test-forbid-edit ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)
      (should (equal " FE:run" mode-line-process))

      ;; following the cursor is disabled
      (let ((cursor (mistty-cursor)))
        (mistty-run-command
         (right-char))
        (should (equal cursor (mistty-cursor)))
        (should (not (equal (point) (mistty-cursor)))))

      ;; leave the mode
      (mistty-send-command)

      (mistty-wait-for-output
       :test (lambda ()
               (save-excursion
                 (goto-char (point-min))
                 (not (search-forward "search:" nil t)))))

      (should (not mistty--forbid-edit))
      (should (equal ":run" mode-line-process)))))

(ert-deftest mistty-test-exit-forbid-edit ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      ;; C-g
      (let ((this-command 'keyboard-quit))
        (mistty--pre-command)
        (mistty--post-command))

      (mistty-wait-for-output
       :test (lambda ()
               (save-excursion
                 (goto-char (point-min))
                 (not (search-forward "search:" nil t)))))

      (should (not mistty--forbid-edit)))))

(ert-deftest mistty-test-forbid-edit-map ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish :selected t)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo third")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))

      (should mistty--forbid-edit)
      (execute-kbd-macro (kbd "e c h"))
      (mistty-wait-for-output
       :test (lambda ()
               (save-excursion)
               (goto-char (point-min))
               (search-forward "search: ech" nil 'noerror)))

      (should (equal (concat
                      "search: ech<>\n"
                      "► echo third  ► echo second  ► echo first")
                     (mistty-test-content
                      :start (mistty-test-pos "search:") :show (point))))

      ;; "echo third", the first option is selected by default.
      ;; select the second option (echo second) and accept it.
      (execute-kbd-macro (kbd "<right>"))
      (mistty-wait-for-output
       :test (lambda ()
               (save-excursion
                 (goto-char (point-min))
                 (search-forward-regexp "\\$ echo second *\nsearch: " nil t))))

      (execute-kbd-macro (kbd "RET"))
      (mistty-wait-for-output
       :test (lambda ()
               (save-excursion
                 (goto-char (point-min))
                 (not (search-forward "search:" nil t)))))

      (should (equal "$ echo second<>"
                     (mistty-test-content
                      :start mistty-sync-marker :show (point)))))))

(ert-deftest mistty-test-forbid-edit-insert ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      (mistty-send-text "echo")

      ;; replay insertion at cursor
      (mistty-run-command
       (goto-char (mistty-cursor))
       (insert " se"))

      (should (equal (concat "$ echo first\n"
                             "first\n"
                             "$ echo second\n"
                             "second\n"
                             "$ echo second\n"
                             "search: echo se<>\n"
                             "► echo second")
                     (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-forbid-edit-ignore-insert-after-cursor ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      (mistty-send-text "echo")

      ;; ignore insertion after cursor
      (mistty-run-command
       (goto-char (1+ (mistty-cursor)))
       (insert "se"))

      (mistty-send-text " fi")

      (should (equal (concat "$ echo first\n"
                             "first\n"
                             "$ echo second\n"
                             "second\n"
                             "$ echo first\n"
                             "search: echo fi<>\n"
                             "► echo first")
                     (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-forbid-edit-ignore-insert-before-cursor ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))

      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      (mistty-send-text "echo")

      ;; insertion before cursor are just appended
      (mistty-run-command
       (goto-char (pos-bol))
       (insert "more"))

      (should (equal (concat "$ echo first\n"
                             "first\n"
                             "$ echo second\n"
                             "second\n"
                             "$\n"
                             "search: echomore<>\n"
                             "(no matches)")
                     (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-forbid-edit-delete ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      (mistty-send-text "echo sec")

      ;; replay deletion
      (mistty-run-command
       (backward-kill-word 1))

      (should (equal (concat "$ echo first\n"
                             "first\n"
                             "$ echo second\n"
                             "second\n"
                             "$ echo second\n"
                             "search: echo <>\n"
                             "► echo second  ► echo first")
                     (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-forbid-edit-ignore-delete-after-cursor ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      (mistty-send-text "echo")

      ;; ignore deletion after cursor
      (mistty-run-command
       (delete-region (mistty-test-pos-after "►") (point-max)))

      (mistty-send-text " se")

      (should (equal (concat "$ echo first\n"
                             "first\n"
                             "$ echo second\n"
                             "second\n"
                             "$ echo second\n"
                             "search: echo se\n"
                             "► echo second")
                     (mistty-test-content))))))

(ert-deftest mistty-test-forbid-edit-ignore-delete-before-cursor ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo first")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo second")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-r"))
      (mistty-wait-for-output :str "search:" :start (point-min))
      (should mistty--forbid-edit)

      (mistty-send-text "echo")

      (mistty-run-command
       (delete-region (pos-bol) (+ 4 (pos-bol))))

      (mistty-send-text " se")

      (should (equal (concat "$ echo first\n"
                             "first\n"
                             "$ echo second\n"
                             "second\n"
                             "$ echo second\n"
                             "search: echo se\n"
                             "► echo second")
                     (mistty-test-content))))))

(ert-deftest mistty-test-forbid-edit-search-in-completion ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    (mistty-with-test-buffer (:shell fish)
      (dotimes (i 4)
        (mistty-send-text (format "function foobar_%s; echo foobar %s; end" i i))
        (mistty-send-and-wait-for-prompt))
      (mistty-send-text "fooba")
      (let ((start (point)))
        (mistty-run-command
         (mistty-tab-command))
        (mistty-wait-for-output :str "foobar_" :start start)
        (mistty-send-key 1 (kbd "C-s"))
        (mistty-wait-for-output :str "search:" :start (point-min))
        (should mistty--forbid-edit)

        (mistty-send-text "2")

        ;; 1st RET leaves the mode, 2nd executes the command.
        (mistty-send-key 2 (kbd "RET"))

        (mistty-wait-for-output :str "foobar 2" :start start)
        (should-not mistty--forbid-edit)))))

(ert-deftest mistty-test-forbid-wrong-position ()
  (let ((mistty-forbid-edit-regexps '("^search: ")))
    ;; This test tries to confuse MisTTY by having fish
    ;; echo search:.
    (mistty-with-test-buffer (:shell fish)
      (mistty-send-text "echo foobar")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo search:")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo")
      (mistty-run-command
       (mistty-tab-command))
      (mistty-wait-for-output :str "search:")
      (should-not mistty--forbid-edit))))

(ert-deftest mistty-test-eof-on-possible-but-outdated-prompt ()
  (mistty-with-test-buffer ()
    (mistty-send-text "p=prompt")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "printf \"output;\\n\\$ non-$p\\ncontinue\"; read; echo .")
    (let ((start (1+ (mistty-cursor))))
      (mistty-send-and-wait-for-prompt)

      (mistty-test-goto "non-prompt")
      (mistty-beginning-of-line)
      (mistty-end-of-line-or-goto-cursor)
      (mistty-send-and-wait-for-prompt)

      (should (equal
               (concat
                "[output;\n"
                "$ non-prompt\n"
                "continue\n"
                ".\n"
                "]$")
             (mistty-test-content
              :start start
              :show-property '(mistty-input-id nil)))))))

(ert-deftest mistty-test-kill-long-line ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 20 20)

    (mistty-run-command
     (insert "echo one two three four five six seven eight nine"))
    (mistty-wait-for-output :str "nine" :cursor-at-end t)

    (mistty-run-command
     (mistty-test-goto "one"))
    (mistty-run-command
     (kill-line))
    (should (equal (concat "$ echo <>")
                   (mistty-test-content :show (point))))))


(ert-deftest mistty-test-undo-insert ()
  (mistty-with-test-buffer ()
    (setq buffer-undo-list nil)
    (mistty-run-command
     (mistty-send-key 1 "e"))
    (mistty-run-command
     (mistty-send-key 1 "c"))
    (mistty-run-command
     (mistty-send-key 1 "h"))
    (mistty-run-command
     (mistty-send-key 1 "o"))
    (mistty-wait-for-output :str "echo" :start (point-min))
    (mistty-run-command (undo))
    (mistty-wait-for-output
     :test
     (lambda ()
       (equal "$" (mistty-test-content))))))

(ert-deftest mistty-test-undo-backward-delete ()
  (mistty-with-test-buffer ()
    (setq buffer-undo-list nil)
    (mistty-send-text "echo test")
    (mistty-run-command
     (mistty-backward-delete-char 4))
    (mistty-wait-for-output
     :test
     (lambda ()
       (equal "$ echo" (mistty-test-content))))
    (mistty-run-command (undo))
    (mistty-wait-for-output :str "echo test" :start (point-min))))

(ert-deftest mistty-test-undo-delete ()
  (mistty-with-test-buffer ()
    (setq buffer-undo-list nil)
    (mistty-send-text "echo hello world")
    (mistty-run-command
     (mistty-test-goto "world"))
    (mistty-run-command
     (mistty-delete-char 2))
    (mistty-wait-for-output :str "hello rld" :start (point-min))
    (mistty-run-command (undo))
    (mistty-wait-for-output :str "hello world" :start (point-min))))

(ert-deftest mistty-test-undo-delete-eof ()
  (mistty-with-test-buffer ()
    (setq buffer-undo-list nil)
    (mistty-send-text "echo hello world")
    (mistty-run-command
     (mistty-test-goto "world"))
    (mistty-run-command
     (mistty-delete-char 100)) ;; deletes past EOF
    (mistty-wait-for-output :regexp "hello $" :start (point-min))
    (mistty-run-command (undo))
    (mistty-wait-for-output :str "hello world" :start (point-min))))

(ert-deftest mistty-test-undo-multiple-types ()
  (mistty-with-test-buffer ()
  (let ((mistty-expected-issues '(hard-timeout)))
    (mistty-send-text "echo hello world.")
    ;; replayed command
    (mistty-run-command
     (mistty-test-goto-after "world")
     (backward-kill-word 1))

    ;; insert
    (mistty-run-command
     (mistty-self-insert 1 ?f))
    (mistty-run-command
     (mistty-self-insert 3 ?o))

    ;; delete
    (mistty-run-command
     (mistty-backward-delete-char))
    (mistty-wait-for-output :str "echo hello foo." :start (point-min))

    ;; undo delete
    (mistty-run-command
     (let ((this-command 'undo))
       (undo)))
    (mistty-wait-for-output :str "echo hello fooo." :start (point-min))

    ;; undo insert
    (mistty-run-command
     (let ((this-command 'undo)
           (last-command 'undo))
       (undo)))
    (mistty-wait-for-output :str "echo hello ." :start (point-min))

    ;; undo replayed insert
    (mistty-run-command
     (let ((this-command 'undo)
           (last-command 'undo))
       (undo)))
    (mistty-wait-for-output :str "echo hello world." :start (point-min))

    ;; flakiness in the position might indicate that the building of
    ;; the position on the last undo sequence isn't correct, as we
    ;; can't necessarily rely on (mistty-cursor) being up-to-date when
    ;; inserting keys quickly.
    )))

(ert-deftest mistty-test-sudo ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo ok")
    (mistty-run-command
     (call-interactively 'mistty-sudo))
    (should (equal "$ sudo echo ok<>" (mistty-test-content :show (point))))))

(ert-deftest mistty-test-sudo-in-scrollback ()
  (mistty-with-test-buffer ()
    (mistty-simulate-scrollback-buffer
     (should-error (call-interactively 'mistty-sudo)))))

(ert-deftest mistty-test-fullscreen-message ()
  (let ((mistty-mode-map (make-sparse-keymap))
        (mistty-fullscreen-map (make-sparse-keymap)))
    (should (equal "Fullscreen mode ON" (mistty--fullscreen-message)))

    (keymap-set mistty-mode-map "C-c C-a" #'mistty-toggle-buffers)
    (keymap-set mistty-fullscreen-map "C-c C-a" #'mistty-toggle-buffers)

    (should (equal
             "Fullscreen mode ON. C-c C-a switches between terminal and scrollback buffers."
             (mistty--fullscreen-message)))

    (keymap-set mistty-fullscreen-map "C-c C-b" #'mistty-toggle-buffers)
    (should (equal
             "Fullscreen mode ON. C-c C-a goes to terminal, C-c C-b to scrollback."
             (mistty--fullscreen-message)))))

(ert-deftest mistty-test-create-buffer-with-prev-output ()
  (mistty-with-test-buffer ()
    (mistty-send-text "printf '#!/bin/bash\\necho ok\\n'")
    (mistty-send-and-wait-for-prompt)
    (let ((newbuf (ert-simulate-keys
                   (kbd "RET")
                   (call-interactively 'mistty-create-buffer-with-output))))
      (unwind-protect
          (with-current-buffer newbuf
            (should (equal "#!/bin/bash\necho ok\n"
                           (buffer-substring-no-properties (point-min) (point-max))))
            (should (equal "printf '#!/bin/bash\\necho ok\\n..."
                           (buffer-name)))
            ;; mode should be recognized thanks to the shebang
            (should (equal 'sh-mode major-mode)))
        (kill-buffer newbuf)))))

(ert-deftest mistty-test-create-buffer-with-prev-n-output ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo world")
    (mistty-send-and-wait-for-prompt)
    (let* ((current-prefix-arg 2)
           (newbuf (ert-simulate-keys
                   (kbd "RET")
                   (call-interactively 'mistty-create-buffer-with-output))))
      (unwind-protect
          (with-current-buffer newbuf
            (should (equal "hello\n"
                           (buffer-substring-no-properties (point-min) (point-max))))
            (should (equal "echo hello"
                           (buffer-name))))
        (kill-buffer newbuf)))))

(ert-deftest mistty-test-create-buffer-with-prev-output-multiline ()
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "for i in hello world; do\n echo $i\n done"))
    (mistty-send-and-wait-for-prompt)
    (let ((newbuf (ert-simulate-keys
                   (kbd "RET")
                   (call-interactively 'mistty-create-buffer-with-output))))
      (unwind-protect
          (with-current-buffer newbuf
            (should (equal "hello\nworld\n"
                           (buffer-substring-no-properties (point-min) (point-max))))
            ;; Extracting the command line failed
            (should (equal "command output" (buffer-name))))
        (kill-buffer newbuf)))))

(ert-deftest mistty-test-create-buffer-with-current-output ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo content")
    (mistty-send-and-wait-for-prompt)
    (mistty-test-goto "echo content")
    (forward-line)
    (let ((newbuf (ert-simulate-keys
                   (kbd "RET")
                   (call-interactively 'mistty-create-buffer-with-output))))
      (unwind-protect
          (with-current-buffer newbuf
            (should (equal "content" (mistty-test-content)))
            (should (equal "echo content" (buffer-name))))
        (kill-buffer newbuf)))))

(ert-deftest mistty-test-create ()
  (let ((buf (mistty-create mistty-test-bash-exe)))
    (unwind-protect
        (with-current-buffer buf
          (should (equal 'mistty-mode major-mode))
          (should (equal buf mistty-work-buffer))
          (should (process-live-p mistty-proc)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-create-with-prefix-arg ()
  (let ((buf (let* ((default-directory (getenv "HOME"))
                    (mistty-shell-command mistty-test-bash-exe)
                    (current-prefix-arg 1)
                    (this-command 'mistty-create)
                    (read-file-name-function #'read-file-name-default))
             (ert-simulate-keys '(?/ ?/ ?\C-j) (mistty-create)))))
    (unwind-protect
        (with-current-buffer buf
          (should (equal 'mistty-mode major-mode))
          (should (equal "/" default-directory)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-create-other-window-with-prefix-arg ()
  (let ((buf (let* ((default-directory (getenv "HOME"))
                    (mistty-shell-command mistty-test-bash-exe)
                    (current-prefix-arg 1)
                    (this-command 'mistty-create-other-window)
                    (read-file-name-function #'read-file-name-default))
             (ert-simulate-keys '(?/ ?/ ?\C-j) (mistty-create-other-window)))))
    (unwind-protect
        (with-current-buffer buf
          (should (equal 'mistty-mode major-mode))
          (should (equal "/" default-directory)))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-create-command-with-args ()
  (let ((buf (mistty-create
              (list mistty-test-bash-exe "--norc" "--noprofile" "--restricted"))))
    (unwind-protect
        (with-current-buffer buf
          ;; The normal setup hasn't been done, so we can't rely on
          ;; the usual helpers.
          (mistty--send-string mistty-proc "cd /\n")
          (mistty-wait-for-output
           :test (lambda ()
                   (string-match ".*bash: cd: restricted.*" (mistty-test-content)))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-create-mistty-shell-command ()
  (let* ((mistty-shell-command
          (list mistty-test-bash-exe "--norc" "--noprofile" "--restricted"))
         (buf (mistty-create)))
    (unwind-protect
        (with-current-buffer buf
          ;; The normal setup hasn't been done, so we can't rely on
          ;; the usual helpers.
          (mistty--send-string mistty-proc "cd /\n")
          (mistty-wait-for-output
           :test (lambda ()
                   (string-match ".*bash: cd: restricted.*" (mistty-test-content)))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-cycle-and-create ()
  (let ((mistty-shell-command mistty-test-bash-exe)
        (start-buf (current-buffer))
        (first-buf nil)
        (second-buf nil)
        (third-buf nil)
        (last-command nil)
        (this-command nil)
        (current-prefix-arg nil)
        (bufs nil))
    (unwind-protect
        (let ((call-mistty (lambda ()
                             (setq this-command 'mistty)
                             (let ((buf (mistty nil (lambda (buf) (memq buf bufs)))))
                               (cl-pushnew buf bufs)
                               (setq last-command this-command)
                               buf))))

          ;; first call creates a new buffer
          (setq first-buf (funcall call-mistty))
          (should (buffer-live-p first-buf))
          (with-current-buffer first-buf
            (should (equal 'mistty-mode major-mode)))

          ;; second call goes back to that buffer
          (pop-to-buffer start-buf)
          (should (progn
                    "create first-buf"
                    (equal first-buf (funcall call-mistty))))

          ;; another call creates a second mistty buffer
          (setq second-buf (funcall call-mistty))
          (should (progn
                    "create second-buf"
                    (buffer-live-p second-buf)))
          (should-not (equal first-buf second-buf))
          (with-current-buffer second-buf
            (should (equal 'mistty-mode major-mode)))

          ;; some time later
          (pop-to-buffer start-buf)
          (setq last-command nil)
          (should (progn
                    "first call goes back to 1st buffer"
                    (equal first-buf (funcall call-mistty))))
          (should (progn
                    "second call goes back to 2nd buffer"
                    (equal second-buf (funcall call-mistty))))
          (should (progn
                    "...and back to the 1st"
                    (equal first-buf (funcall call-mistty))))

          ;; calling mistty with an argument creates a 3rd buffer
          (let ((current-prefix-arg 1))
            (setq third-buf (funcall call-mistty)))
          (should (progn
                    "C-u M-x mistty should have create a 3rd buffer"
                    (and (not (eq third-buf first-buf))
                         (not (eq third-buf second-buf)))))
          (with-current-buffer third-buf
            (should (equal 'mistty-mode major-mode)))

          ;; and later on, we cycle through the three buffers again.
          (setq last-command nil)
          (pop-to-buffer start-buf)
          (should (progn
                    "cycle through 3 buffers, first call"
                    (equal first-buf (funcall call-mistty))))
          (should (progn
                    "cycle through 3 buffers, second call"
                    (equal second-buf (funcall call-mistty))))
          (should (progn
                    "cycle through 3 buffers, third call"
                    (equal third-buf (funcall call-mistty))))
          (should (progn
                    "cycle through 3 buffers, 4th call"
                    (equal first-buf (funcall call-mistty)))))

      ;; unwind
      (let ((kill-buffer-query-functions nil))
        (dolist (buf bufs)
          (kill-buffer buf))))))

(ert-deftest mistty-test-cycle-and-create-default-directory ()
  (let ((mistty-shell-command mistty-test-bash-exe)
        (home (expand-file-name (getenv "HOME")))
        (first-buf nil)
        (second-buf nil)
        (last-command nil)
        (this-command nil)
        (bufs nil))
    (unwind-protect
        (let ((start-buf (let ((buf (generate-new-buffer "start-buf")))
                           (push buf bufs)
                           buf))
              (call-mistty (lambda ()
                             (setq this-command 'mistty)
                             (let ((buf (mistty nil (lambda (buf) (memq buf bufs)))))
                               (cl-pushnew buf bufs)
                               (setq last-command this-command)
                               buf))))

          ;; start with a buffer in home
          (pop-to-buffer start-buf)
          (should (eq start-buf (current-buffer)))
          (setq default-directory home)

          (setq first-buf (funcall call-mistty))
          (with-current-buffer first-buf
            (should (equal home default-directory)))

          ;; some time later, start-buf is in a different directory
          (pop-to-buffer start-buf)
          (should (eq start-buf (current-buffer)))
          (setq default-directory "/")
          (setq last-command nil)
          (should (progn
                    "first call goes back to 1st buffer"
                    (equal first-buf (funcall call-mistty))))
          (setq second-buf (funcall call-mistty))
          (should-not (progn "second call creates a new buffer"
                             (equal first-buf second-buf)))

          ;; second-buf is in "/", the default-directory of start-buf,
          ;; even though when mistty was called, the current buffer
          ;; was first-buf, whose default-directory is home.
          (with-current-buffer second-buf
            (should (equal "/" default-directory))))

      ;; unwind
      (let ((kill-buffer-query-functions nil))
        (dolist (buf bufs)
          (kill-buffer buf))))))

(ert-deftest mistty-test-mistty-other-window ()
  (let* ((mistty-shell-command mistty-test-bash-exe)
         (buf (mistty-other-window)))
    (unwind-protect
        (with-current-buffer buf
          (should (equal 'mistty-mode major-mode))
          (should (equal buf (window-buffer (selected-window))))
          (should (equal 2 (length (window-list)))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-window-size ()
  (let* ((buf (mistty-create mistty-test-bash-exe)))
    (unwind-protect
        (with-current-buffer buf
          (let ((win (get-buffer-window buf)))
            ;; initial window size
            (should (equal (- (window-max-chars-per-line win) left-margin-width)
                           (buffer-local-value 'term-width mistty-term-buffer)))
            (should (equal (floor (window-screen-lines))
                           (buffer-local-value 'term-height mistty-term-buffer)))
            (split-window-horizontally nil win)
            ;; window size changed
            (mistty--window-size-change win)
            (should (equal (- (window-max-chars-per-line win) left-margin-width)
                           (buffer-local-value 'term-width mistty-term-buffer)))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest mistty-test-detect-foreign-overlays-labelled ()
  (let ((mistty-detect-foreign-overlays t))
    (mistty-with-test-buffer ()
      (let (test-overlay)
        (mistty-send-text "echo hello, world")
        (mistty-run-command
         (mistty-test-goto "hello")
         (setq test-overlay (make-overlay (point) (+ 5 (point))))
         (overlay-put test-overlay 'mistty-long-running-command t))

        (mistty-wait-for-output :test (lambda ()
                                        (equal '(mistty-overlays) mistty--inhibit)))
        (should (mistty-long-running-command-p))))))

(ert-deftest mistty-test-foreign-overlay-detection-disabled ()
  (let ((mistty-detect-foreign-overlays nil))
    (mistty-with-test-buffer ()
      (let (test-overlay)
        (mistty-send-text "echo hello, world")
        (mistty-run-command
         (mistty-test-goto "hello")
         (setq test-overlay (make-overlay (point) (+ 5 (point))))
         (overlay-put test-overlay 'mistty-long-running-command t))

        (should-not (mistty-long-running-command-p))))))

(ert-deftest mistty-test-ignore-unlabelled-foreign-overlays ()
  (let ((mistty-detect-foreign-overlays t))
    (mistty-with-test-buffer ()
      (mistty-send-text "echo hello, world")
      (mistty-run-command
       (mistty-test-goto "hello")
       (make-overlay (point) (+ 5 (point))))

      (should-not mistty--inhibit))))

(ert-deftest mistty-test-ignore-detected-foreign-overlays-after-C-g ()
  (let ((mistty-detect-foreign-overlays t))
    (mistty-with-test-buffer ()
      (let (test-overlay)
        (mistty-send-text "echo hello, world")
        (mistty-run-command
         (mistty-test-goto "hello")
         (setq test-overlay (make-overlay (point) (+ 5 (point))))
         (overlay-put test-overlay 'mistty-long-running-command t))

        ;; A long command was detected.
        (mistty-wait-for-output :test (lambda ()
                                        (equal '(mistty-overlays) mistty--inhibit)))
        (should (mistty-long-running-command-p))

        ;; Simulate C-g
        (let ((this-command 'keyboard-quit))
          (mistty--pre-command)
          (mistty--post-command))

        ;; We're back to normal.
        (mistty-wait-for-output :test (lambda ()
                                        (not mistty--inhibit)))
        (should-not (mistty-long-running-command-p))

        (should (equal "hello, world"
                       (mistty-send-and-capture-command-output)))))))

(ert-deftest mistty-test-autoignore-early-foreign-overlays ()
  (let ((mistty-detect-foreign-overlays t)
        (mistty-mode-hook
         (list (lambda ()
                 (let ((ov (make-overlay (point-min) (point-max))))
                   (overlay-put ov 'mistty-long-running-command t)
                   (mistty-log "MODE HOOK overlay: %s" ov))))))
    (mistty-with-test-buffer ()
      (should-not mistty-detect-foreign-overlays))))

(ert-deftest mistty-test-detect-completion-in-region ()
  (mistty-with-test-buffer ()
    (mistty-send-text "echo hello")
    (setq completion-in-region-mode t)
    (run-hooks 'completion-in-region-mode-hook)
    (should (equal '(mistty-completion-in-region) mistty--inhibit))
    (mistty-run-command
     (insert ","))
    (mistty-run-command
     (insert " world"))
    (should (equal "$ echo hello, world" (mistty-test-content)))
    (setq completion-in-region-mode nil)
    (run-hooks 'completion-in-region-mode-hook)
    (should (equal nil mistty--inhibit))
    (mistty-wait-for-output :str "hello, world")))

(ert-deftest mistty-test-report-long-running-command ()
  (mistty-with-test-buffer ()
    (should-not (mistty-long-running-command-p))
    (should (equal ":run" mode-line-process))

    (mistty-report-long-running-command 'test t)
    (should (mistty-long-running-command-p))
    (should (equal " CMD:run" mode-line-process))

    (insert "echo hello, world")
    (mistty-report-long-running-command 'test nil)
    (should-not (mistty-long-running-command-p))
    (should (equal ":run" mode-line-process))

    ;; the change made during the long-running command have been
    ;; replayed.
    (mistty-wait-for-output :str "hello, world")))

(ert-deftest mistty-test-ignore-long-running-command ()
  (mistty-with-test-buffer ()
    (mistty-report-long-running-command 'test-1 t)
    (should (mistty-long-running-command-p))
    (mistty-report-long-running-command 'test-2 t)

    (insert "echo hello, world")

    (mistty-ignore-long-running-command)
    (should-not (mistty-long-running-command-p))

    ;; the change made during the long-running command have been
    ;; replayed.
    (mistty-wait-for-output :str "hello, world")))

(ert-deftest mistty-test-replays-are-queued ()
  (mistty-with-test-buffer ()
    (let ((unfreeze (mistty-test-freeze-queue)))
      ;; the queue is frozen for now

      ;; put two replays in the queue
      (mistty-run-command-nowait
       (insert "echo hello"))

      (mistty-run-command-nowait
       (insert " world."))

      ;; the work buffer is still frozen, showing a preview.
      (should (equal "$ echo hello world.<>" (mistty-test-content :show (point))))

      ;; restart the queue and wait until it's empty
      (funcall unfreeze)
      (mistty-wait-for-output
       :test (lambda ()
               (mistty--queue-empty-p mistty--queue)))

      (mistty-wait-for-output :str "echo hello world.")
      (should (equal "$ echo hello world.<>" (mistty-test-content :show (point)))))))

(ert-deftest mistty-test-interactive-inserts ()
  (let ((mistty-interactive-insert-hook nil))
    (mistty-with-test-buffer ()
      (let ((start (point))
            (calls))
      (add-hook 'mistty-interactive-insert-hook
                (lambda ()
                  (push (mistty--safe-bufstring start (point))
                        calls)))
      (setq this-command 'mistty-self-insert)
      (mistty-run-command
       (mistty-self-insert 1 ?e))
      (setq last-command 'mistty-self-insert)
      (mistty-run-command
       (mistty-self-insert 1 ?c))
      (mistty-run-command
       (mistty-self-insert 1 ?h))
      (mistty-run-command
       (mistty-self-insert 1 ?o))
      (mistty-run-command
       (mistty-self-insert 1 ?\ ))
      (mistty-run-command
       (mistty-self-insert 1 ?o))
      (mistty-run-command
       (mistty-self-insert 1 ?k))
      (mistty-wait-for-output :str "echo ok")
      (should (length> calls 0))
      (should (equal "echo ok" (car calls)))
      (should (equal "ok" (mistty-send-and-capture-command-output)))))))

(ert-deftest mistty-test-simulate-self-insert-commands ()
  (mistty-with-test-buffer ()
    (let ((mistty-interactive-insert-hook '(mistty-simulate-self-insert-command))
          (mistty-simulate-self-insert-command t)
          pre-calls
          post-calls)
      (add-hook 'pre-command-hook
                (lambda ()
                  (push this-command pre-calls)))
      (add-hook 'post-command-hook
                (lambda ()
                  (push this-command post-calls)))
      (setq this-command 'mistty-self-insert)
      (mistty-run-command
       (mistty-self-insert 1 ?e))
      (setq last-command 'mistty-self-insert)
      (mistty-run-command
       (mistty-self-insert 1 ?c))
      (mistty-run-command
       (mistty-self-insert 1 ?h))
      (mistty-run-command
       (mistty-self-insert 1 ?o))
      (mistty-run-command
       (mistty-self-insert 1 ?\ ))
      (mistty-run-command
       (mistty-self-insert 1 ?o))
      (mistty-run-command
       (mistty-self-insert 1 ?k))
      (mistty-wait-for-output :str "echo ok")
      (should (length> pre-calls 0))
      (should (length> post-calls 0))
      (should (equal 'self-insert-command (car pre-calls)))
      (should (equal 'self-insert-command (car post-calls)))
      (should (= (length post-calls) (length pre-calls)))
      (should (equal "ok" (mistty-send-and-capture-command-output))))))

(ert-deftest mistty-test-toggle-fringe ()
  (mistty-with-test-buffer ()
    (mistty-fringe-mode nil)
    (should mistty-fringe-mode)
    (should (overlay-get mistty--sync-ov 'line-prefix))

    (mistty-fringe-mode -1)
    (should-not mistty-fringe-mode)
    (should-not (overlay-get mistty--sync-ov 'line-prefix))))

(ert-deftest mistty-test-fringe-enabled ()
  (let ((orig-value mistty-fringe-enabled))
    (mistty-with-test-buffer ()
      (customize-set-variable 'mistty-fringe-enabled t)
      (should mistty-fringe-mode)

      (customize-set-variable 'mistty-fringe-enabled nil)
      (should-not mistty-fringe-mode)

      (customize-set-variable 'mistty-fringe-enabled orig-value))))

(ert-deftest mistty-kill-buffer-after-exit ()
  (let ((mistty-after-process-start-hook nil)
        (mistty-after-process-end-hook nil))
    (add-hook 'mistty-after-process-end-hook #'mistty-kill-buffer)
    (mistty-with-test-buffer ()
      (let ((term-proc mistty-proc)
            (term-buffer mistty-term-buffer))
        (mistty-send-text "exit")
        (mistty-send-command)
        (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)))))

(ert-deftest mistty-kill-buffer-keeps-buffer-if-fails ()
  (let ((mistty-after-process-start-hook nil)
        (mistty-after-process-end-hook nil)
        (called nil))
    (add-hook 'mistty-after-process-end-hook #'mistty-kill-buffer)
    (add-hook 'mistty-after-process-end-hook (lambda (_proc) (setq called t)) 100)
    (mistty-with-test-buffer ()
      (mistty-send-text "exit 10")
      (mistty-send-command)
      (mistty-wait-for-output :test (lambda () called)))))

(ert-deftest mistty-kill-buffer-and-window-after-exit ()
  (let ((mistty-after-process-start-hook nil)
        (mistty-after-process-end-hook nil))
    (add-hook 'mistty-after-process-end-hook #'mistty-kill-buffer-and-window)
    (mistty-with-test-buffer (:selected t)
      (let ((win (selected-window)))
        (split-window)
        (let ((term-proc mistty-proc)
              (term-buffer mistty-term-buffer))
          (mistty-send-text "exit")
          (mistty-send-command)
          (mistty-wait-for-term-buffer-and-proc-to-die term-buffer term-proc)
          (should-not (window-live-p win)))))))

(ert-deftest mistty-kill-buffer-and-window-keeps-buffer-if-fails ()
  (let ((mistty-after-process-start-hook nil)
        (mistty-after-process-end-hook nil)
        (called nil))
    (add-hook 'mistty-after-process-end-hook #'mistty-kill-buffer-and-window)
    (add-hook 'mistty-after-process-end-hook (lambda (_proc) (setq called t)) 100)
    (mistty-with-test-buffer ()
      (mistty-send-text "exit 10")
      (mistty-send-command)
      (mistty-wait-for-output :test (lambda () called)))))

(ert-deftest mistty-test-buffer-name ()
  (let ((mistty-buffer-name '("mistty")))
    (should (equal "*mistty*" (mistty-new-buffer-name)))))

(ert-deftest mistty-test-buffer-name-shell ()
  (let ((mistty-buffer-name '(mistty-buffer-name-shell))
        (mistty-shell-command "/usr/local/bin/zsh"))
    (should (equal "*zsh*" (mistty-new-buffer-name))))

  (let ((mistty-buffer-name '(mistty-buffer-name-shell))
        (mistty-shell-command '("/usr/bin/fish" "-i")))
    (should (equal "*fish*" (mistty-new-buffer-name)))))

(ert-deftest mistty-test-set-EMACS ()
  (setenv "EMACS" nil) ;; Sometimes set to run Eldev

  (let ((mistty-set-EMACS nil))
    (mistty-with-test-buffer ()
      (mistty-send-text "echo EMACS=${EMACS}")
      (should (equal "EMACS=" (mistty-send-and-capture-command-output)))))
  (let ((mistty-set-EMACS t))
    (mistty-with-test-buffer ()
      (mistty-send-text "echo EMACS=${EMACS}")
      (should (equal (format "EMACS=%s (term:%s)"
                           emacs-version term-protocol-version)
                     (mistty-send-and-capture-command-output))))))

(ert-deftest mistty-test-change-term-colors ()
  (turtles-ert-test :instance 'mistty)

  (unwind-protect
      (progn
        (load-theme 'modus-vivendi 'no-confirm) ;; dark theme
        (mistty-with-test-buffer ()
          ;; A prompt that sets a color and switches back to the default.
          (mistty--send-string mistty-proc "PS1='\\e[48;5;94m\\e[38;5;15m$ \\e[0m'")
          (mistty-send-and-wait-for-prompt)

          ;; Text should still reference the default face and not
          ;; specific colors.
          (mistty-send-text "echo hello")
          (mistty-send-and-wait-for-prompt)

          (mistty-send-text "echo world")

          (disable-theme 'modus-vivendi)
          (load-theme 'modus-operandi 'no-confirm) ;; light theme

          ;; Allow some time for terminal refresh.
          (accept-process-output mistty-proc 0.1)
          (while (accept-process-output mistty-proc 0))

          ;; We switched from a dark to a light  theme. Make sure the
          ;; background of normal text is light .
          (turtles-with-grab-buffer ()
            (goto-char (point-min))

            ;; scrollback region
            (search-forward "echo hello")
            (goto-char (match-beginning 0))
            (should (equal (color-values (background-color-at-point))
                           (color-values (modus-themes-color 'bg-main))))

            ;; terminal region
            (search-forward "echo hello")
            (goto-char (match-beginning 0))
            (should (equal (color-values (background-color-at-point))
                           (color-values (modus-themes-color 'bg-main)))))))
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))))

(ert-deftest mistty-test-scroll-window-up ()
  (turtles-ert-test :instance 'mistty)

  (mistty-with-test-buffer (:shell fish :selected t)
    (let ((win (selected-window))
          (testbuf (current-buffer))
          (lastline (lambda (&optional buf)
                      (with-current-buffer (or buf (current-buffer))
                        (save-excursion
                          (goto-char (point-max))
                          (buffer-substring (pos-bol) (pos-eol)))))))
      (delete-other-windows)

      ;; Make sure "fi" is at the bottom of the window
      (dotimes (_ (+ 5 (window-height)))
        (mistty-send-text "echo hello")
        (mistty-send-and-wait-for-prompt))
      (mistty-send-text "fi")

      (recenter nil 'redisplay)
      (turtles-with-grab-buffer (:win win)
        (should (equal "$ fi" (funcall lastline))))

      ;; Command completion should display a few lines below the
      ;; prompt. Since we're at the bottom of the window, these
      ;; lines would normally not be visible.
      (mistty-run-command
       (mistty-tab-command))
      (turtles-with-grab-buffer (:win win)
        (goto-char (point-max))
        (should (equal
                 (string-trim (funcall lastline testbuf))
                 (string-trim (funcall lastline))))))))

(ert-deftest mistty-test-keep-active-mark-on-prompt ()
  (mistty-with-test-buffer ()
    (transient-mark-mode 1)

    (mistty-send-text "echo hello, world")
    (mistty-test-goto "hello")

    ;; The terminal moving the cursor should not deactivate the mark.
    (mistty-run-command
     (set-mark (point)))
    (mistty-run-command
     (call-interactively #'mistty-end-of-line-or-goto-cursor))
    (should mark-active)
    (should (equal "hello, world"
                   (buffer-substring-no-properties (mark) (point))))))

(ert-deftest mistty-test-deactivate-mark-on-send ()
  (mistty-with-test-buffer ()
    (transient-mark-mode 1)

    (mistty-send-text "echo hello, world")
    (mistty-run-command
     (mistty-test-goto "hello")
     (set-mark (point)))
    (mistty-send-text "hello,")
    (mistty-send-and-wait-for-prompt #'mistty-send-command)
    (should-not mark-active)))

(ert-deftest mistty-test-sync-autoreset-recovery ()
  (mistty-with-test-buffer ()
    (mistty--set-process-window-size 80 20)

    ;; Make sure that there's more text in the term buffer than the
    ;; window height allows.
    (dotimes (i 20)
      (mistty-send-text (format "echo line %d" i))
      (mistty-send-and-wait-for-prompt))

    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "for i in {0..5}; do")
    (mistty-send-text "\n  echo -n \"$ \"")
    (mistty-send-text "\n  read")
    (mistty-send-text "\n  echo \"line $i\"")
    (mistty-send-text "\ndone ; printf \"\\r\\e[6;AMODIFIED\\r\\e[6;BDONE\\n\"")
    (dotimes (_ 7)
      (mistty-send-and-wait-for-prompt)
      (mistty-send-key 1 (kbd "C-a")))
    (mistty-send-text "echo three")
    (mistty-send-and-wait-for-prompt)
    (should
     (equal
      (concat
       ;; Skipped beginning "line 0 - 39" at the beginning of the
       ;; buffer.
       "$ echo one\n"
       "one\n"
       "$ echo two\n"
       "two\n"
       "$ for i in {0..5}; do\n"
       ">   echo -n \"$ \"\n"
       ">   read\n"
       ">   echo \"line $i\"\n"
       "> done ; printf \"\\r\\e[6;AMODIFIED\\r\\e[6;BDONE\\n\"\n"

       ;; These are the fake prompts generated by the script above
       ;; to fool MisTTY.

       "$ ^A\n"
       "line 0\n"
       "$ ^A\n"
       "line 1\n"
       "$ ^A\n"
       "line 2\n"

       ;; If recovery doesn't work, we'll see repeated content here.
       "MODIFIED\n"
       "line 3\n"
       "$ ^A\n"
       "line 4\n"
       "$ ^A\n"
       "line 5\n"
       ;; Next prompt after recovery
       "DONE\n"
       "$ echo three\n"
       "three\n"
       "$")
      (mistty-test-content
       :start (save-excursion (goto-char (point-min))
                              (mistty-test-pos "$ echo one")))))))

(ert-deftest mistty-test-sync-autoreset-recovery-despite-fakenl-cleanup ()
  (let ((mistty--inhibit-fake-nl-cleanup nil))
    (mistty-with-test-buffer ()
      (mistty--set-process-window-size 20 20)

      ;; Insert text that'll be too long for the window, so term
      ;; inserts fake newlines, which later get cleaned up when the
      ;; zone transitions to scrollback. These changes should not
      ;; throw off recovery.
      (dotimes (i 20)
        (mistty-send-text (format "echo one two three four five six seven eight nine %d" i))
        (mistty-send-and-wait-for-prompt))

      (mistty-send-text "echo one")
      (mistty-send-and-wait-for-prompt)
      (mistty-send-text "echo two")
      (mistty-send-and-wait-for-prompt)

      ;; Using mistty-send-string directly as the lines will get fake
      ;; newlines, which confuses mistty-send-text.
      (mistty--send-string
       mistty-proc
       (concat "for i in {0..5}; do"
               "\n  echo -n \"$ \""
               "\n  read"
               "\n  echo \"line $i\""
               "\ndone ; printf \"\\r\\e[6;AMODIFIED\\r\\e[6;BDONE\\n\""))
      (dotimes (_ 7)
        (mistty-send-and-wait-for-prompt)
        (mistty-send-key 1 (kbd "C-a")))
      (mistty-send-text "echo three")
      (mistty-send-and-wait-for-prompt)
      (should
       (equal
        (concat
         "$ ^A\n"
         "line 0\n"
         "$ ^A\n"
         "line 1\n"
         "$ ^A\n"
         "line 2\n"
         "MODIFIED\n"
         "line 3\n"
         "$ ^A\n"
         "line 4\n"
         "$ ^A\n"
         "line 5\n"
         "DONE\n"
         "$ echo three\n"
         "three\n"
         "$")
        (mistty-test-content
         :start (save-excursion (goto-char (point-min))
                                (mistty-test-pos "$ ^A\nline 0\n"))))))))

;; https://github.com/szermatt/mistty/issues/27
(ert-deftest mistty-test-bash-reverse-i-search-and-nl ()
  (turtles-ert-test :instance 'mistty)

  ;; This is a turtles test, because the problem only appears when you
  ;; take invisibility into account.
  (mistty-with-test-buffer (:shell bash :selected t)
    (mistty-send-text "(sleep 0.0 && echo hi)")
    (mistty-send-and-wait-for-prompt)

    (mistty--send-string mistty-proc "\C-rec\n")
    (mistty-send-and-wait-for-prompt)

    (turtles-with-grab-buffer (:win (selected-window))
      (should (equal (concat "$ (sleep 0.0 && echo hi)\n"
                             "hi\n"
                             "$ (sleep 0.0 && echo hi)\n"
                             "hi\n"
                             "$")
                     (buffer-string))))))

(ert-deftest mistty-test-ipython ()
  (mistty-with-test-buffer (:shell ipython)
    (mistty-send-text "print('hello')")
    (should (equal "hello" (mistty-send-and-capture-command-output)))))

(ert-deftest mistty-test-ipython-detect-continue-prompt ()
  (mistty-with-test-buffer (:shell ipython)
    (mistty--send-string mistty-proc "for i in (1, 2, 3):\nif i > 2:\nprint(i)")
    (mistty-wait-for-output :test (lambda () (save-excursion
                                               (goto-char (point-min))
                                               (and (search-forward "...:" nil 'noerror)
                                                    (search-forward "...:" nil 'noerror)))))

    (should (equal (concat "In [1]: for i in (1, 2, 3):\n"
                           "[   ...: ]    if i > 2:\n"
                           "[   ...: ]        print(i)")
                   (mistty-test-content
                    :start (save-excursion
                             (goto-char (point-min))
                             (search-forward "In [")
                             (match-beginning 0))
                    :show-property '(mistty-skip continue-prompt))))))

(ert-deftest mistty-test-ipython-skip-continue-prompt ()
  (mistty-with-test-buffer (:shell ipython :selected t)
    (let ((win (selected-window))
          (mistty-skip-empty-spaces t))
      (mistty--send-string mistty-proc "for i in (1, 2, 3):\nif i > 2:\nprint(i)")
      (mistty-wait-for-output :test (lambda () (save-excursion
                                                 (goto-char (point-min))
                                                 (and (search-forward "...:" nil 'noerror)
                                                      (search-forward "...:" nil 'noerror)))))

      (mistty-test-goto "    if i > 2")
      (mistty--cursor-skip win)
      (goto-char (1- (point)))
      (mistty--cursor-skip win)

      (should (equal
               "In [1]: for i in (1, 2, 3):<>"
               (mistty-test-content
                :show (point)
                :start (pos-bol) :end (pos-eol)))))))

(ert-deftest mistty-test-ipython-reconcile-in-another-line ()
  (mistty-with-test-buffer (:shell ipython)
    (mistty--send-string mistty-proc "for i in (1, 2, 3):\nif i > 2:\nprint(i)")
    (mistty-wait-for-output :test (lambda () (save-excursion
                                               (goto-char (point-min))
                                               (and (search-forward "...:" nil 'noerror)
                                                    (search-forward "...:" nil 'noerror)))))
    (narrow-to-region (save-excursion
                        (goto-char (point-min))
                        (search-forward "In [")
                        (match-beginning 0))
                      (point-max))


      (should (equal (concat "In [1]: for i in (1, 2, 3):\n"
                             "   ...:     if i > 2:\n"
                             "   ...:         print(i)")
                     (mistty-test-content)))

      (mistty-run-command
       (goto-char (point-min))
       (search-forward "(1, 2, 3)")
       (replace-match "(10, 11)" nil t))

      (should (equal (concat "In [1]: for i in (10, 11):\n"
                             "   ...:     if i > 2:\n"
                             "   ...:         print(i)")
                     (mistty-test-content)))))

(ert-deftest mistty-test-ipython-reconcile-multiline-delete ()
  (mistty-with-test-buffer (:shell ipython)
    (mistty--send-string mistty-proc "for i in (1, 2, 3):\nif i > 2:\nprint(i)")
    (mistty-wait-for-output :test (lambda () (save-excursion
                                               (goto-char (point-min))
                                               (and (search-forward "...:" nil 'noerror)
                                                    (search-forward "...:" nil 'noerror)))))
    (narrow-to-region (save-excursion
                        (goto-char (point-min))
                        (search-forward "In [")
                        (match-beginning 0))
                      (point-max))

    (should (equal (concat "In [1]: for i in (1, 2, 3):\n"
                           "   ...:     if i > 2:\n"
                           "   ...:         print(i)")
                   (mistty-test-content)))

    (mistty-run-command
     (goto-char (point-min))
     (search-forward-regexp "if i > 2\\(.\\|\n\\)*print(i)")
     (replace-match "total += i" nil t))
    (goto-char (point-min))

    (should (equal (concat "In [1]: for i in (1, 2, 3):\n"
                           "   ...:     total += i")
                   (mistty-test-content)))))

(ert-deftest mistty-test-ipython-move-cursor ()
  (mistty-with-test-buffer (:shell ipython)
    (mistty--send-string mistty-proc "for i in (1, 2, 3):\nif i > 2:\nprint(i)")
    (mistty-wait-for-output :test (lambda () (save-excursion
                                               (goto-char (point-min))
                                               (and (search-forward "...:" nil 'noerror)
                                                    (search-forward "...:" nil 'noerror)))))
    (narrow-to-region (save-excursion
                        (goto-char (point-min))
                        (search-forward "In [")
                        (match-beginning 0))
                      (point-max))

    (should (equal (concat "In [1]: for i in (1, 2, 3):\n"
                           "   ...:     if i > 2:\n"
                           "   ...:         print(i)<>")
                   (mistty-test-content :show (mistty-cursor))))

    (mistty-run-command
     (goto-char (point-min))
     (mistty-test-goto "(1, 2, 3)"))

    (should (equal (concat "In [1]: for i in <>(1, 2, 3):\n"
                           "   ...:     if i > 2:\n"
                           "   ...:         print(i)")
                   (mistty-test-content :show (mistty-cursor))))

    (mistty-run-command
     (goto-char (point-min))
     (mistty-test-goto "if"))

    (should (equal (concat "In [1]: for i in (1, 2, 3):\n"
                           "   ...:     <>if i > 2:\n"
                           "   ...:         print(i)")
                   (mistty-test-content :show (mistty-cursor))))))
