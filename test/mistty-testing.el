;;; Utilities for testing mistty -*- lexical-binding: t -*-

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
(require 'seq)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-changeset)
(require 'mistty-log)
(require 'mistty-queue)

(eval-when-compile
  ;; defined in term
  (defvar term-width)
  (defvar term-height)
  (defvar term-home-marker))

(defvar mistty-test-bash-exe (executable-find "bash"))
(defvar mistty-test-zsh-exe (executable-find "zsh")) ;; optional
(defvar mistty-test-fish-exe (executable-find "fish"));; optional
(defvar mistty-test-log nil
  "Set to t to enable logging for all tests using mistty-with-test-buffer.")

(defvar mistty-wait-for-output-timeout-s
  (if noninteractive 10 3)
  "Time, in seconds, to wait for expected output in tests.

When running tests automatically, a larger value is useful to
avoid falkey tests in case the machine running the tests is slow.

When running tests manually, a smaller value is useful to avoid
waiting for failing test results.")

(defvar mistty-test-max-try-count
  (if noninteractive 5 mistty-max-try-count)
  "Value of `mistty-max-try-count' active in tests.

Tests are strict by default, that is, every problem reported to
`mistty--report-issue-function' is treated as fatal. Tests might
want to clear or customize this function if they expect specific
problems.")

(defvar mistty-test-timeout-s
  (if noninteractive 3 mistty-timeout-s)
  "Value of `mistty-timeout-s' active in tests.")

(defvar mistty-test-stable-delay-s mistty-stable-delay-s
  "Value of `mistty-stable-delay-s' active in tests.")

(defvar mistty-test-prompt "$ ")

(defvar mistty-test-had-issues nil
  "When this is non-nil, the test will fail.

This is used to work around errors being swallowed and then
silently ignored by the test.")

(defvar-local mistty-test-content-start nil
  "Start of the buffer for `mistty-test-content'.

Defaults to (point-min).")

(defvar mistty-expected-issues nil
  "A certain number of expected issues.

This is handled by `mistty-test-report-issue' and must contain
the symbol of the expected issues, in order.")

(cl-defmacro mistty-with-test-buffer
    ((&key (shell 'bash) (selected nil)) &body body)
  "Run BODY in a MisTTY buffer.

SHELL specifies the program that is run in that buffer, bash,
zsh, or fish.

If SELECTED is non-nil, make sure the buffer is in a selected
window while BODY is running."
  (declare (indent 1))
  (let ((exec-var (intern (concat "mistty-test-" (symbol-name shell) "-exe"))))
    `(progn
       ,(if (memq shell '(bash))
            `(should ,exec-var)
          `(skip-unless ,exec-var))
       (ert-with-test-buffer ()
         (let ((mistty--report-issue-function #'mistty-test-report-issue)
               (mistty-max-try-count mistty-test-max-try-count)
               (mistty-timeout-s mistty-test-timeout-s)
               (mistty-stable-delay-s mistty-test-stable-delay-s)
               (mistty-backlog-size 500)
               (mistty-test-ok nil)
               (mistty-test-had-issues nil)
               (mistty--inhibit-fake-nl-cleanup t)
               (mistty-log mistty-test-log))
           (mistty-test-setup (quote ,shell))
           (unwind-protect
               (prog1
                 ,(if selected
                      `(with-selected-window (display-buffer (current-buffer))
                         ,@body)
                    `(progn ,@body))
                 (should-not mistty-test-had-issues)
                 (setq mistty-test-ok 'ok))
             (unless mistty-test-ok (mistty-start-log))))))))

(cl-defmacro mistty-simulate-scrollback-buffer (&body body)
  "Run BODY in a simulated scrollback buffer."
  `(let ((term-buffer mistty-term-buffer))
     (mistty--detach)
     (unwind-protect
         (progn
           ,@body)
       (mistty--attach term-buffer))))

(defmacro mistty-run-command (&rest body)
  `(progn
     (mistty-test-pre-command)
     (progn ,@body)
     (mistty-test-after-command)))

(defmacro mistty-run-command-nowait (&rest body)
  `(progn
     (mistty-test-pre-command)
     (progn ,@body)
     (mistty-test-after-command 'noempty-queue)))

(defun mistty-test-find-p (str &optional start)
  "Returns non-nil if STR is found in the current buffer."
  (save-excursion
    (goto-char (or start (point-min)))
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

(defun mistty-test-narrow (start)
  "Narrow to region START to end of buffer."
  (setq mistty-test-content-start start)
  (narrow-to-region start (point-max)))

(defun mistty-test-setup (shell)
  (mistty-mode)
  (cond
   ((eq shell 'bash)
    (mistty--exec (list mistty-test-bash-exe "--noprofile" "--norc" "-i"))
    (mistty-run-command)
    (mistty-test-set-ps1))

   ((eq shell 'zsh)
    (mistty--exec (list mistty-test-zsh-exe "-i" "--no-rcs"))
    (mistty-run-command)
    (mistty-test-set-ps1))

   ((eq shell 'fish)
    (mistty--exec
     (list
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
              "bind \\cg cancel; "
              "bind \\b backward-delete-char; ")))
    (mistty-run-command)
    (mistty-send-and-wait-for-prompt (lambda ())))

   (t (error "Unsupported shell %s" shell))))

(defun mistty-test-set-ps1 ()
  (mistty-wait-for-output :test (lambda () (< (point-min) (point-max))))
  ;; Sending the PS1 might happen too early for mistty-send-text,
  ;; which expects the text it sends to appear after the text that's
  ;; already there. It's enough to wait for the prompt anyways.
  (mistty--send-string mistty-proc (concat "PS1='" mistty-test-prompt "'"))
  (mistty-test-narrow (mistty-send-and-wait-for-prompt)))

(defun mistty-setup-fish-right-prompt ()
  (mistty-send-text "function fish_right_prompt; printf '< right'; end")
  (mistty-send-and-wait-for-prompt)
  (mistty-test-narrow (mistty--bol (point))))

(defun mistty-test-pre-command ()
  (mistty--pre-command)
  (when (and (listp buffer-undo-list) (car buffer-undo-list))
    (push nil buffer-undo-list)))

(defun mistty-test-after-command (&optional noempty-queue)
  (mistty--post-command)
  (ert-run-idle-timers)
  (unless noempty-queue
    (mistty-wait-for-output
     :test (lambda ()
             (mistty--queue-empty-p mistty--queue)))))

(cl-defun mistty-wait-for-output
    (&key (test nil) (str nil) (regexp nil) (start (point-min))
          (cursor-at-end nil)
          (proc mistty-proc) (on-error nil))
  "Wait for process output.

With TEST set to a function, keep waiting for process output
until the function evaluates to true.

With STR set to a string, keep waiting for process output until
the text can be found. If START is set, always go back to START
to search for the text instead of (point-min)

With REGEXP set to a string, keep waiting for process output
until the regexp matches. If START is set, always go back to
START to search for the text instead of (point-min).

When checking STR or REGEXP and CURSOR-AT-END is non-nil, also
require the cursor to be at the end of the matched string."
  (ert-run-idle-timers)
  (let ((condition nil)
        (condition-descr nil))
    (when str
      (setq regexp (regexp-quote str))
      (setq condition-descr (format ":str \"%s\"" str)))
    (cond
     (regexp
      (setq condition
            (lambda ()
              (save-excursion
                (when start (goto-char start))
                (and (search-forward-regexp regexp nil 'noerror)
                     (or (not cursor-at-end) (= (match-end 0) (mistty-cursor)))))))
      (unless condition-descr
        (setq condition-descr (format ":regexp \"%s\"" regexp)))
      (when cursor-at-end
        (setq condition-descr (concat condition-descr " :cursor-at-end"))))
     (test
      (setq condition test)
      (setq condition-descr (format ":test %s" test)))

     (t (error "mistty-wait-for-output: no condition specified")))

    (let ((time-limit (time-add (current-time) mistty-wait-for-output-timeout-s)))
      (while (not (funcall condition))
        (unless (time-less-p (current-time) time-limit)
          (if on-error
              (funcall on-error)
            (mistty-test-report-issue
             (format "condition not met after %ss (wait-for-output %s)"
                     mistty-wait-for-output-timeout-s condition-descr))))
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
      (mistty-test-narrow next-prompt-start))
    output))

(defun mistty-send-and-wait-for-prompt (&optional send-command-func prompt)
  "Send the current command line and wait for a prompt to appear.

Puts the point at the end of the prompt and return the position
of the beginning of the prompt."
  (let ((before-send (point)))
    (funcall (or send-command-func #'mistty-send-command))
    (mistty-wait-for-output
     :regexp (concat "^" (regexp-quote (or prompt mistty-test-prompt)))
     :start before-send)
    (match-beginning 0)))

(defun mistty-test-report-issue (issue)
  "Report ISSUE with extra debugging information.

This is meant to be assigned to `mistty--report-issue-function'
 as well as called directly from tests."
  (let ((mistty-log mistty-log))
    (if (eq issue (car mistty-expected-issues))
        (progn
          (mistty-log "EXPECTED %s" issue)
          (setq mistty-expected-issues (cdr mistty-expected-issues)))

      ;; unexpected
      (save-excursion (mistty-start-log))
      (let ((error-message
             (format "%s: BUF<<EOF%sEOF"
                     issue
                     (mistty-test-content
                      :show-property '(mistty-skip t)
                      :show (list (point) (ignore-errors (mistty-cursor)))))))
        (mistty-log error-message)
        ;; Errors might get caught. This makes sure
        (setq mistty-test-had-issues t)
      (error "%s" error-message)))))

(cl-defun mistty-test-content (&key (start (or mistty-test-content-start (point-min)))
                                    (end (point-max))
                                    (show nil)
                                    (show-property '(nil nil))
                                    (strip-last-prompt nil)
                                    (trim t))
  "Return buffer content, post-processed.

START and END specify the region to extract.

SHOW might be a specific position to highlight with <> in the
string, often just the output of `point' or `mistty-cursor', or
a list of such positions. Positions that are nil are ignored.

STRIP-LAST-PROMPT, if t, removes the last, empty prompt from the
returned content.

SHOW-PROPERTY \\='(PROP VAL) puts section with text-property PROP
set to VAL within brackets.

Unless TRIM is set to nil, trailing newlines are always stripped
out from the output."
  (interactive)
  (let ((output (buffer-substring start end)))
    (when show
      (setq output (mistty-show-point
                    output start end
                    (if (listp show) show (list show)))))
    (pcase-let ((`(,prop ,val) show-property))
      (when prop
        (setq output (mistty-show-property prop val output))))
    (set-text-properties 0 (length output) nil output)

    (when strip-last-prompt
      (setq output (replace-regexp-in-string "\\$ \\(<>\\)?\n?$" "" output)))
    (when trim
      (setq output (replace-regexp-in-string "[ \t]*$" "" output))
      (setq output (replace-regexp-in-string "[ \t\n]*\\'" "" output)))
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
     :cursor-at-end t
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

(defun mistty-show-point (text start end positions)
  "Highlight positions with mistty-point property with '<>'."
  (let ((idx 0)
        (len (length text))
        (position-alist
         (delq nil
               (seq-map-indexed
                (lambda (pos idx)
                  (when (number-or-marker-p pos)
                    (cons (if (zerop idx) "<>" (format "<%d>" idx))
                          (- pos start))))
                positions)))
        output)
    (setq position-alist
          (seq-filter
           (lambda (elt)
             (and (>= (cdr elt) 0) (<= (cdr elt) (- end start))))
           position-alist))
    (setq position-alist
          (sort position-alist
                (lambda (a b)
                  (< (cdr a) (cdr b)))))
    (pcase-dolist (`(,show-str . ,pos) position-alist)
      (push (substring text idx pos) output)
      (push show-str output)
      (setq idx pos))
    (push (substring text idx len) output)
    (apply #'concat (nreverse output))))

(defun mistty-ert-explain-string-match (a b)
  `(string-match ,a ,b))
(put 'equal 'ert-explainer 'mistty-ert-explain-string-match)

(defun mistty--stuck-interaction (text)
  "Returns a mistty--interact that waits for TEXT.

The interaction waits for TEXT, but never sends it, so it'll wait
forever - or until the test calls mistty--send-text directly."
  (mistty--interact stuck (interact)
    (if (with-current-buffer mistty-term-buffer
          (looking-back (regexp-quote text) nil))
        (mistty--interact-done)
      (mistty--interact-keep-waiting))))

(defun mistty-test-freeze-queue ()
  "Freezes `mistty-queue'.

Add an interaction into the queue that'll get stuck until
the function returned by this function is called.

This should only be used when the shell is bash, as it sends a
^G, which bash answers with another ^G, but cause other shells to
redraw everything."
  (let ((can-continue nil))
    (mistty--enqueue
     mistty--queue
     (mistty--interact freeze (interact)
       (if can-continue
           (mistty--interact-done)
         (mistty--interact-keep-waiting))))

    (lambda ()
      (setq can-continue t)
      ;; make sure we get something back from the shell right away.
      (mistty--send-string mistty-proc "\C-g"))))

(defun mistty-test-tramp-methods ()
  "A value for `test-tramp-methods' with a dummy test protocol."
  '(("dummy"
     (tramp-login-program "/bin/sh")
     (tramp-login-args nil)
     (tramp-remote-shell "/bin/sh")
     (tramp-remote-shell-args ("-c"))
     (tramp-connection-timeout 10))))

(defun mistty-test-tramp-prefix ()
  "Build a TRAMP file prefix for a remote file for testing."
  (format "/dummy:%s:" (system-name)))

(defun mistty-test-tramp-protocol ()
  "The TRAMP protocol used for testing."
  '(:protocol "dummy"))

(defun mistty-test-remote-command ()
  "Return the remote command, as reported by TRAMP or nil."
  ;; tramp-handle-make-process sets remote-command on the processes
  ;; it starts.
  (pcase-let ((`("/bin/sh" "-c" ,_ ".." ,command . _)
               (process-get mistty-proc 'remote-command)))
    command))

(defun mistty-test-nobracketed-paste ()
  "Disable bracketed paste in a Bash shell."
  (mistty-send-text "bind 'set enable-bracketed-paste off'")
  (mistty-test-narrow (mistty-send-and-wait-for-prompt)))

(defun mistty-reload-all ()
  "Force a reload of all mistty .el files.

This is useful after files have changed, such as after checking
 out a new branch."
  (interactive)
  (load "mistty-util.el" nil 'nomessage 'nosuffix)
  (load "mistty-log.el" nil 'nomessage 'nosuffix)
  (load "mistty-changeset.el" nil 'nomessage 'nosuffix)
  (load "mistty-undo.el" nil 'nomessage 'nosuffix)
  (load "mistty-term.el" nil 'nomessage 'nosuffix)
  (load "mistty-queue.el" nil 'nomessage 'nosuffix)
  (load "mistty-osc7.el" nil 'nomessage 'nosuffix)
  (load "mistty.el"nil 'nomessage 'nosuffix))

(provide 'mistty-testing)
