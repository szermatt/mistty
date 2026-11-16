;;; mistty-nav.el --- Navigation tools for MisTTY -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; This file define types used to navigate between prompt on a MisTTY
;; buffer.

(require 'pcase)
(require 'cl-lib)
(require 'seq)
(require 'imenu)

(require 'mistty-prompt)
(require 'mistty-util)

;;; Code:

(cl-defstruct (mistty--prompt-ranges
               (:constructor mistty--make-prompt-ranges))
  ;; Start of the prompt (buffer position)
  start

  ;; Start of the command output and end of the prompt (buffer position)
  output

  ;; End of the command output (buffer position)
  end

  ;; Non-nil if this is the current prompt
  current-p

  ;; Command-line set through OSC 133
  osc133-cmdline

  ;; Start of user input set through OSC 133 (buffer position)
  osc133-user-input-start)

(defsubst mistty--prompt-ranges-empty-p (prompt-ranges)
  "Return non-nil if PROMPT-RANGES output is empty."
  (not (> (mistty--prompt-ranges-end prompt-ranges)
          (mistty--prompt-ranges-output prompt-ranges))))

(defun mistty--prompt-ranges-cmdline (prompt-ranges &optional merge-lines)
  "Get command-line for PROMPT-RANGES.

Return nil if no cmdline was provided and it couldn't be extracted."
  (or (mistty--prompt-ranges-osc133-cmdline prompt-ranges)
      (let* ((beg (mistty--prompt-ranges-start prompt-ranges))
             (end (mistty--prompt-ranges-output prompt-ranges))
             (cmdline (or
                       (when-let* ((pos (text-property-not-all
                                         beg end 'mistty-cmdline nil)))
                         (get-text-property pos 'mistty-cmdline))
                       (mistty--extract-cmdline prompt-ranges))))
        (if merge-lines
            (replace-regexp-in-string
             "[[:blank:]]*\n[[:blank:]]*"
             mistty-newline-replacement cmdline)
          cmdline))))

(defun mistty--extract-cmdline (prompt-ranges)
  "Extract cmdline from PROMPT-RANGES, if possible.

Note that this is unreliable unless the shell issued OSS code B to mark
the prompt beginning.

Return nil if no cmdline could be extracted."
  (let ((prompt-text (buffer-substring
                      (mistty--user-input-start prompt-ranges)
                      (mistty--prompt-ranges-output prompt-ranges))))
    (with-temp-buffer
      (insert prompt-text)

      ;; Remove anything marked mistty-skip.
      (let ((pos (point-min)))
        (while (and
                (> (point-max) pos)
                (setq pos (text-property-not-all
                           pos (point-max) 'mistty-skip nil)))
          (delete-region pos (next-single-property-change
                              pos 'mistty-skip nil (point-max)))))

      ;; Trim
      (goto-char (point-min))
      (while (search-forward-regexp "^[[:blank:]]+" nil 'noerror)
        (replace-match ""))
      (goto-char (point-min))
      (while (search-forward-regexp "[[:blank:]\r]+$" nil 'noerror)
        (replace-match ""))
      (while (eq ?\n (char-before (point-max)))
        (delete-region (1- (point-max)) (point-max)))

      ;; Return whatever is left
      (when (> (point-max) (point-min))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun mistty--user-input-start (prompt-ranges)
  "Find the position of the start of user input within PROMPT-RANGES."
  (or (mistty--prompt-ranges-osc133-user-input-start prompt-ranges)
      (mistty--extract-user-input-start
       (mistty--prompt-ranges-start prompt-ranges)
       (mistty--prompt-ranges-output prompt-ranges))))

(defun mistty--extract-user-input-start (start end)
  "Extract the position of the start of user input in range START to END."
  (let (pos)
    (save-excursion
      (cond
       ;; Prompt is marked.
       ((setq pos (text-property-any start end 'field 'prompt))
        (next-single-property-change pos 'field nil end))

       ;; Prompt is not marked. Look for something that looks like a prompt.
       ((progn
          (goto-char end)
          (search-backward-regexp "^[^#$%>\n]*[#$%>] +\\([[:alpha:]]+\\|$\\)" start 'noerror))
        (match-beginning 1))

       ;; Last line look like the end of a multi-line-prompt. Keep
       ;; everything in [start,end].
       ((progn
          (goto-char end)
          (while (and (>= (point) start)
                      (eq ?\n (char-before (point))))
            (goto-char (1- (point))))
          (goto-char (pos-bol))
          (looking-at "[[:blank:]]*\\(end\\|done\\|fi\\)"))
        start)

       ;; Keep only the last line within [start,end].
       (t (mistty--bol end))))))

(defun mistty-imenu-create-index ()
  "Create an imenu index of command-lines and outputs.

This is meant to be bound to `imenu-create-index-function'."
  (let ((iter (mistty--make-navigator (point-max) 'backward))
        (maybe-marker (lambda (pos)
                        (if imenu-use-markers
                            (copy-marker pos)
                          pos)))
        prompt output-alist prompt-alist)
    (while (setq prompt (funcall iter))
      (if (mistty--prompt-ranges-current-p prompt)
          ;; Active prompt
          (push (cons "CURRENT"
                      (funcall maybe-marker
                               (mistty--user-input-start prompt)))
                prompt-alist)

        ;; Previous prompts
        (when-let* ((cmdline (mistty--prompt-ranges-cmdline prompt 'merge-lines)))
          (unless (mistty--prompt-ranges-empty-p prompt)
            (push (cons cmdline
                        (funcall maybe-marker
                                 (mistty--prompt-ranges-output prompt)))
                  output-alist))
          (when (> (mistty--prompt-ranges-output prompt)
                   (mistty--prompt-ranges-start prompt))
            (push (cons cmdline
                        (funcall maybe-marker
                                 (mistty--user-input-start prompt)))
                  prompt-alist)))))
    `(("Outputs" . ,(nreverse output-alist))
      ("Commands" . ,(nreverse prompt-alist)))))


(defun mistty-beginning-of-defun (&optional n)
  "Go to the beginning of the N'th defun.

This is meant to be set as `beginning-of-defun-function' in MisTTY
buffers."
  (let ((n (or n 1)))
    (when-let* ((prompt (mistty--navigator-nth
                         (if (>= n 0) 'backward 'forward)
                         (abs n)
                         (lambda (prompt)
                           ;; Skip the prompt around point if already at its
                           ;; start, unless called indirectly by `end-of-defun'.
                           (not (and (eq this-command 'beginning-of-defun)
                                     (= (point) (mistty--prompt-ranges-start prompt))))))))
      (prog1 t
        (goto-char (if (>= n 0)
                       (mistty--prompt-ranges-start prompt)
                     (mistty--prompt-ranges-end prompt)))))))

(defun mistty-end-of-defun ()
  "Go to the end of the current defun.

This function assumes that the point is at the beginning of a defun, set
by `mistty-beginning-of-defun'.

This is meant to be set as `end-of-defun-function' in MisTTY buffers."
  (when-let* ((ranges (funcall (mistty--make-navigator (point) 'forward))))
    (prog1 t
      (goto-char (mistty--prompt-ranges-end ranges)))))

(defun mistty-next-input (n)
  "Move the point to the Nth next input in the buffer."
  (interactive "p")
  (if-let* ((prompt (mistty--navigator-nth
                     'forward n
                     (lambda (prompt)
                       ;; Skip the prompt around point.
                       (< (point) (mistty--prompt-ranges-start prompt))))))
      (goto-char (mistty--user-input-start prompt))
    (user-error "No next input")))

(defun mistty-previous-input (n)
  "Move the point to the Nth previous input in the buffer."
  (interactive "p")
  (if-let* ((prompt (mistty--navigator-nth
                     'backward n
                     (lambda (prompt)
                       ;; Accept the prompt around point only when point is in
                       ;; its output; never accept the active prompt (where
                       ;; output is point-max).
                       (and (>= (point) (mistty--prompt-ranges-output prompt))
                            (not (mistty--prompt-ranges-current-p prompt)))))))
      (goto-char (mistty--user-input-start prompt))
    (user-error "No previous input")))

(defun mistty-next-output (n)
  "Move the point to the beginning of the Nth next output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (if-let* ((prompt (mistty--navigator-nth
                     'forward n
                     (lambda (prompt)
                       ;; Accept the prompt around point only when point is
                       ;; still in its input.
                       (and (< (point) (mistty--prompt-ranges-output prompt))
                            (not (mistty--prompt-ranges-empty-p prompt)))))))
      (prog1 (cons (mistty--prompt-ranges-output prompt)
                   (mistty--prompt-ranges-end prompt))
        (goto-char (mistty--prompt-ranges-output prompt)))
    (user-error "No next output")))

(defun mistty-previous-output (n)
  "Move the point to the beginning of the Nth previous output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (if-let* ((prompt (mistty--navigator-nth
                     'backward n (mistty--accept-previous-output))))
      (prog1 (cons (mistty--prompt-ranges-output prompt)
                   (mistty--prompt-ranges-end prompt))
        (goto-char (mistty--prompt-ranges-output prompt)))
    (user-error "No previous output")))

(defun mistty-current-output-range ()
  "Return the range of the current output, if point is on an output.

This function returns nil, if the point is not inside an output.
Otherwise, it returns a cons containing the start and end
position of the output."
  (let ((prompt (funcall (mistty--make-navigator (point) 'backward))))
    (if (and prompt
             (and (<= (mistty--prompt-ranges-output prompt) (point)) ; point must be below input
                  (< (point) (mistty--prompt-ranges-end prompt)))) ; point must be above output end
        (cons (mistty--prompt-ranges-output prompt)
              (mistty--prompt-ranges-end prompt)))))

(defun mistty-select-output (&optional n)
  "Select the current or Nth previous output range."
  (interactive "P")
  (if-let* ((prompt (mistty--navigator-nth
                     'backward n
                     (mistty--accept-previous-output (not (numberp n))))))
      (progn
        (goto-char (mistty--prompt-ranges-output prompt))
        (set-mark (mistty--prompt-ranges-end prompt)))
    (user-error "No current or previous output")))

(defun mistty-create-buffer-with-output (buffer-name &optional n)
  "Create the new buffer BUFFER-NAME with N'th previous output.

If called with no arguments and the point is on an output, create
a buffer with that output."
  (interactive
   (list (read-buffer
          "New buffer name: "
          ;; Generate a reasonable default new buffer name.
          (generate-new-buffer-name
           (mistty--truncate-string
            (or
             (when-let* ((prompt (mistty--navigator-nth
                                  'backward current-prefix-arg
                                  (mistty--accept-previous-output
                                   (not (numberp current-prefix-arg))))))
               (mistty--prompt-ranges-cmdline prompt 'merge-lines))
             "command output")
            30)))
          current-prefix-arg))
  (if-let* ((prompt (mistty--navigator-nth
                     'backward n
                     (mistty--accept-previous-output (not (numberp n)))))
            (buffer (generate-new-buffer buffer-name)))
      (progn
        (copy-to-buffer buffer
                        (mistty--prompt-ranges-output prompt)
                        (mistty--prompt-ranges-end prompt))
        (with-current-buffer buffer
          (set-auto-mode 'keep-mode-if-sane))
        (pop-to-buffer buffer)
        buffer)
    (user-error "No current or previous output")))

(defun mistty--accept-previous-output (&optional current-ok)
  "Return a predicate for `mistty--navigator-nth' matching previous outputs.

If CURRENT-OK is non-nil, also accept the output around point."
  (lambda (prompt)
    (and (or current-ok
             ;; Skip the prompt around point, even if point is in its output.
             (> (point) (mistty--prompt-ranges-end prompt)))
         (not (mistty--prompt-ranges-empty-p prompt)))))

(defun mistty--navigator-nth (navigator n &optional accept)
  "Go through NAVIGATOR and return the accepted Nth element.

NAVIGATOR may be a function returned by `mistty--make-navigator' or a
direction symbol (\\='forward or \\='backward) to navigate from point.

If N is not a number, it defaults to 1.

If specified ACCEPT must be a function that takes a prompt-range and
return non-nil to accept it."
  (let ((navigator (if (functionp navigator)
                       navigator
                     (mistty--make-navigator (point) navigator)))
        (n (if (numberp n) n 1))
        (accepted-count 0)
        (accept (or accept (lambda (_) t)))
        prompt)
    (while (and (setq prompt (funcall navigator))
                (progn
                  (when (funcall accept prompt)
                    (cl-incf accepted-count))
                  (< accepted-count n))))

    prompt))


(defun mistty--make-navigator (start-pos direction)
  "Return an iterator over prompt-ranges starting at START-POS.

DIRECTION must be either \\='forward to move forward each call or
\\='backward to move backward each call.

The return value is a function that returns `mistty--prompt-ranges'
instance, starting with the one around START-POS, then looking in the
specified direction. It returns nil once there are no more ranges."
  (unless (memq direction '(forward backward))
    (error "Direction should be forward or backward, not %S" direction))
  (let* (;; Prompts in the terminal zone
         (active (seq-into (reverse (mistty--all-prompts mistty--prompt-cell))
                           'vector))
         (top-active-pos (or (when (length> active 0)
                               (mistty--find-scrolline
                                (mistty--prompt-start (aref active 0))))
                             (point-max)))
         ;; Current prompt-ranges
         current
         ;; Either nil or the index of the current prompt in ACTIVE
         current-idx
         ;; Value to return next time the function is called
         first-result)

    (cl-labels
        ;; Build a prompt range from (aref active IDX)
        ;;
        ;; Return nil if IDX is out of range of if the scrolline
        ;; positions cannot be found.
        ((build-from-active (idx)
           (when (and (>= idx 0) (< idx (length active)))
             (when-let* ((p (aref active idx))
                         (start (mistty--find-scrolline (mistty--prompt-start p)))
                         (output (or (when (mistty--prompt-end p)
                                             (mistty--find-scrolline (mistty--prompt-end p)))
                                         (point-max)))
                         (end (if (< (1+ idx) (length active))
                                  (mistty--find-scrolline
                                   (mistty--prompt-start (aref active (1+ idx))))
                                output)))
               (mistty--make-prompt-ranges
                :current-p (eq p (mistty--prompt-cell-current mistty--prompt-cell))
                :osc133-cmdline (mistty--prompt-cmdline p)
                :osc133-user-input-start
                (when-let* ((input-start (mistty--prompt-user-input-start p))
                            (bol (mistty--find-scrolline (car input-start))))
                  (min (mistty--eol bol) (+ bol (cdr input-start))))
                :start start
                :output output
                :end end))))

         ;; Build a prompt range starting at START.
         ;;
         ;; START is unchecked. Use it only when you're sure
         ;; that this is the start of a text region marked with a
         ;; specific input-id.
         ;;
         ;; Return nil if no range found.
         (build-starting-at (start)
           (let* (;; prompt-end is the end of the region marked with the
                  ;; current input id.
                  (prompt-end (next-single-property-change
                               start 'mistty-input-id nil top-active-pos))

                  ;; end is the beginning of a later region marked
                  ;; with a different input id.
                  (end (if (or (>= prompt-end top-active-pos)
                                      (get-text-property prompt-end 'mistty-input-id))
                                  prompt-end
                                (next-single-property-change
                                 prompt-end 'mistty-input-id nil top-active-pos))))
             (mistty--make-prompt-ranges
              :start start
              :output prompt-end
              :end end)))

         ;; Build a prompt range around POS
         ;;
         ;; Return nil if no range found.
         (build-around (pos)
           (cl-block nil
             (unless (get-text-property pos 'mistty-input-id)
               ;; Look for an input region above point
               (setq pos (previous-single-property-change (min (1+ pos) (point-max)) 'mistty-input-id))
               (unless pos (cl-return nil))
               (cl-decf pos))
             ;; Look for the beginning of the current input region
             (when-let* ((start (previous-single-property-change
                                        (min (1+ pos) (point-max))
                                        'mistty-input-id nil (point-min))))
               (build-starting-at start)))))

      ;; Set current and current idx to get started
      (if (and (>= start-pos top-active-pos)
               (length> active 0))
          ;; Build from active.
          (let ((start-scrolline (mistty--scrolline-at start-pos)))
            (let ((idx-after (seq-position active nil
                                           (lambda (p _)
                                             (> (mistty--prompt-start p)
                                                start-scrolline)))))
              ;; idx-after is the index of a prompt that starts after
              ;; start-scrolline. We want the prompt just before.
              (setq current-idx (1- (or idx-after (length active))))
              (setq current (build-from-active current-idx))))

        ;; build from text properties
        (setq current (build-around start-pos))
        (when (and (null current) (eq direction 'forward))
          (setq current (build-around
                         (text-property-not-all
                          start-pos top-active-pos 'mistty-input-id nil)))))

      ;; The first time the lambda is called, return whatever was
      ;; found.
      (setq first-result current)

      (lambda ()
        (cond
         (first-result
          (prog1 first-result
            (setq first-result nil)))

         ;; Find prompt range after current
         ((and current (eq direction 'forward))
          (if current-idx
              ;; Return from active, until we reach array end
              (progn
                (setq current-idx (1+ current-idx))
                (setq current (build-from-active current-idx)))

            ;; Look in text properties, until we reach active
            (let ((pos (mistty--prompt-ranges-end current)))
              (if (>= pos top-active-pos)
                  (progn
                    (setq current-idx 0)
                    (setq current (build-from-active 0)))
                (setq current (build-starting-at pos)))))

          current)

         ;; Find prompt range before current
         ((and current (eq direction 'backward))
          (if (and current-idx (> current-idx 0))
              ;; Return from active, until index reaches 0
              (progn
                (setq current-idx (1- current-idx))
                (setq current (build-from-active current-idx)))

            ;; Look in text properties, until we reach buffer top
            (let ((pos (mistty--prompt-ranges-start current)))
              (setq current-idx nil)
              (setq current (when (> pos (point-min))
                              (build-around (1- pos))))))

          current))))))

(provide 'mistty-nav)

;;; mistty-nav.el ends here
