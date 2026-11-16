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

(require 'cl-lib)
(require 'imenu)

(require 'mistty-prompt)
(require 'mistty-util)

;;; Code:

(cl-defstruct (mistty--prompt-ranges
               (:constructor mistty--make-prompt-ranges))
  ;; Start of the prompt (buffer position)
  prompt-start

  ;; Start of the command output and end of the prompt (buffer position)
  output-start

  ;; End of the command output (buffer position)
  output-end)

(defun mistty--prompt-ranges-for-current-or-previous-output (n)
  "Return the prompt range of the current or the Nth output.

If N is a number, behave like `mistty-previous-output', otherwise
return either the current or the previous output

This function fails if there is no current or previous output."
  (let ((must-be-above-point (numberp n))
        (n (if (numberp n) n 1)))
    (if-let* ((prompt
              (mistty--backward-prompt-ranges
               (let ((accepted-count 0))
                 (lambda (prompt)
                   (when (and
                          ;; Output must be above point (same as previous-output)
                          (or (not must-be-above-point)
                              (> (point) (mistty--prompt-ranges-output-end prompt)))
                          ;; Ignore empty prompts
                          (> (mistty--prompt-ranges-output-end prompt)
                             (mistty--prompt-ranges-output-start prompt)))
                     (cl-incf accepted-count))

                   (>= accepted-count n))))))
        prompt
      (user-error "No current or previous output"))))

(defun mistty--prompt-ranges-around (pos active-prompt-ranges)
  "Return ranges of prompt around POS.

If a prompt is found around POS, return a `mistty--prompt-ranges'
struct, otherwise return nil.

ACTIVE-PROMPT-RANGES must be the output of
`mistty--active-or-potential-prompt-ranges'."
  (catch 'mistty-return
    (when (and active-prompt-ranges
               (>= pos (mistty--prompt-ranges-prompt-start active-prompt-ranges)))
      (throw 'mistty-return active-prompt-ranges))
    (unless (get-text-property pos 'mistty-input-id)
      (setq pos (previous-single-property-change (min (1+ pos) (point-max)) 'mistty-input-id))
      (unless pos
        (throw 'mistty-return nil))
      (setq pos (1- pos)))
    ;; At pos, mistty-input-id is non-nil
    (setq pos (previous-single-property-change (min (1+ pos) (point-max)) 'mistty-input-id nil (point-min)))
    (mistty--prompt-ranges-at pos active-prompt-ranges)))

(defun mistty--prompt-ranges-at (start active-prompt-ranges)
  "Return ranges of a prompt starting at START.

If a prompt is found that starts at START, return a
`mistty--prompt-ranges' struct, otherwise return nil.

ACTIVE-PROMPT-RANGES must be the output of
`mistty--active-or-potential-prompt-ranges'."
  (let ((limit (if active-prompt-ranges
                   (mistty--prompt-ranges-prompt-start active-prompt-ranges)
                 (point-max))))
    (if (and (< start limit) (get-text-property start 'mistty-input-id))
        (let* ((prompt-end (next-single-property-change start 'mistty-input-id nil limit))
               (output-end (if (or (>= prompt-end limit)
                                   (get-text-property prompt-end 'mistty-input-id))
                               prompt-end
                             (next-single-property-change prompt-end 'mistty-input-id nil limit))))
          (mistty--make-prompt-ranges
           :prompt-start start
           :output-start prompt-end
           :output-end output-end))
      (if (and active-prompt-ranges
               (equal start (mistty--prompt-ranges-prompt-start active-prompt-ranges)))
          active-prompt-ranges
        (error "Invalid usage of mistty--prompt-ranges-at; no prompt starts at %s" start)))))

(defun mistty--active-or-potential-prompt-ranges ()
  "If a prompt is active, return a `mistty--prompt-ranges' struct.

Also accepts inactive (potential) prompts."
  (when-let* ((prompt (mistty--get-prompt mistty--prompt-cell))
              (start-pos (mistty--find-scrolline (mistty--prompt-start prompt)))
              (end-pos (or (when-let* ((end (mistty--prompt-end prompt)))
                             (mistty--find-scrolline end))
                           (point-max))))
    (mistty--make-prompt-ranges
     :prompt-start start-pos
     :output-start end-pos
     :output-end end-pos)))

(defun mistty--forward-prompt-ranges (accept)
  "Go through prompt ranges forward until ACCEPT return non-nil.

Return the prompt range that was accepted or nil."
  (let* ((active-prompt-ranges (mistty--active-or-potential-prompt-ranges))
         (prompt (mistty--prompt-ranges-around (point) active-prompt-ranges)))
    (catch 'mistty--accepted
      (unless prompt
        ;; If there's no prompt at point, look for a prompt after point.
        (when-let* ((pos (next-single-property-change (point) 'mistty-input-id)))
            (setq prompt (mistty--prompt-ranges-around pos active-prompt-ranges))))
      (when (and prompt (funcall accept prompt))
        (throw 'mistty--accepted prompt))
      (while
          (setq prompt (when (and prompt (< (mistty--prompt-ranges-output-end prompt) (point-max)))
                         (mistty--prompt-ranges-at
                          (mistty--prompt-ranges-output-end prompt)
                          active-prompt-ranges)))
        (when (funcall accept prompt)
          (throw 'mistty--accepted prompt))))))

(defun mistty--backward-prompt-ranges (accept)
  "Go through prompt ranges backward until ACCEPT return non-nil.

Return the prompt range that was accepted or nil."
  (let* ((active-prompt-ranges (mistty--active-or-potential-prompt-ranges))
         (prompt (mistty--prompt-ranges-around (point) active-prompt-ranges)))
    (catch 'mistty--accepted
      (when (and prompt (funcall accept prompt))
        (throw 'mistty--accepted prompt))
      (while
          (setq prompt (when (and prompt (> (mistty--prompt-ranges-prompt-start prompt) (point-min)))
                         (mistty--prompt-ranges-around
                          (1- (mistty--prompt-ranges-prompt-start prompt))
                          active-prompt-ranges)))
        (when (funcall accept prompt)
          (throw 'mistty--accepted prompt))))))

(defun mistty--cmdline-for-output (prompt-ranges)
  "Extract cmdline for PROMPT-RANGES, if possible.

Return nil if no cmdline could be extracted."
  (when-let* ((pos (text-property-not-all
                    (mistty--prompt-ranges-prompt-start prompt-ranges)
                    (mistty--prompt-ranges-output-start prompt-ranges)
                    'mistty-cmdline nil)))
    (get-text-property pos 'mistty-cmdline)))

(defun mistty--extract-cmdline (beg end)
  "Extract cmdline from range BEG to END, if possible.

Note that this is unreliable unless the shell issued OSS code B to mark
the prompt beginning.

Return nil if no cmdline could be extracted."
  (let ((prompt-text (buffer-substring
                      (mistty--user-input-start beg end)
                      end)))
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

      ;; Get rid of newlines.
      (goto-char (point-min))
      (while (search-forward-regexp
              "[[:blank:]]*\n[[:blank:]]*" nil 'noerror)
        (replace-match mistty-newline-replacement))

      ;; Return whatever is left
      (when (> (point-max) (point-min))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun mistty--user-input-start (start end)
  "Find the position of the start of user input within PROMPT-RANGES."
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
  (let ((active-prompt (mistty--active-or-potential-prompt-ranges))
        (maybe-marker (lambda (pos)
                        (if imenu-use-markers
                            (copy-marker pos)
                          pos)))
        output-alist prompt-alist)
    (save-excursion
      (goto-char (point-max))
      (mistty--backward-prompt-ranges
       (lambda (prompt)
         (prog1 nil ;; keep looking
           (if (equal prompt active-prompt)
               ;; Active prompts
               (push (cons "CURRENT"
                           (funcall maybe-marker
                                    (mistty--user-input-start
                                     (mistty--prompt-ranges-prompt-start prompt)
                                     (mistty--prompt-ranges-output-start prompt))))
                     prompt-alist)

             ;; Previous prompts
             (when-let* ((cmdline (mistty--cmdline-for-output prompt)))
               (when (> (mistty--prompt-ranges-output-end prompt)
                        (mistty--prompt-ranges-output-start prompt))
                 (push (cons cmdline
                             (funcall maybe-marker
                                      (mistty--prompt-ranges-output-start prompt)))
                       output-alist))
               (when (> (mistty--prompt-ranges-output-start prompt)
                        (mistty--prompt-ranges-prompt-start prompt))
                 (push (cons cmdline
                             (funcall maybe-marker
                                      (mistty--user-input-start
                                       (mistty--prompt-ranges-prompt-start prompt)
                                       (mistty--prompt-ranges-output-start prompt))))
                       prompt-alist)))))))
      `(("Outputs" . ,(nreverse output-alist))
        ("Commands" . ,(nreverse prompt-alist))))))


(defun mistty-beginning-of-defun (&optional n)
  "Go to the beginning of the N'th defun.

This is meant to be set as `beginning-of-defun-function' in MisTTY
buffers."
  (let ((n (or n 1)))
    (let ((accept (let ((accepted-count 0)
                        (abs-n (abs n)))
                    (lambda (prompt)
                      (unless (and (eq this-command 'beginning-of-defun)
                                   (= (point) (mistty--prompt-ranges-prompt-start prompt)))
                        (cl-incf accepted-count))
                      (>= accepted-count abs-n)))))
      (if (>= n 0)
          (when-let* ((prompt (mistty--backward-prompt-ranges accept)))
            (prog1 t
              (goto-char (mistty--prompt-ranges-prompt-start prompt))))
        (when-let* ((prompt (mistty--forward-prompt-ranges accept)))
          (prog1 t
            (goto-char (mistty--prompt-ranges-output-end prompt))))))))

(defun mistty-end-of-defun ()
  "Go to the end of the current defun.

This function assumes that the point is at the beginning of a defun, set
by `mistty-beginning-of-defun'.

This is meant to be set as `end-of-defun-function' in MisTTY buffers."
  (let ((active-prompt-ranges (mistty--active-or-potential-prompt-ranges)))
    (when-let* ((ranges (mistty--prompt-ranges-at (point) active-prompt-ranges)))
      (prog1 t
        (goto-char (mistty--prompt-ranges-output-end ranges))))))

(defun mistty-next-input (n)
  "Move the point to the Nth next input in the buffer."
  (interactive "p")
  (if-let* ((prompt
            (mistty--forward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (< (point) (mistty--prompt-ranges-prompt-start prompt)) ; input must be below point
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (goto-char (mistty--user-input-start (mistty--prompt-ranges-prompt-start prompt)
                                           (mistty--prompt-ranges-output-start prompt)))
    (user-error "No next input")))

(defun mistty-previous-input (n)
  "Move the point to the Nth previous input in the buffer."
  (interactive "p")
  (if-let* ((prompt
            (mistty--backward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (and (>= (point) (mistty--prompt-ranges-output-start prompt))  ; input must be above point
                            (> (point-max) (mistty--prompt-ranges-output-start prompt))) ;; ignore active prompt
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (goto-char (mistty--user-input-start (mistty--prompt-ranges-prompt-start prompt)
                                           (mistty--prompt-ranges-output-start prompt)))
    (user-error "No previous input")))

(defun mistty-next-output (n)
  "Move the point to the beginning of the Nth next output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (if-let* ((prompt
            (mistty--forward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (and
                        (< (point) (mistty--prompt-ranges-output-start prompt)) ; output must be above point
                        (> (mistty--prompt-ranges-output-end prompt)
                           (mistty--prompt-ranges-output-start prompt))) ; ignore empty prompts
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (prog1 (cons (mistty--prompt-ranges-output-start prompt)
                   (mistty--prompt-ranges-output-end prompt))
        (goto-char (mistty--prompt-ranges-output-start prompt)))
    (user-error "No next output")))

(defun mistty-previous-output (n)
  "Move the point to the beginning of the Nth previous output in the buffer.

In addition to moving the point, this function also returns a
cons, (START . END) containing the start and end position of the
output."
  (interactive "p")
  (if-let* ((prompt
            (mistty--backward-prompt-ranges
             (let ((accepted-count 0))
               (lambda (prompt)
                 (when (and (> (point) (mistty--prompt-ranges-output-end prompt)) ; output must be above point
                            (> (mistty--prompt-ranges-output-end prompt)
                               (mistty--prompt-ranges-output-start prompt))) ; ignore empty prompts
                   (cl-incf accepted-count))

                 (>= accepted-count n))))))
      (prog1 (cons (mistty--prompt-ranges-output-start prompt)
                   (mistty--prompt-ranges-output-end prompt))
        (goto-char (mistty--prompt-ranges-output-start prompt)))
    (user-error "No previous output")))

(defun mistty-current-output-range ()
  "Return the range of the current output, if point is on an output.

This function returns nil, if the point is not inside an output.
Otherwise, it returns a cons containing the start and end
position of the output."
  (let ((prompt (mistty--prompt-ranges-around
                 (point) (mistty--active-or-potential-prompt-ranges))))
    (if (and prompt
             (and (<= (mistty--prompt-ranges-output-start prompt) (point))  ; point must be below input
                  (< (point) (mistty--prompt-ranges-output-end prompt)))) ; point must be above output end
        (cons (mistty--prompt-ranges-output-start prompt)
              (mistty--prompt-ranges-output-end prompt)))))

(defun mistty-select-output (&optional n)
  "Select the current or Nth previous output range."
  (interactive "P")
  (let ((prompt-ranges (mistty--prompt-ranges-for-current-or-previous-output n)))
    (goto-char (mistty--prompt-ranges-output-start prompt-ranges))
    (set-mark (mistty--prompt-ranges-output-end prompt-ranges))))

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
             (condition-case nil
                 (when-let* ((prompt (mistty--prompt-ranges-for-current-or-previous-output
                                     current-prefix-arg)))
                   (mistty--cmdline-for-output prompt))
               (error nil))
             "command output")
            30)))
          current-prefix-arg))
  (let ((prompt (mistty--prompt-ranges-for-current-or-previous-output n))
        (buffer (generate-new-buffer buffer-name)))
    (copy-to-buffer buffer
                    (mistty--prompt-ranges-output-start prompt)
                    (mistty--prompt-ranges-output-end prompt))
    (with-current-buffer buffer
      (set-auto-mode 'keep-mode-if-sane))
    (pop-to-buffer buffer)
    buffer))

(provide 'mistty-nav)

;;; mistty-nav.el ends here
