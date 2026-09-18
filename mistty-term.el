;;; mistty-term.el --- Extensions for term.el for MisTTY -*- lexical-binding: t -*-

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
;; This file collects helpers for mistty.el that deal with the
;; terminal buffer.

(require 'pcase)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-scrolline)
(require 'mistty-util)
(require 'mistty-log)
(require 'mistty-accum)
(eval-when-compile
  (require 'mistty-accum-macros))
(require 'mistty-kbd)
(require 'mistty-osc7)
(require 'mistty-term-base)

;;; Code:

(defcustom mistty-set-EMACS nil
  "Whether the EMACS env variable should be set, for Bash 4.3 and older.

NOTE: Bash detecting that it's running under Emacs only works when using
term.el as Bash wants the TERM env variable to be set to eterm. When
using the module, only OSC7 work for directory tracking.

You can set set this if:
 - you're stuck using a very old version of Bash (4.3 or older)
 - you don't want to set up directory tracking using OSC7
   as described in the manual

When set, MisTTY sets the EMACS env variable, which Bash 4.3 and
older check to decide whether to send out directory tracking
information. (Newer version check INSIDE_EMACS instead.)

As this is usually host-specific, it can be set as a
connection-local variable. This might be useful when connecting
with TRAMP to hosts or docker instances that use a very old
version of Bash that you don't want to configure.

For example:

  (connection-local-set-profile-variables
   \\='profile-old-bash
   \\='((mistty-set-EMACS . t)
     (mistty-shell-command . (\"/bin/bash\" \"-i\"))))

  (connection-local-set-profiles \\='(:machine \"oldhost.example.com\")
   \\='profile-old-bash)
  (connection-local-set-profiles \\='(:protocol \"docker\")
   \\='profile-old-bash)"
  :group 'mistty
  :type 'boolean)

(defcustom mistty-multi-line-continue-prompts
  '("^    *\\.\\.\\.: " ; ipython
    )
  "Regexp used to identify multi-line command prompts.

These regexps identifies prompts that tell the user that they can
type more, while still allowing them to edit what's above. MisTTY
uses these regexps to identify sections of texts it should ignore
when editing.

Note that bash \"> \" is not a continuation prompt, with this
definition, because it doesn't allow editing what's above."
  :group 'mistty
  :type '(list regexp))

(defconst mistty-right-str "\eOC"
  "Sequence to send to the process when the rightarrow is pressed.")

(defconst mistty-left-str "\eOD"
  "Sequence to send to the process when the left arrow is pressed.")

(defconst mistty-up-str "\eOA"
  "Sequence to send to the process when the uparrow is pressed.")

(defconst mistty-down-str "\eOB"
  "Sequence to send to the process when the left arrow is pressed.")

(defvar-local mistty--term-properties-to-add-alist nil
  "An alist of id to text properties to add to the term buffer.

This variable associates arbitrary symbols to property lists. It
is set by `mistty-register-text-properties' and read whenever
text is written to the terminal.

This variable is available in the work buffer.")

(defvar-local mistty--original-cursor nil
  "The local value `cursor-type' had before it was hidden.

Will be nil even though the cursor is hidden if the cursor had no
local value. `mistty--show-cursor' then restores the global
value.

Used in `mistty--hide-cursor' and `mistty--show-cursor'.")

(defvar-local mistty--prompt-cell nil
  "A `mistty--prompt-cell' instance.

This is used to share prompts between the work and term buffers. This is
accessible from either buffer.

Always access it through the places `mistty--prompt'
`mistty--prompt-archive' and `mistty--prompt-counter'.")

(defconst mistty--prompt-regexp
  "[^[:alnum:][:cntrl:][:blank:]][[:blank:]]$"
  "Regexp used to identify prompts.

New, empty lines that might be prompts are evaluated against this
regexp. This regexp should match something that looks like the
end of a prompt with no commands.")

(cl-defstruct (mistty--prompt-cell
               (:constructor mistty--make-prompt-cell
                             (&aux (counter 0))))
  current
  archive
  counter)

;; A detected prompt.
;;
;; This datastructure is shared between the work and term buffer and
;; uses scrollines as units.
(cl-defstruct (mistty--prompt
               (:constructor mistty--make-prompt
                             (source start &optional end &key text
                                     &aux (input-id
                                           (progn
                                             (cl-incf (mistty--prompt-cell-counter
                                                       mistty--prompt-cell)))))))
  input-id

  ;; prompt source:
  ;;  - regexp
  ;;  - bracketed paste
  source

  ;; Non-nil once the prompt has been accepted by MisTTY
  realized

  ;; Start scrolline. Shouldn't be nil.
  start

  ;; End scrolline, or nil if prompt is open-ended.
  ;;
  ;; This is the first scrolline on which the prompt is *not* present, so
  ;; a single-line prompt starting at 10 would end at 11.
  end

  ;; Text of the prompt, used when source=regexp.
  text

  ;; Position of the start of the user input, if known.
  ;; This is a (cons scrolline column).
  user-input-start)

(defun mistty--prompt ()
  "Get the value of the current `mistty--prompt' struct or nil."
  (when-let* ((cell mistty--prompt-cell))
    (mistty--prompt-cell-current cell)))

(defun mistty--prompt-archive ()
  "Get the list of archived `mistty--prompt' structs."
  (when-let* ((cell mistty--prompt-cell))
    (mistty--prompt-cell-archive cell)))

(defun mistty--prompt-counter ()
  "Get the number of prompt instances created in this buffer."
  (when-let* ((cell mistty--prompt-cell))
    (mistty--prompt-cell-counter cell)))

(gv-define-setter mistty--prompt (val)
  "Sets the value of the current `mistty--prompt' struct.

The old value, if any, is pushed into `mistty--prompt-archive'."
  `(progn
     (when-let* ((old (mistty--prompt-cell-current mistty--prompt-cell)))
       (push old (mistty--prompt-cell-archive mistty--prompt-cell)))
     (setf (mistty--prompt-cell-current mistty--prompt-cell) ,val)))

(gv-define-setter mistty--prompt-archive (val)
  "Sets the value of `mistty--prompt-archive'."
  `(setf (mistty--prompt-cell-archive mistty--prompt-cell) ,val))

(defun mistty--prompt-contains (prompt scrolline)
  "Return non-nil if SCROLLINE is inside of PROMPT."
  (and (>= scrolline (mistty--prompt-start prompt))
       (or (null (mistty--prompt-end prompt))
           (< scrolline (mistty--prompt-end prompt)))))

(defun mistty--add-prompt-detection (accum term)
  "Register processors to ACCUM for prompt detection for TERM.

Detected prompts can be found in `mistty-prompt'."
  (mistty--accum-add-post-processor
   accum (mistty--regexp-prompt-detector))

  ;; Enable bracketed paste
  (mistty--accum-add-processor
   accum
   '(seq CSI "?2004h")
   (lambda (ctx _)
     (mistty--accum-ctx-flush ctx)
     (unless mistty-bracketed-paste
       (let* ((prompt (mistty--prompt))
              (inhibit-read-only t)
              (inhibit-modification-hooks t)
              (start-pos (pos-bol))
              (scrolline (mistty--scrolline-at-point)))
         (when (or (null prompt)
                   (memq (mistty--prompt-source prompt) '(regexp prompt_sp))
                   (not (mistty--prompt-contains prompt scrolline)))
           (cond
            ;; Zsh enables bracketed past after opening the prompt. Rely on prompt_sp
            ;; to find the beginning of possible multiline prompts.
            ((and prompt
                  (eq 'prompt_sp (mistty--prompt-source prompt))
                  (mistty--prompt-contains prompt scrolline)
                  (< (mistty--prompt-start prompt) scrolline))
             (when-let* ((start-scrolline (mistty--prompt-start prompt))
                         (pos (mistty--find-scrolline start-scrolline)))
               (setq start-pos pos)
               (mistty-log "Reusing prompt_sp start %s@%s" start-scrolline start-pos)
               (setq scrolline start-scrolline))))
           (setq prompt (mistty--make-prompt 'bracketed-paste scrolline))
           (mistty-log "Detected %s prompt #%s [%s-]"
                       (mistty--prompt-source prompt)
                       (mistty--prompt-input-id prompt)
                       (mistty--prompt-start prompt))
           (when (> (pos-eol) start-pos)
             (mistty--term-changed term start-pos (pos-eol)))
           (setf (mistty--prompt) prompt))
         (unless (eq 'osc133 (mistty--prompt-source prompt))
           (setf (mistty--prompt-source prompt) 'bracketed-paste)
           (setf (mistty--prompt-end prompt) nil)))
       (setq mistty-bracketed-paste t))))

  ;; Disable bracketed paste
  (mistty--accum-add-processor
   accum
   '(seq CSI "?2004l")
   (lambda (ctx _)
     (mistty--accum-ctx-flush ctx)
     (when mistty-bracketed-paste
       (when-let* ((prompt (mistty--prompt))
                  (scrolline (if (eq ?\n (char-before (point)))
                                 (mistty--scrolline-at-point)
                               (1+ (mistty--scrolline-at-point)))))
         (when (and (eq 'bracketed-paste (mistty--prompt-source prompt))
                    (null (mistty--prompt-end prompt))
                    (> scrolline (mistty--prompt-start prompt)))
           (setf (mistty--prompt-end prompt) scrolline)))
       (setq mistty-bracketed-paste nil))))

  ;; Detect prompt-sp as many spaces followed by CR at the end of a
  ;; line.
  ;;
  ;; Not using " \r" as regexp for the processor as it would mean
  ;; waiting after a space in case a \r eventually comes. This isn't
  ;; an escape sequence.
  (mistty--accum-add-processor
   accum
   'CR
   (lambda (ctx _)
     ;; If we received at least 8 spaces before the \r (enough to fill
     ;; the look-back buffer) flush and look at the state of the
     ;; buffer just before the \r is taken into account.
     (when (string= "        " (mistty--accum-ctx-look-back ctx))
       (mistty--accum-ctx-flush ctx)
       (when (or (and (= (1- (mistty--term-columns term))
                         (cdr (mistty--term-cursor-linecol term)))
                      (eq ?\  (char-before (point))))
                 (and (get-text-property (pos-eol 0) 'term-line-wrap)
                      (string-match "^ *$" (buffer-substring (pos-bol) (pos-eol)))))
         (let* ((prompt (mistty--prompt))
                (pos (pos-bol))
                (scrolline (mistty--scrolline-at pos)))
           (when (get-text-property (pos-eol 0) 'term-line-wrap)
             (mistty--term-cleanup-prompt-sp term (point))
             (let* ((eol (pos-eol 0))
                    (pos eol)
                    (inhibit-read-only t)
                    (inhibit-modification-hooks t))
               (remove-text-properties eol (1+ eol) '(term-line-wrap nil))
               (while (eq ?\  (char-before pos))
                 (cl-decf pos))
               (when (> eol pos)
                 (add-text-properties pos eol '(mistty-blank t))))
             (cl-incf scrolline))
           (when (or (null prompt)
                     (not (mistty--prompt-contains prompt scrolline)))
             (setq prompt (mistty--make-prompt 'prompt_sp scrolline))
             (setf (mistty--prompt) prompt)
             (mistty-log "Suspected %s prompt #%s: [%s,)"
                            (mistty--prompt-source prompt)
                            (mistty--prompt-input-id prompt)
                            (mistty--prompt-start prompt))))))
     (mistty--accum-ctx-push-down ctx "\r"))))

(defun mistty--regexp-prompt-detector ()
  "Build a post-processor that look for a new prompt at cursor.

The return value is meant to be
`mistty--accum-add-post-processor'.

 The post-processor updates `mistty--prompt' after the content of the
terminal buffer has been updated."
  (let ((last-nonempty-scrolline 0))
    (lambda ()
      (let ((scrolline (mistty--scrolline-at-point)))
        ;; Only look at new lines
        (when (> scrolline
                 (prog1 last-nonempty-scrolline
                   ;; for next time
                   (setq last-nonempty-scrolline
                         (mistty--scrolline-at
                          (mistty--last-non-ws)))))
          (let ((cursor (point))
                (bos (mistty--scrolline-start-pos))
                (prompt (mistty--prompt)))
            (when (and (or (null prompt)
                           (and (mistty--prompt-end prompt)
                                (>= scrolline (mistty--prompt-end prompt))))
                       (> cursor bos)
                       (>= cursor (mistty--last-non-ws))
                       (string-match
                        mistty--prompt-regexp
                        (mistty--safe-bufstring bos cursor)))
              (let ((prompt (mistty--make-prompt
                             'regexp scrolline (1+ scrolline)
                             :text (mistty--safe-bufstring bos (+ bos (match-end 0))))))
                (setf (mistty--prompt) prompt)
                (mistty-log "Suspected %s prompt #%s: [%s-%s] '%s'"
                            (mistty--prompt-source prompt)
                            (mistty--prompt-input-id prompt)
                            (mistty--prompt-start prompt)
                            (mistty--prompt-end prompt)
                            (mistty--prompt-text prompt))))))))))

(defun mistty-register-text-properties (id props)
  "Add PROPS to any text written to the terminal.

Call `mistty-unregister-text-properties' with the same ID to turn
that off.

If this function is called more than once with the same ID, only
the last set of properties to be registered is applied."
  (unless (eq 'term-mode major-mode) (error "Requires a term-mode buffer"))
  (if-let* ((cell (assq id mistty--term-properties-to-add-alist)))
      (setcdr cell props)
    (push (cons id props) mistty--term-properties-to-add-alist)))

(defun mistty-unregister-text-properties (id)
  "Stop applying properties previously registered with ID."
  (unless (eq 'term-mode major-mode) (error "Requires a term-mode buffer"))
  (when-let* ((cell (assq id mistty--term-properties-to-add-alist)))
    (setq mistty--term-properties-to-add-alist
          (delq cell
                mistty--term-properties-to-add-alist))))

(defun mistty--hide-cursor ()
  "Temporarily hide the cursor.

Does nothing if the cursor is already hidden."
  (when cursor-type
    (if (local-variable-p 'cursor-type)
        (setq mistty--original-cursor cursor-type)
      (setq mistty--original-cursor nil))
    (setq cursor-type nil)))

(defun mistty--show-cursor ()
  "Show the cursor again, after `mistty--hide-cursor'.

Does nothing if the cursor is already shown."
  (when (and (local-variable-p 'cursor-type) (null cursor-type))
    (if mistty--original-cursor
        (setq cursor-type mistty--original-cursor
              mistty--original-cursor nil)
      (kill-local-variable 'cursor-type))))

(defun mistty--clear-term-face-value (value)
  "Clean a font-lock-face VALUE generated by term.el.

This just removes the default background and foreground, as set
on the term face."
  (pcase value
    (`((:foreground ,fg :background ,bg . ,more-props) . ,rest)
     (let ((props more-props))
       (unless (equal fg (face-foreground 'term nil 'default))
         (setq props (plist-put props :foreground fg)))
       (unless (equal bg (face-background 'term nil 'default))
         (setq props (plist-put props :background bg)))
       (append (list props) rest '(term))))
    (_ value)))

(defun mistty--detect-dead-spaces-after-insert (term content beg)
  "Mark dead trailing spaces left in TERM after inserting CONTENT.

When inserting a newline in an existing line, the terminal often just
overwrites the existing characters with space instead of re-creating the
line properly. The result are spaces that should be skipped.

BEG is the position at which CONTENT was inserted in the terminal
buffer.

Detected dead spaces are marked with the text property \\='mistty-skip
\\='dead."
  (let ((lines (split-string content "\n")))
    (when (length> lines 1)
      (let ((first-line (car lines)))
        (let ((real-trailing-ws 0))
          (while (string-suffix-p " " first-line)
            (cl-incf real-trailing-ws)
            (setq first-line (substring first-line 0 -1)))
          (save-excursion
            (goto-char beg)
            (let ((eol (pos-eol)))
              (goto-char eol)
              (skip-chars-backward " " (1- beg))
              (dotimes (_ real-trailing-ws)
                (when (eq ?\  (char-after (point)))
                  (goto-char (1+ (point)))))
              (let ((inhibit-read-only t)
                    (inhibit-modification-hooks t))
                (when (> eol (point))
                  (mistty-log "@%s [%s-%s) %s dead spaces, %s real"
                              (point) (pos-bol) eol (- eol (point)) real-trailing-ws)
                  (mistty--term-clear-to-eol term (point))
                  ;; in case the buffer is accessed before rendering again
                  (put-text-property (point) eol 'mistty-skip 'dead))))))))))

(defun mistty--term-reset-scrolline (scrolline)
  "Make the screen start at SCROLLINE.

This is useful after a reset, where scrolline have been lost. Generally,
this allows arbitrarily manipulating the alignment between the work and
terminal buffers. To avoid issues with prompt locations, it should only
be used to increase the value of `mistty--scrolline-base'."
  (setq mistty--scrolline-home-num scrolline))

(defun mistty--term-scrolline-at-screen-start()
  "Scrolline at the top of the screen."
  mistty--scrolline-home-num)

(defun mistty-osc133 (_ osc-seq)
  "Handle OSC 133 codes.

OSC-SEQ contains the subcode followed optionally by a semi-colon and
arguments (ignored).

MisTTY supports code A-D:

 - A marks the start of a new command.
 - B marks the end of the prompt and the start of user input.
 - C marks the start of command output.
 - D marks the end of the command.

Everything else is ignored."
  (when (and (length> osc-seq 0))
    (let ((command-char (aref osc-seq 0)))
      (mistty-log "OSC 133 %c" command-char)
      (pcase command-char
        (?A ;; start a new command
         ;; Overwrite any other prompt source.
         (let ((prompt (mistty--make-prompt 'osc133 (mistty--scrolline-at-point))))
           (setf (mistty--prompt) prompt)
           (mistty-log "Detected %s prompt #%s [%s-]"
                       (mistty--prompt-source prompt)
                       (mistty--prompt-input-id prompt)
                       (mistty--prompt-start prompt))))

        (?B ;; end of prompt/start of user input
         (when-let* ((prompt (mistty--prompt)))
           (setf (mistty--prompt-user-input-start prompt)
                 (cons (mistty--scrolline-at-point)
                       (- (point) (pos-bol))))))

        (?C ;; start of command output
         (when-let* ((prompt (mistty--prompt)))
           (when (eq 'osc133 (mistty--prompt-source prompt))
             (mistty-log "Closed %s prompt #%s [%s-]"
                         (mistty--prompt-source prompt)
                         (mistty--prompt-input-id prompt)
                         (mistty--prompt-start prompt))
             (setf (mistty--prompt-end prompt) (mistty--scrolline-at-point)))))

        (?D ;; end of command (possible anytime after ?A)
         (when-let* ((prompt (mistty--prompt)))
           (when (and (eq 'osc133 (mistty--prompt-source prompt))
                      (null (mistty--prompt-end prompt)))
             (mistty-log "Aborted %s prompt #%s [%s-]"
                         (mistty--prompt-source prompt)
                         (mistty--prompt-input-id prompt)
                         (mistty--prompt-start prompt))
             (setf (mistty--prompt-end prompt) (mistty--scrolline-at-point)))))))))

(defun mistty-call-term-mode-hook ()
  "Call the functions registered to `term-mode-hook'.

Remove this hook from `mistty-term-mode-hook' to allow the terminal
modes started by MisTTY to have a completely separate setup from normal
terminal modes. See the documentation of `mistty-term-mode-hook' for
details."
  (run-hooks 'mistty-shadowed-term-mode-hook))

(defun mistty--truncate-buffer (home-marker)
  "Truncate the buffer before HOME-MARKER.

If there's more than 1000 chars in the buffer before the given marker,
truncate the buffer at a BOL, leaving at least 200 chars."
  (let ((chars (- home-marker (point-min))))
    (when (>= chars 1000)
      (save-excursion
        (goto-char (- home-marker 200))
        (goto-char (pos-bol))
        (unless (bobp)
          (mistty-log "[term] truncate %s chars of scrollback, leaving %s."
                      (- (point) (point-min))
                      (- home-marker (point)))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (delete-region (point-min) (point))))))))

(provide 'mistty-term)

;;; mistty-term.el ends here
