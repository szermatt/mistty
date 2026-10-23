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
;;
;; Use (mistty--register-prompt) to create and register new prompts.
(cl-defstruct (mistty--prompt
               (:constructor mistty--make-prompt))
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

(defun mistty--prompt-source< (a b)
  "Compare prompt sources A and B.

Comparison goes from the most trusted (lowest number) to the least
trusted (highest number).

Return non-nil if A < B."
  (cl-labels ((priority (source)
                (pcase source
                  ('osc133 0)
                  ('bracketed-paste 1)
                  ('prompt_sp 2)
                  ('regexp 3)
                  (_ 10))))
    (< (priority a) (priority b))))

(defun mistty--prompt-merge-from (p other)
  "Update P with values from OTHER.

P input-id slot is not modified."

  ;; Select the most trusted source
  (when (mistty--prompt-source<
         (mistty--prompt-source other) (mistty--prompt-source p))
    (setf (mistty--prompt-source p) (mistty--prompt-source other)))

  ;; Expand start and end to cover the union of both ranges.
  ;;
  ;; Expand the end when going from regexp (1 line) to other,
  ;; open-ended prompt type. Expand prompt start when merging
  ;; 'prompt_sp and and 'bracketed paste on Zsh, where bracketed
  ;; past is called on the last line of a multi-line prompt.
  (setf (mistty--prompt-start p)
        (min (mistty--prompt-start other)
             (mistty--prompt-start p)))
  (cond
   ((and (mistty--prompt-end other) (mistty--prompt-end p))
    (setf (mistty--prompt-end p)
          (max (mistty--prompt-end other)
               (mistty--prompt-end p))))
   ((null (mistty--prompt-end other))
    (setf (mistty--prompt-end p) nil)))

  ;; Text is a restriction only valid on 'regexp that captures
  ;; the very first prompt text seen. Mostly only clear it.
  (unless (and (eq 'regexp (mistty--prompt-source p))
               (mistty--prompt-text other)
               (mistty--prompt-text p))
    (setf (mistty--prompt-text p) nil))

  ;; User-input-start is provided explicitly. Always use the
  ;; latest value.
  (when-let* ((s (mistty--prompt-user-input-start other)))
    (setf (mistty--prompt-user-input-start p) s)))

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
  "Sets the value of the current `mistty--prompt' struct to VAL.

This also assigns the slot input-id of VAL.

The old value, if any, is pushed into `mistty--prompt-archive'."
  `(let ((val ,val)
         (c mistty--prompt-cell))
     (when val
       (setf (mistty--prompt-input-id val)
             (cl-incf (mistty--prompt-cell-counter c))))
     (when-let* ((old (mistty--prompt-cell-current c)))
       (push old (mistty--prompt-cell-archive c)))
     (setf (mistty--prompt-cell-current c) val)))

(cl-defun mistty--register-prompt (source start &key end text user-input-start)
  "Create and register a new prompt or merge it with the current one.

This function also logs the new or updated prompt.

SOURCE is the prompt source.
START is the scrolline of the prompt start position.
END is nil or the scrolline of the prompt end position.
TEXT is the prompt text.
USER-INPUT-START is (cons scrolline pos) of the start of user input.

Return the new prompt or the modified p prompt. In either case,
this is the same as what's returned by `mistty--prompt' after this call."
  (let* ((p (mistty--prompt))
         (other (mistty--make-prompt
                 :source source
                 :start start
                 :end end
                 :text text
                 :user-input-start user-input-start))
         (overlap (and p
                       (mistty--prompt-overlaps p other)
                       (<= (abs (- (mistty--prompt-start p)
                                   (mistty--prompt-start other)))
                           2))))
    (if overlap
        (mistty--prompt-merge-from p other)

      ;; Register the new prompt
      (setq p other)
      (setf (mistty--prompt) p))
    (mistty-log "%s %s prompt #%s [%s-%s]%s%s"
                (if overlap "Updated" "Detected")
                (mistty--prompt-source p)
                (mistty--prompt-input-id p)
                (mistty--prompt-start p)
                (or (mistty--prompt-end p) "")
                (if-let* ((text (mistty--prompt-text p)))
                  (format " :text %S" text) "")
                (if-let* ((s (mistty--prompt-user-input-start p)))
                  (format " :user-input-start %S" s) ""))))

(defun mistty--end-prompt (source end)
  "End the current prompt if it comes from SOURCE at END."
  (when-let* ((p (mistty--prompt)))
    (when (and (eq source (mistty--prompt-source p))
               (null (mistty--prompt-end p))
               (> end (mistty--prompt-start p)))
      (setf (mistty--prompt-end p) end))))

(gv-define-setter mistty--prompt-archive (val)
  "Sets the value of `mistty--prompt-archive'."
  `(setf (mistty--prompt-cell-archive mistty--prompt-cell) ,val))

(defun mistty--prompt-contains (prompt scrolline)
  "Return non-nil if SCROLLINE is inside of PROMPT."
  (and (>= scrolline (mistty--prompt-start prompt))
       (or (null (mistty--prompt-end prompt))
           (< scrolline (mistty--prompt-end prompt)))))

(defun mistty--prompt-overlaps (prompt other)
  "Return non-nil if PROMPT range overlaps with OTHER's."
  (or (mistty--prompt-contains
       prompt (mistty--prompt-start other))
      (mistty--prompt-contains
       other (mistty--prompt-start prompt))))

(defun mistty--add-regexp-prompt-detection (accum term)
  "Setup ACCUM for regexp prompt detection for TERM.

Detected prompts can be found in `mistty-prompt'."
  (mistty--accum-add-post-processor
   accum
   (let ((last-nonempty-scrolline 0))
     (lambda ()
       (when (mistty--term-detect-prompt-p term)
         (let ((scrolline (mistty--scrolline-at-point)))
           ;; Only look at new lines
           (when (> scrolline
                    (prog1 last-nonempty-scrolline
                      ;; for next time
                      (setq last-nonempty-scrolline
                            (mistty--scrolline-at
                             (mistty--last-non-ws)))))
             (let ((cursor (point))
                   (bos (mistty--scrolline-start-pos)))
               (when (and (> cursor bos)
                          (>= cursor (mistty--last-non-ws))
                          (string-match
                           mistty--prompt-regexp
                           (mistty--safe-bufstring bos cursor)))
                 (mistty--register-prompt
                  'regexp scrolline
                  :end (1+ scrolline)
                  :text (mistty--safe-bufstring bos (+ bos (match-end 0)))))))))))))

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


(defun mistty--add-toggle-cursor (accum &rest bufs)
  "Configure ACCUM to show/hide cursor in BUFS."
  (mistty--accum-add-processor
   accum
   '(seq CSI "?25h")
   (lambda (_ _)
     (mistty-log "Show cursor")
     (dolist (buf bufs)
       (mistty--with-live-buffer buf
         (mistty-log "Show Cursor in %s" major-mode)
         (mistty--show-cursor)))))
  (mistty--accum-add-processor
   accum
   '(seq CSI "?25l")
   (lambda (_ _)
     (mistty-log "Hide cursor")
     (dolist (buf bufs)
       (mistty--with-live-buffer buf
         (mistty-log "Hide cursor in %s" major-mode)
         (mistty--hide-cursor))))))

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

(defun mistty--term-scrolline-at-screen-start()
  "Scrolline at the top of the screen."
  mistty--scrolline-home-num)

(defun mistty-osc133 (_ osc-seq &optional send-clicks-func)
  "Handle OSC 133 codes.

OSC-SEQ contains the subcode followed optionally by a semi-colon and
arguments (ignored).

If SEND-CLICKS-FUNC is non-nil, it is called when click events are requested.

MisTTY supports code A-D:

 - A marks the start of a new command.
 - B marks the end of the prompt and the start of user input.
 - C marks the start of command output.
 - D marks the end of the command.

Everything else is ignored."
  (when (and (length> osc-seq 0))
    (let ((command-char (aref osc-seq 0)))
      (mistty-log "OSC 133 %s" osc-seq)
      (pcase command-char
        (?A ;; start a new command
         ;; Overwrite any other prompt source.
         (mistty--register-prompt 'osc133 (mistty--scrolline-at-point))
         (when (and send-clicks-func (string-match-p ";click_events=1" osc-seq))
           (funcall send-clicks-func t)))

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
             (setf (mistty--prompt-end prompt) (mistty--scrolline-at-point))))))

      (when (and send-clicks-func
                 (not (memq command-char '(?A ?B))))
        (funcall send-clicks-func nil)))))

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


(defun mistty--detect-change-before-scrolline (func term-buffer scrolline)
  "Return non-nil if FUNC modifies TERM-BUFFER above SCROLLINE.

FUNC is called in the caller's environment. It is supposed to modify
TERM-BUFFER.

Return non-nil if changes were detected."
  (let ((old-sync-position (mistty--with-live-buffer term-buffer
                             (mistty--find-scrolline scrolline))))
    ;; Reminder: call func with no buffer set, to avoid strange
    ;; breakages when the term buffer is killed.
    (funcall func)
    (mistty--with-live-buffer term-buffer
      (when-let* ((new-sync-position (mistty--with-live-buffer term-buffer
                                       (mistty--find-scrolline scrolline))))
        (/= old-sync-position new-sync-position)))))

(defvar-local mistty--point-marker nil
  "Marker (re)used by `mistty--sync-buffer' on Emacs 31 and later.")

(defun mistty--catchup
    (home-marker home-scrolline dest-buffer sync-pos sync-scrolline)
  "Catch up scrollback lines.

This function copies lines before HOME-MARKER, with scrolline
HOME-SCROLLINE, to DEST-BUFFER, at SYNC-POS. The line copies are those
between SYNC-SCROLLINE and HOME-SCROLLINE.

Returns new sync position and scrolline (cons SYNC-POS SYNC-SCROLLINE)."
  (cl-assert (< sync-scrolline home-scrolline))
  (mistty-log "Detected rapid scroll (sync @%s, home now @%s). Catching up."
              sync-scrolline home-scrolline)
  (let* ((source-buffer (current-buffer))
         (catchup-lines (- home-scrolline sync-scrolline))
         (catchup-start (save-excursion
                          (goto-char home-marker)
                          (pos-bol (1+ (- catchup-lines)))))
         (catchup-end (marker-position home-marker)))
    (mistty-log "Catchup [%s-%s] %s lines" catchup-start catchup-end catchup-lines)
    (cons (mistty--with-live-buffer dest-buffer
            (save-excursion
              (goto-char sync-pos)
              (let ((inhibit-modification-hooks t)
                    (inhibit-read-only t))
                (insert-buffer-substring
                 source-buffer catchup-start catchup-end))
              (point)))
          home-scrolline)))

(defun mistty--realign-buffers
    (source-buffer home-scrolline dest-buffer sync-pos sync-scrolline)
  "Reset sync position and scrolline after an unexpected change.

This function is called when something changed about the current sync
position, either because the position was set too low or because the
screen was overwritten by the currently running command. It does its
best to find align the two buffers while avoiding duplicated lines as
much as possible.

SOURCE-BUFFER is the terminal buffer, HOME-SCROLLINE the scrolline that
correspond to the top of the screen on the terminal.

DEST-BUFFER is the destination buffer to terminal data to at SYNC-POS,
which correspond to SYNC-SCROLLINE. The lines above SYNC-POS in
DEST-BUFFER may be annotated with \\='mistty-scrolline to specify the
scrolline the original data corresponds to; this function uses this
information to align the two buffers.

Return (cons SYNC-POS SYNC-SCROLLINE) with the new proposed values of
the sync position in DEST-BUFFER and the corresponding scrolline."
  (mistty--with-live-buffer dest-buffer
    (let ((prop (save-excursion
                  (goto-char sync-pos)
                  (text-property-search-backward
                   'mistty-scrolline
                   home-scrolline ;; goal
                   (lambda (goal val)
                     (and val (<= val goal)))))))
      (cond
       ;; Found exact scrolline, align there or below if rows match
       ((and prop (= home-scrolline (prop-match-value prop)))
        (pcase-setq `(,sync-pos . ,sync-scrolline)
                    (mistty--skip-identical-rows
                     (prop-match-beginning prop)
                     home-scrolline
                     source-buffer))
        (mistty-log "REALIGN scrolline %s to pos %s; terminal [%s-]"
                    sync-scrolline sync-pos home-scrolline))

       ;; Found scrolline < goal, align at line after
       (prop
        (let ((found-pos (mistty--bol (prop-match-beginning prop) 2)))
          (mistty-log "REALIGN APPROXIMATE scrolline %s to pos %s; terminal [%s-]"
                      home-scrolline found-pos home-scrolline)
          (setq sync-pos found-pos
                sync-scrolline home-scrolline)))

       ;; Couldn't find beginning. It might have been deleted. Sync
       ;; whole buffer.
       (t
        (mistty-log "REALIGN FALLBACK scrolline %s to point-min %s"
                    home-scrolline (point-min))
        (setq sync-pos (point-min)
              sync-scrolline home-scrolline))))

    (cons sync-pos sync-scrolline)))

(defun mistty--sync-buffer
    (source-buffer source-pos dest-buffer dest-pos keep-markers)
  "Copy SOURCE-BUFFER from SOURCE-POS to eob to DEST-BUFFER.

The text and text properties of the region from DEST-POS to eob is
overwritten. If KEEP-MARKERS is non-nil, the function attempt to keep
markers point and overlays as it rewrites the text."
  (if (not keep-markers)
      ;; Quicker version of sync-buffer that doesn't bother with
      ;; markers.
      (mistty--with-live-buffer dest-buffer
        (save-restriction
          (widen)
          (let ((old-point (when (< (point) dest-pos (point-max))
                             (point))))
            (goto-char dest-pos)
            (delete-region dest-pos (point-max))
            (insert-buffer-substring source-buffer source-pos)
            (if old-point
                ;; old-point was before dest-pos. Restore it.
                (goto-char old-point)
              ;; copy point position from source buffer, if possible
              (goto-char (+ dest-pos
                            (max 0 (with-current-buffer source-buffer
                                     (- (point) source-pos)))))))))

    ;; Complete but expensive version of sync-buffer that conserves
    ;; markers.
    (mistty--with-live-buffer source-buffer
      (save-restriction
        (narrow-to-region source-pos (point-max))
        (let ((properties (mistty--save-properties (point-min))))
          (with-current-buffer dest-buffer
            (let ((old-point (and (< (point) dest-pos) (point))))
              (save-restriction
                (narrow-to-region dest-pos (point-max))
                (if (eval-when-compile (>= emacs-major-version 31))
                    (let ((point-marker (or mistty--point-marker
                                            (setq mistty--point-marker (make-marker)))))
                      (set-marker point-marker (point))
                      (set-marker-insertion-type point-marker nil)
                      (replace-region-contents (point-min) (point-max) source-buffer 0.2)
                      (goto-char point-marker)
                      (move-marker point-marker nil))
                  ;; Before Emacs 31, replace-region-contents could
                  ;; not take a buffer as source.
                  (replace-buffer-contents source-buffer 0.2))
                (mistty--restore-properties properties (point-min)))

              ;; If the point was outside the sync region, restore it,
              ;; as it has been moved by narrow-to-region . Otherwise,
              ;; trust replace-buffer-contents to do something
              ;; reasonable with it.
              (when old-point
                (goto-char old-point)))))))))


(defun mistty--save-properties (start)
  "Extracts the properties from START in the current buffer.

Returns a list of (BEG END PROPERTIES), ordered, with positions
relative to START."
  (let ((pos start) intervals)
    (while (< pos (point-max))
      (let ((props (text-properties-at pos))
            (last-pos pos))
        (setq pos (next-property-change pos nil (point-max)))
        (push `(,(- last-pos start) ,(- pos start) ,props)
              intervals)))

    intervals))

(defun mistty--restore-properties (intervals start)
  "Apply saved properties INTERVALS to the buffer at START.

This is the reverse operation of `mistty--save-properties'."
  (pcase-dolist (`(,beg ,end ,props) intervals)
    (set-text-properties (+ beg start) (+ end start) props)))

(defun mistty--skip-identical-rows (pos scrolline source-buffer)
  "Skip rows from POS that are the same on SOURCE-BUFFER.

Return a (CONS new-pos new-scrolline), possibly modified values for POS
and SCROLLINE."
  (save-excursion
    (while
        (when (equal scrolline (get-text-property pos 'mistty-scrolline))
          (when-let* ((end-row (next-single-property-change pos 'mistty-scrolline)))
            (when (string= (string-trim (buffer-substring-no-properties pos end-row)
                                        "" "\n")
                           (mistty--with-live-buffer source-buffer
                             (save-excursion
                               (goto-char (mistty--find-scrolline scrolline))
                               (mistty--unwrapped-scrolline-text))))
              (setq pos end-row))))
      (goto-char pos)
      (cl-incf scrolline)))

  `(,pos . ,scrolline))

(provide 'mistty-term)

;;; mistty-term.el ends here
