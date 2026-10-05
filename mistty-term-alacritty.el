;;; mistty-term-alacritty.el --- Use the alacritty library to run the terminall -*- lexical-binding: t -*-

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
;; This file implements generic methods defined in mistty-term-base.el
;; on top of the module using the alacritty library. The resulting
;; terminal is an alacritty terminal (TERM=alacritty).

(require 'cl-lib)
(require 'mistty-term-base)
(require 'mistty-alacritty)
(require 'mistty-term)
(require 'mistty-accum)
(require 'mistty-scrolline)

;;; Code:

(eval-when-compile
  (require 'mistty-accum-macros))

(cl-defstruct (mistty--term-alacritty
               (:constructor mistty--make-term-alacritty)
               (:copier nil))
  ;; associated process
  proc

  ;; process buffer a.k.a. term buffer
  buf

  ;; virtual terminal, from mistty-alacritty-vt module
  vterm

  ;; current terminal width
  columns

  ;; current terminal height
  lines

  ;; if non-nil, a change before scrolline was detected
  change-before-scrolline

  ;; if non-nil, the kitty keyboard protocol is enabled
  kkp

  ;; if non-nil, we are in fullscreen mode
  fs)

(cl-defmethod mistty--create-term
  ((_type (eql 'alacritty)) name command
   &key width height
   enter-fullscreen
   leave-fullscreen
   active-prompt
   after-clear-screen
   sync-scrolline)
  "Create an alacritty-type terminal with the given NAME and COMMAND.

If WIDTH and HEIGHT are specified, they'll be used as terminal line and
column count. The default is 80x24."
  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks))
        (program (car command))
        (args (cdr command))
        (width (or width 80))
        (height (or height 24)))
    (with-current-buffer term-buffer
      (mistty-alacritty-mode)
      (setq-local mistty--prompt-cell (mistty--make-prompt-cell))
      (setq-local scroll-margin 0)
      (let ((process-environment
             (if (with-connection-local-variables mistty-set-EMACS)
                 (cons (format "EMACS=%s" emacs-version)
                       process-environment)
               process-environment)))
        (mistty-alacritty--exec name program args width height))
      (let* ((proc (get-buffer-process term-buffer))
             (term (mistty--make-term-alacritty
                    :buf term-buffer
                    :proc proc
                    :vterm (mistty-alacritty--make-vterm width height)
                    :lines height
                    :columns width))
             (accum (mistty--make-accumulator
                     (mistty--term-alacritty-filter term))))
        (set-process-filter proc accum)
        (process-put proc 'mistty-term term)

        (mistty--add-prompt-detection accum term)
        (mistty--term-alacritty-add-osc-detection accum term)
        (unless enter-fullscreen (error ":enter-fullscreen required"))
        (mistty--accum-add-processor
         accum
         '(seq CSI (or "47" "?47" "?1047" "?1049") ?h)
         (lambda (ctx str)
           (unless (mistty--term-alacritty-fs term)
             (mistty--accum-ctx-flush ctx)
             (funcall enter-fullscreen nil)
             (setf (mistty--term-alacritty-fs term) t))
           (mistty--accum-ctx-push-down ctx str)))

        (unless leave-fullscreen (error ":leave-fullscreen required"))
        (mistty--accum-add-processor
         accum
         '(seq CSI (or "47" "?47" "?1047" "?1049") ?l)
         (lambda (ctx str)
           (mistty--accum-ctx-push-down ctx str)
           (when (mistty--term-alacritty-fs term)
             (mistty--accum-ctx-flush ctx)
             (setf (mistty--term-alacritty-fs term) nil)
             (funcall leave-fullscreen))))

        (unless active-prompt (error ":active-prompt required"))
        (mistty--accum-add-processor
         accum '(seq CSI ?2 ?J) ;; Clear screen
         (lambda (ctx str)
           (if (mistty--term-alacritty-fs term)
               (mistty--accum-ctx-push-down ctx str)

             (mistty--accum-ctx-flush ctx)
             (if (when-let* ((p (funcall active-prompt)))
                   (equal
                    (mistty--prompt-start p)
                    (mistty--with-live-buffer (mistty--term-alacritty-buf term)
                      mistty--scrolline-home-num)))
                 (progn
                   (mistty-log "CLEAR PROMPT (%S)" str)
                   (mistty--accum-ctx-push-down
                    ctx
                    ;; This is equivalent to CSI 2J, but doesn't trigger
                    ;; alacritty's storing the current screen content into
                    ;; scrollback.
                    "\e[1J\e[0J"))
               (mistty-log "CLEAR SCREEN (%S)" str)
               (mistty--accum-ctx-push-down ctx str)
               (mistty--accum-ctx-flush ctx)
               (when after-clear-screen
                 (funcall after-clear-screen))))))

        ;; Detect changes made to the terminal above the sync scrolline, which
        ;; means that the sync scrolline needs to be updated.
        ;; TODO: re-think and move at least partially into the module.
        (mistty--accum-add-around-process-filter
         accum
         (lambda (func)
           (if (mistty--term-alacritty-fs term)
               (funcall func)

             (let ((limit (funcall sync-scrolline)))
               (when (mistty--detect-change-before-scrolline
                      func (mistty--term-alacritty-buf term) limit)
                 (mistty-log "DETECTED BUFFER CHANGE, above %s" limit)
                 (setf (mistty--term-alacritty-change-before-scrolline term) t))))))

        term))))

(cl-defmethod mistty--term-buf ((term mistty--term-alacritty))
  "Return TERM's associated terminal/process buffer."
  (mistty--term-alacritty-buf term))

(cl-defmethod mistty--term-proc ((term mistty--term-alacritty))
  "Return TERM's associated process."
  (mistty--term-alacritty-proc term))

(cl-defmethod mistty--term-screen-top-pos ((term mistty--term-alacritty))
  "Return the position of the terminal in TERM's terminal buffer."
  (with-current-buffer (mistty--term-alacritty-buf term)
    mistty-alacritty--home))

(cl-defmethod mistty--term-screen-top-scrolline ((term mistty--term-alacritty))
  "Return the scrolline number of the first line of TERM's terminal."
  (with-current-buffer (mistty--term-alacritty-buf term)
    mistty--scrolline-home-num))

(cl-defmethod mistty--term-alt-screen-p ((term mistty--term-alacritty))
  "Return non-nil if TERM is showing the alt buffer."
  (mistty-alacritty-vt-alt-screen-p (mistty--term-alacritty-vterm term)))

(cl-defmethod mistty--term-detect-prompt-p ((term mistty--term-alacritty))
  "Return non-nil prompt detection should be enabled in TERM."
  (not (mistty--term-alacritty-fs term)))

(cl-defmethod mistty--term-lines ((term mistty--term-alacritty))
  "Return the number of lines in TERM (its height)."
  (mistty--term-alacritty-lines term))

(cl-defmethod mistty--term-columns ((term mistty--term-alacritty))
  "Return the number of columns in TERM (its width)."
  (mistty--term-alacritty-columns term))

(cl-defmethod mistty--term-cursor-linecol ((term mistty--term-alacritty))
  "Return the terminal line and column of the cursor in TERM."
  (mistty-alacritty-vt-cursor (mistty--term-alacritty-vterm term)))

(cl-defmethod mistty--term-sentinel-func ((_term mistty--term-alacritty))
  "Return the default sentinel of the process.

The actual sentinel may different from this."
  #'mistty-alacritty--sentinel)

(cl-defmethod mistty--term-resize ((term mistty--term-alacritty) width height)
  "Resize TERM to WIDTH columns and HEIGHT lines."
  (if (or (/= (mistty--term-alacritty-columns term) width)
          (/= (mistty--term-alacritty-lines term) height))
      (when-let* ((vterm (mistty--term-alacritty-vterm term))
                  (proc (mistty--term-alacritty-proc term)))
        (mistty-log "RESIZE: %s lines %s columns" height width)
        (mistty-alacritty-vt-resize vterm width height)
        (setf (mistty--term-alacritty-columns term) width)
        (setf (mistty--term-alacritty-lines term) height)
        (set-process-window-size proc height width))))

(cl-defmethod mistty--term-autoresize ((_term mistty--term-alacritty) _enable)
  "Ignored as mistty-alacritty-mode buffers don't support auto-resize.")

(cl-defmethod mistty--term-setup-buffer ((_term mistty--term-alacritty) &optional _fullscreen)
  "Does nothing.")

(cl-defmethod mistty--term-sync
  ((term mistty--term-alacritty) dest-buffer sync-pos sync-scrolline keep-markers
   cursor-marker)
  (if (mistty--term-alacritty-fs term)
      ;; fullscreen mode, without support for prompts
      (mistty--with-live-buffer dest-buffer
        (let ((vterm (mistty--term-alacritty-vterm term)))
          (save-excursion
            (mistty-log "RENDER FULLSCREEN")
            (goto-char sync-pos)
            (mistty-alacritty-vt-render-screen vterm cursor-marker)
            (put-text-property sync-pos (point-max) 'read-only t))))

    ;; normal mode, with support for prompts
    (mistty--with-live-buffer (mistty--term-alacritty-buf term)
      (let ((home-marker mistty-alacritty--home)
            (home-scrolline mistty--scrolline-home-num)
            (proc (mistty--term-alacritty-proc term))
            (source-buffer (current-buffer)))
        ;; Detect shenanigans and update sync-pos and sync-scrolline accordingly
        (cond
         ((< sync-scrolline home-scrolline)
          (pcase-setq
           `(,sync-pos . ,sync-scrolline)
           (mistty--catchup home-marker home-scrolline dest-buffer sync-pos sync-scrolline)))
         ((mistty--term-alacritty-change-before-scrolline term)
          (mistty-log "Detected terminal change above sync mark, at scrolline %s"
                      mistty--scrolline-home-num)
          (pcase-setq
           `(,sync-pos . ,sync-scrolline)
           (mistty--realign-buffers
            source-buffer home-scrolline dest-buffer sync-pos sync-scrolline))))

        (setf (mistty--term-alacritty-change-before-scrolline term) nil)

        (let ((source-sync-pos (mistty--find-scrolline sync-scrolline)))
          (mistty--sync-buffer
           source-buffer source-sync-pos
           dest-buffer sync-pos
           keep-markers)

          (mistty--with-live-buffer dest-buffer
            (set-marker cursor-marker
                        (+ sync-pos
                           (- (process-mark proc) source-sync-pos)))

            ;; When rendering, alacritty always render a final newline. Mark it.
            (let ((last-newline (1- (point-max))))
              (when (and (> last-newline sync-pos)
                         (eq ?\n (char-after last-newline)))
                (add-text-properties
                 last-newline (point-max)
                 '(mistty-skip empty-lines-at-eob
                               yank-handler (nil "" nil nil))))))))))

  (cons sync-pos sync-scrolline))

(cl-defmethod mistty--term-clear-to-eol ((term mistty--term-alacritty) pos)
  "Mark spaces as cleared from POS to the end of the line."
  (let ((vterm (mistty--term-alacritty-vterm term)))
    (mistty--with-live-buffer (mistty--term-alacritty-buf term)
      (when (> pos mistty-alacritty--home)
        (mistty-alacritty-vt-clear-to-eol vterm
                                          (mistty--count-lines mistty-alacritty--home pos)
                                          (- pos (mistty--bol pos)))))))

(cl-defmethod mistty--term-cleanup-prompt-sp ((term mistty--term-alacritty) pos)
  "Cleanup the prompt as POS after a prompt-sp hack."
  (let ((vterm (mistty--term-alacritty-vterm term)))
    (mistty--with-live-buffer (mistty--term-alacritty-buf term)
      (when (> pos mistty-alacritty--home)
        (mistty-alacritty-vt-cleanup-prompt-sp
         vterm
         (mistty--count-lines mistty-alacritty--home pos))))))

(cl-defmethod mistty--term-changed ((_term mistty--term-alacritty) _beg _end)
  "Does nothing.")

(cl-defmethod mistty--term-truncate-buffer ((term mistty--term-alacritty) scrolline-limit)
  "Truncate the terminal buffer, if necessary.

Always keep SCROLLINE-LIMIT and below."
  (with-current-buffer (mistty--term-alacritty-buf term)
    (when (>= scrolline-limit mistty--scrolline-home-num)
      (mistty--truncate-buffer mistty-alacritty--home))))

(defun mistty--term-alacritty-add-osc-detection (accum term)
  "Register handlers for OSC sequences in ACCUM for TERM."

  ;; This intercepts just a few OSC sequences not supported by the
  ;; alacritty, let others through.
  (mistty--accum-add-processor-lambda
   accum
   (_ctx '(seq OSC "7;" (let text Pt) ST))
   (mistty-osc7 "7" text))
  (mistty--accum-add-processor-lambda
   accum
   (ctx '(seq OSC "133;" (let text Pt) ST))
   (mistty--accum-ctx-flush ctx) ;; for accurate cursor pos
   (unless (mistty--term-alacritty-fs term)
     (mistty-osc133 "133" text))))

(cl-defmethod mistty--term-clear-scrollback ((term mistty--term-alacritty))
  "Clear any scrollback still stored in the process buffer or vterm."
  (with-current-buffer (mistty--term-alacritty-buf term)
    (when-let* ((home mistty-alacritty--home))
      (when (> home (point-min))
        (delete-region (point-min) home)))
    (mistty-alacritty-vt-clear-scrollback
     (mistty--term-alacritty-vterm term))))

(cl-defmethod mistty--term-translate-key ((_term mistty--term-alacritty) key n)
  "Generate the key byte sequence for TERM.

KEY is an Emacs key event and n the number of repetition for that event.

The function returns the byte sequence appropriate for sending that key
to the terminal."
  (mistty--translate-key-default key n mistty-alacritty--key-map))

(cl-defmethod mistty--term-list-special-keys ((_type (eql 'alacritty)))
  "List the basic type of special event types (special keys)."
  (mistty--list-basic-types-from-map mistty-alacritty--key-map))

(defun mistty--term-alacritty-filter (term)
  "Build a filter function for TERM.

The filter updates the vterm and optionally renders to the terminal
buffer."
  (lambda (proc str)
    (mistty-log "RECV %S" str)
    (let ((vterm (mistty--term-alacritty-vterm term))
          (buf (mistty--term-alacritty-buf term)))
      (dolist (ev (mistty-alacritty-vt-process-bytes vterm (vconcat str)))
        (pcase ev
          (`(pty-write ,data)
           (mistty-log "REPLY %S" data)
           (process-send-string proc data))
          (`(title ,title)
           (mistty-log "TITLE %S" title)
           (mistty--with-live-buffer buf
             (setq ansi-osc-window-title title)))
          (`(kkp ,val)
           (setf (mistty--term-alacritty-kkp term) val))))
      (unless (mistty--term-alacritty-fs term)
        (mistty--with-live-buffer buf
          (mistty-alacritty--render vterm))))))

(provide 'mistty-term-alacritty)

;;; mistty-term-alacritty.el ends here
