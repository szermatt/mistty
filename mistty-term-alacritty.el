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
  proc buf)

(cl-defmethod mistty--create-term ((_type (eql 'alacritty)) name command &key width height)
  "Create an alacritty-type terminal with the given NAME and COMMAND.

If WIDTH and HEIGHT are specified, they'll be used as terminal line and
column count. The default is 80x24."
  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks))
        (program (car command))
        (args (cdr command)))
    (with-current-buffer term-buffer
      (mistty-alacritty-mode)
      (setq-local mistty--prompt-cell (mistty--make-prompt-cell))
      (setq-local scroll-margin 0)
      (let ((process-environment
             (if (with-connection-local-variables mistty-set-EMACS)
                 (cons (format "EMACS=%s" emacs-version)
                       process-environment)
               process-environment)))
        (mistty-alacritty-exec name program args width height))
      (let* ((proc (get-buffer-process term-buffer))
             (term (mistty--make-term-alacritty :buf term-buffer :proc proc)))
        (set-process-filter proc (mistty--make-accumulator
                                  (mistty--term-filter-func term)))
        (process-put proc 'mistty-term term)

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
  (with-current-buffer (mistty--term-alacritty-buf term)
    (mistty-alacritty--alt-screen-p)))

(cl-defmethod mistty--term-lines ((term mistty--term-alacritty))
  "Return the number of lines in TERM (its height)."
  (with-current-buffer (mistty--term-alacritty-buf term)
    mistty-alacritty-lines))

(cl-defmethod mistty--term-columns ((term mistty--term-alacritty))
  "Return the number of columns in TERM (its width)."
  (with-current-buffer (mistty--term-alacritty-buf term)
    mistty-alacritty-columns))

(cl-defmethod mistty--term-cursor-linecol ((term mistty--term-alacritty))
  "Return the terminal line and column of the cursor in TERM."
  (with-current-buffer (mistty--term-alacritty-buf term)
    (mistty-alacritty--cursor-linecol)))

(cl-defmethod mistty--term-sentinel-func ((_term mistty--term-alacritty))
  "Return the default sentinel of the process.

The actual sentinel may different from this."
  #'mistty-alacritty--sentinel)

(cl-defmethod mistty--term-filter-func ((_term mistty--term-alacritty))
  "Return the default process filter of the process.

The actual process filter may different from this."
  #'mistty-alacritty--process-filter)

(cl-defmethod mistty--term-resize ((term mistty--term-alacritty) width height)
  "Resize TERM to WIDTH columns and HEIGHT lines."
  (with-current-buffer (mistty--term-alacritty-buf term)
    (mistty-alacritty-resize width height))
  (set-process-window-size (mistty--term-alacritty-proc term) height width))

(cl-defmethod mistty--term-autoresize ((term mistty--term-alacritty) enable)
  "Turn on or off TERM dimension tracking its window dimensions.

If ENABLE is non-nil, enable autoresize, otherwise disable it."
  (with-current-buffer (mistty--term-alacritty-buf term)
    (mistty-alacritty-auto-resize enable)))

(cl-defmethod mistty--term-setup-buffer ((_term mistty--term-alacritty) &optional _fullscreen)
  "Does nothing.")

(cl-defmethod mistty--term-setup-accum  ((term mistty--term-alacritty) accum
                                         &key enter-fullscreen active-prompt after-clear-screen)
  "Setup TERM's ACCUM.

ENTER-FULLSCREEN is to be called when entering fullscreen mode.
ACTIVE-PROMPT should return the active `mistty--prompt'.
AFTER-CLEAR-SCREEN is to be called right after the screen has been cleared."
  (mistty--add-prompt-detection accum term)
  (mistty--term-alacritty-add-osc-detection accum term)
  (unless enter-fullscreen (error ":enter-fullscreen required"))
  (mistty--accum-add-processor
   accum
   '(seq CSI (or "47" "?47" "?1047" "?1049") ?h)
   (lambda (ctx str)
     (mistty--accum-ctx-flush ctx)
     (funcall enter-fullscreen)
     (mistty--accum-ctx-push-down ctx str)))

  (unless active-prompt (error ":active-prompt required"))
  (mistty--accum-add-processor
   accum '(seq CSI ?2 ?J) ;; Clear screen
   (lambda (ctx str)
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

(cl-defmethod mistty--term-setup-accum-for-fullscreen ((term mistty--term-alacritty) accum
                                                       &key leave-fullscreen)
  "Setup TERM's ACCUM for fullescreen mode.

LEAVE-FULLSCREEN is to be called when leaving fullscreen mode."
  (mistty--term-alacritty-add-osc-detection accum term)
  (unless leave-fullscreen (error ":leave-fullscreen required"))
  (mistty--accum-add-processor
   accum
   '(seq CSI (or "47" "?47" "?1047" "?1049") ?l)
   (lambda (ctx str)
     (mistty--accum-ctx-push-down ctx str)
     (mistty--accum-ctx-flush ctx)
     (funcall leave-fullscreen))))

(cl-defmethod mistty--term-clear-to-eol ((_term mistty--term-alacritty) pos)
  "Mark spaces as cleared from POS to the end of the line."
  (mistty-alacritty--clear-to-eol pos))

(cl-defmethod mistty--term-cleanup-prompt-sp ((_term mistty--term-alacritty) pos)
  "Cleanup the prompt as POS after a prompt-sp hack."
  (mistty-alacritty--cleanup-prompt-sp pos))

(cl-defmethod mistty--term-changed ((_term mistty--term-alacritty) _beg _end)
  "Does nothing.")

(cl-defmethod mistty--term-after-refresh ((_term mistty--term-alacritty) beg)
  "Post-process work buffer from BEG to end after a refresh.

This marks the final newline as \\='empty-lines-at-eob."

  ;; When rendering, alacritty always render a final newline. Mark it.
  (let ((last-newline (1- (point-max))))
    (when (and (> last-newline beg)
               (eq ?\n (char-after last-newline)))
      (add-text-properties
       last-newline (point-max)
       '(mistty-skip empty-lines-at-eob yank-handler (nil "" nil nil))))))

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
   (unless (mistty--term-alt-screen-p term)
     (mistty-osc133 "133" text))))

(provide 'mistty-term-alacritty)

;;; mistty-term-alacritty.el ends here
