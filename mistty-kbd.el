;;; mistty-kbd.el --- Keyboard utilities for MisTTY -*- lexical-binding: t -*-

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
;; This file collects helpers and map for handling keyboards. This is
;; normally accessed through mistty.el.

;;; Code:
(require 'mistty-log)

(defcustom mistty-exit-capture-keyboard-key "C-g"
  "Key that ends `mistty-capture-keyboard'.

While it is running, `mistty-capture-keyboard' sends all keys to the
terminal, all except this specific key or key combination, which exits
the mode.

It must be a single key or key combination reported by `read-key', not a
key sequence.

It's also possible to exit keyboard capture with the mouse, clicking
anywhere, or programatically by calling `mistty-exit-capture-keyboard'."
  :group `mistty
  :type 'key)

(defconst mistty-del "\C-h"
  "Sequence to send to the process when backspace is pressed.

Both BS (^H) and DEL (^?) have been used to map to backspace, though
inconsistently across terminals. Mistty 1.3 and earlier sent DEL, but
that was switched to BS when Fish 4 started interpreting that as the
delete key instead of the backspace key.")

(defvar mistty-translation-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "ESC") (kbd "<escape>"))
    (define-key map (kbd "RET") (kbd "<return>")) ;; a.k.a. C-m
    (define-key map (kbd "TAB") (kbd "<tab>")) ;; a.k.a. C-i
    (define-key map (kbd "DEL") (kbd "<backspace>"))
    (define-key map (kbd "<backtab>") (kbd "S-<tab>"))

    map)
  "Translation applied to input keys by `mistty-send-key'.

This map standardize some terminal-based or historical keyboard
bindings, such as ESC, RET and <backtab> so that further translation can
just support the symbol-based key definition and ignore the
ASCII one.

Note that ESC <char> is translated into M-<char> before the key even
gets to this keymap.")

(defvar mistty-term-key-map (make-sparse-keymap)
  "Maps keys to the corresponding sequence to send to the terminal.

This map can be used to customize the byte sequences returned by
`mistty-send-key'. Any mapping added here overrides the default key
sequences for the current terminal type.")

(defvar mistty-start-capture-keyboard-hook nil
  "Hooks run when `mistty-capture-keyboard' starts.

Failures interrupt `mistty-capture-keyboard'.")

(defvar mistty-end-capture-keyboard-hook nil
  "Hooks run when `mistty-capture-keyboard' has ended.")

(defvar-local mistty-bracketed-paste nil
  "Whether bracketed paste is enabled in the buffer's terminal.

This variable is non-nil when bracketed paste is turned on by the
command that controls.")

(defvar-local mistty--send-function #'mistty--send-default
  "Function to use to send a string to the right process.

This allows calling `mistty-send-key' `mistty-send-last-key'
`mistty-capture-keyboard' or `mistty-translate-key' on non-mistty
buffers, as long as they either have a process or have redefined this
function..

The function should take 4 arguments STR KEY N POSITIONAL. STR is a
translated string to send to the terminal. If non-nil, KEY is the
original Emacs key that was transformed into STR. If non-nil, N is the
number of times KEY appears in STR, for repeated keys. If POSITIONAL is
non-nil, this tells the `mistty-mode' buffer to move the point to the
cursor.

The default implementation just sends to the buffer process.")

(defvar-local mistty--translate-key-function
  #'mistty--translate-key-default
  "Function to use to translate a key into bytes.

Takes a key and a number of repetitions. The key is that's passed to
this function has already been pre-translated with
`mistty-translation-keymap' to standardize around symbol-based
representations for ESC, TAB and RET and ESC <char> have been converted
to M-<char>.")

(defvar mistty--capture-keyboard-active nil
  "This boolean is set globally by `mistty-capture-keyboard'.")

(defun mistty--send-default (str _key _n _positional)
  "Send STR to the buffer process.

This is meant to be used as default for `mistty--send-function'. It works
on any buffer that has a process associated to it."
  (process-send-string (get-buffer-process (current-buffer)) str))

(defun mistty-send-key (&optional n key positional)
  "Send the current key sequence to the terminal.

This command sends N times the current key sequence, or KEY if it is
specified, directly to the terminal. In a `mistty-mode' buffer, if the
key sequence is positional or if POSITIONAL evaluates to true, MisTTY
attempts to move the terminal's cursor to the current point.

KEY must be a string or vector as would be returned by `kbd'.

This command is available in fullscreen mode."
  (interactive "p")
  (let* ((key (or key (this-command-keys-vector)))
         (translated-key (mistty-translate-key key n)))
    (funcall mistty--send-function translated-key key n positional)))

(defun mistty-send-last-key (&optional n)
  "Send the last key that was typed to the terminal N times.

This command extracts element of `this-command-key`, translates
it and sends it to the terminal.

This is a convenient variant to `mistty-send-key' which allows
burying key binding to send to the terminal inside of a keymap
with an arbitrary prefix.

This command is available in fullscreen mode."
  (interactive "p")
  (mistty-send-key
   (or n 1) (seq-subseq (this-command-keys-vector) -1)))

(define-obsolete-face-alias
 'mistty-send-key-sequence 'mistty-capture-keyboard "2.1")

(defun mistty-capture-keyboard ()
  "Send all keys to terminal until interrupted.

This function continuously read keys and sends them to the
terminal, just like `mistty-send-key', until it is interrupted
with \\[keyboard-quit] or until it is passed a key or event it
doesn't support, such as a mouse event.

It can also be stopped programmatically by calling
`mistty-exit-capture-keyboard' from a hook or a filter."
  (interactive)
  (when mistty--capture-keyboard-active
    (error "Recursive call to mistty-capture-keyboard"))
  (unwind-protect
      (let ((mistty--capture-keyboard-active t)
            (exit-key (kbd mistty-exit-capture-keyboard-key))
            (prompt (format "Sending all KEYS to terminal... Exit with %s."
                            mistty-exit-capture-keyboard-key)))
        (unless (length= exit-key 1)
          (user-error "Invalid value for mistty-exit-capture-keyboard-key; must be a single key"))
        (setq exit-key (aref exit-key 0))
        (mistty-log "capture start hook")
        (run-hooks 'mistty-start-capture-keyboard-hook)
        (catch 'mistty-capture-keyboard
          (let (key)
            (while
                (and
                 (setq key
                       (read-key prompt
                                 'inherit-input-method))
                 (not (eq key exit-key)))

              (pcase key
                ((pred mouse-event-p)
                 (throw 'mistty-capture-keyboard nil))
                (`(xterm-paste ,str)
                 (funcall mistty--send-function
                          (mistty--maybe-bracketed-str str) nil nil nil))
                (_ (mistty-send-key 1 (make-vector 1 key))))))))
    (mistty-log "capture end hook")
    (run-hook-wrapped
     'mistty-end-capture-keyboard-hook
     (lambda (func)
       (mistty-with-errors-logged "mistty-end-key-sequence-hook"
         (funcall func))))))

(defun mistty-exit-capture-keyboard ()
  "Abort any currently running `mistty-capture-keyboard'.

Does nothing if there is no running `mistty-capture-keyboard'."
  (when mistty--capture-keyboard-active
    (run-with-idle-timer 0 nil
                         (lambda ()
                           (when mistty--capture-keyboard-active
                             (throw 'mistty-capture-keyboard nil))))))

(defun mistty-translate-key (key &optional n noerror)
  "Generate string to sent to the terminal for KEY.

This function translates an Emacs key sequence, as returned by
`kbd', into a string that can be written to the terminal to
express that that key has been pressed in a way that commands
will hopefully understand.

The conversion can be configured by modifying
`mistty-term-key-map'.

If N is specified, the string is repeated N times.

If NOERROR is non-nil, return nil if a key is unknown instead of
failing."
  (let ((n (or n 1))
        (key (if (stringp key) (vconcat key) key)))
    ;; Standardize the key events, favoring the symbol-based
    ;; representation of keys just as ESC, RET and using M-<char>
    ;; instead of ESC <char>.
    (pcase key
      ((and `[?\e ,c] (guard (characterp c)))
       (setq key (vector (logior c #x8000000)))))
    (setq key (or (lookup-key mistty-translation-keymap key) key))

    (or (funcall mistty--translate-key-function key n)
        (if noerror
            nil
          (error "No terminal sequence known for key %S" key)))))

(defun mistty--translate-key-default (key n &optional extra-map)
  "Default implementation for `mistty--translate-key-function'.

This function generates a byte sequence for KEY, repeated N times. If
EXTRA-MAP is non-nil, it is looked up right after `mistty-term-key-map'.

Alone, this function only knows how to deal with M-<char>, control
characters and self-inserting characters.

If NOERROR is non-nil, return nil instead of signaling an error."
  (cl-block nil
    (dolist (map (list mistty-term-key-map extra-map))
      (when map
        (let ((translated-key (lookup-key map key)))
          (if (and translated-key
                   ;; weed out key prefixes
                   (or (characterp translated-key)
                       (stringp translated-key)))
              (cl-return
                     (mistty--repeat-string
                      n (concat translated-key)))))))

    (pcase key
      ;; M-<char> -> ESC-<char>
      ((and
        `[,c]
        (guard (and (equal '(meta) (event-modifiers c))
                    (numberp c)
                    (characterp (event-basic-type c)))))
       (mistty--repeat-string n (format "\e%c" (event-basic-type c))))

      ;; A single self-inserted characters
      ((and `[,c] (guard (characterp c)))
       (make-string n (elt key 0)))

      (_ nil))))

(defun mistty--maybe-bracketed-str (str)
  "Prepare STR to be sent, possibly bracketed, to the terminal."
  (if mistty-bracketed-paste
      (mistty--bracketed-str str)
    (mistty--untabify str)))

(defun mistty--bracketed-str (str)
  "Mark STR as bracketed-paste string."
  (concat "\e[200~" str "\e[201~"))

(defun mistty--untabify (str)
  "Replace tabs in STR with spaces."
  (string-replace "\t" (make-string tab-width ? ) str))

(defun mistty--list-basic-types-from-map (map)
  "Extract a list of basic event types referenced in MAP.

This does not go through sub-maps."
  (let ((symbols (list)))
    (map-keymap (lambda (ev _)
                  (when (and (eventp ev) (symbolp ev))
                    (when-let* ((type (event-basic-type ev)))
                      (cl-pushnew type symbols))))
                map)

    symbols))

(provide 'mistty-kbd)

;;; mistty-kbd.el ends here
