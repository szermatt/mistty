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
;; terminal and the `term-mode' buffer. term.el would be a better fit
;; for many of these.

(require 'term)
(defvar term-width) ; defined in term.el
(defvar term-height) ; defined in term.el
(defvar term-home-marker) ; defined in term.el

(require 'pcase)
(require 'subr-x)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-util)
(require 'mistty-log)
(require 'mistty-accum)
(eval-when-compile
  (require 'mistty-accum-macros))

;;; Code:

(autoload 'mistty-osc7 "mistty-osc7")
(autoload 'mistty-osc-query-color "mistty-osc-colors")
(autoload 'ansi-osc-window-title-handler "ansi-osc")
(autoload 'ansi-osc-hyperlink-handler "ansi-osc")

(defcustom mistty-osc-handlers
  '(
    ;; not using ansi-osc-directory-tracker because it doesn't decode
    ;; the coding system of the path after percent-decoding it.
    ;; TODO: propose a fix for ansi-osc
    ("7" . mistty-osc7)

    ;; These handlers are reasonably compatibly with MisTTY OSC. This
    ;; isn't necessary going to be the case for all such handlers.
    ("0" . ansi-osc-window-title-handler)
    ("2" . ansi-osc-window-title-handler)
    ("8" . ansi-osc-hyperlink-handler)

    ;; Allow querying foreground and background color. While OSC 10/11
    ;; normally supports changing color, this isn't supported here.
    ("10" . mistty-osc-query-color)
    ("11" . mistty-osc-query-color)
    ("133" . mistty-osc133))
  "Hook run when unknown OSC sequences have been received.

This hook is run on the `term-mode' buffer. It is passed the OSC code as
a string and the content of OSC sequence - everything between OSC (ESC
]) and ST (ESC \\ or \\a) and may choose to handle them.

The current buffer a`term-mode' buffer. The hook is allowed to
modify it, to add text properties, for example. In such case,
consider using `mistty-register-text-properties'.

Most handlers written for the ansi-osc package (Emacs 29) should
work here as well.

If you add here a handler that sets a buffer-local variable,
consider adding that variable to `mistty-variables-to-copy' so
that its value is available in the main MisTTY buffer, not just
the terminal buffer."
  :group 'mistty
  :type '(alist :key-type string :value-type function))

(defcustom mistty-set-EMACS nil
  "Whether the EMACS env variable should be set, for Bash 4.3 and older.

You only need to set this if:
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

(defconst mistty-del "\C-h"
  "Sequence to send to the process when backspace is pressed.

Both BS (^H) and DEL (^?) have been used to map to backspace, though
inconsistently across terminals. Mistty 1.3 and earlier sent DEL, but
that was switched to BS when Fish 4 started interpreting that as the
delete key instead of the backspace key.

If you change this key, remember to change the mapping in
`mistty-term-key-map' as well")

(defvar mistty-term-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") "\t")
    (define-key map (kbd "<return>") "\C-m")
    (define-key map (kbd "<backspace>") mistty-del)
    (define-key map (kbd "<escape>") "\e")

    ;; The following is a reversed copy of xterm-function-map from
    ;; term/xterm. Simulating xterm keys is generally convenient, as
    ;; most command-line tools support xterm.
    (define-key map (kbd "<delete>") "\e[3~")
    (define-key map (kbd "<down>") "\eOB")
    (define-key map (kbd "<end>") "\eOF")
    (define-key map (kbd "<f10>") "\e[21~")
    (define-key map (kbd "<f11>") "\e[23~")
    (define-key map (kbd "<f12>") "\e[24~")
    (define-key map (kbd "<f1>") "\eOP")
    (define-key map (kbd "<f2>") "\eOQ")
    (define-key map (kbd "<f3>") "\eOR")
    (define-key map (kbd "<f4>") "\eOS")
    (define-key map (kbd "<f5>") "\e[15~")
    (define-key map (kbd "<f6>") "\e[17~")
    (define-key map (kbd "<f7>") "\e[18~")
    (define-key map (kbd "<f8>") "\e[19~")
    (define-key map (kbd "<f9>") "\e[20~")
    (define-key map (kbd "<home>") "\e[1~")
    (define-key map (kbd "<insert>") "\e[2~")
    (define-key map (kbd "<kp-0>") "\eOp")
    (define-key map (kbd "<kp-1>") "\eOq")
    (define-key map (kbd "<kp-2>") "\eOr")
    (define-key map (kbd "<kp-3>") "\eOs")
    (define-key map (kbd "<kp-4>") "\eOt")
    (define-key map (kbd "<kp-5>") "\eOu")
    (define-key map (kbd "<kp-6>") "\eOv")
    (define-key map (kbd "<kp-7>") "\eOw")
    (define-key map (kbd "<kp-8>") "\eOx")
    (define-key map (kbd "<kp-9>") "\eOy")
    (define-key map (kbd "<kp-add>") "\eOk")
    (define-key map (kbd "<kp-divide>") "\eOo")
    (define-key map (kbd "<kp-equal>") "\eOX")
    (define-key map (kbd "<kp-multiply>") "\eOj")
    (define-key map (kbd "<kp-separator>") "\eOl")
    (define-key map (kbd "<kp-subtract>") "\eOm")
    (define-key map (kbd "<left>") "\eOD")
    (define-key map (kbd "<menu>") "\e[29~")
    (define-key map (kbd "<next>") "\e[6~")
    (define-key map (kbd "<prior>") "\e[5~")
    (define-key map (kbd "<right>") "\eOC")
    (define-key map (kbd "<select>") "\e[4~")
    (define-key map (kbd "<up>") "\eOA")
    (define-key map (kbd "C-!") "\e[33;6u")
    (define-key map (kbd "C-#") "\e[35;6u")
    (define-key map (kbd "C-$") "\e[36;6u")
    (define-key map (kbd "C-%") "\e[37;6u")
    (define-key map (kbd "C-&") "\e[38;6u")
    (define-key map (kbd "C-'") "\e[39;5u")
    (define-key map (kbd "C-(") "\e[40;6u")
    (define-key map (kbd "C-)") "\e[41;6u")
    (define-key map (kbd "C-*") "\e[42;6u")
    (define-key map (kbd "C-+") "\e[43;6u")
    (define-key map (kbd "C-,") "\e[44;5u")
    (define-key map (kbd "C--") "\e[45;5u")
    (define-key map (kbd "C-.") "\e[46;5u")
    (define-key map (kbd "C-/") "\e[47;5u")
    (define-key map (kbd "C-0") "\e[48;5u")
    (define-key map (kbd "C-1") "\e[49;5u")
    (define-key map (kbd "C-9") "\e[57;5u")
    (define-key map (kbd "C-:") "\e[58;6u")
    (define-key map (kbd "C-;") "\e[59;5u")
    (define-key map (kbd "C-<") "\e[60;6u")
    (define-key map (kbd "C-<delete>") "\e[3;5~")
    (define-key map (kbd "C-<down>") "\e[1;5B")
    (define-key map (kbd "C-<end>") "\e[1;5F")
    (define-key map (kbd "C-<f10>") "\e[21;5~")
    (define-key map (kbd "C-<f11>") "\e[23;5~")
    (define-key map (kbd "C-<f12>") "\e[24;5~")
    (define-key map (kbd "C-<f1>") "\eO5P")
    (define-key map (kbd "C-<f2>") "\eO5Q")
    (define-key map (kbd "C-<f3>") "\eO5R")
    (define-key map (kbd "C-<f4>") "\eO5S")
    (define-key map (kbd "C-<f5>") "\e[15;5~")
    (define-key map (kbd "C-<f6>") "\e[17;5~")
    (define-key map (kbd "C-<f7>") "\e[18;5~")
    (define-key map (kbd "C-<f8>") "\e[19;5~")
    (define-key map (kbd "C-<f9>") "\e[20;5~")
    (define-key map (kbd "C-<home>") "\e[1;5H")
    (define-key map (kbd "C-<insert>") "\e[2;5~")
    (define-key map (kbd "C-<left>") "\e[1;5D")
    (define-key map (kbd "C-<next>") "\e[6;5~")
    (define-key map (kbd "C-<prior>") "\e[5;5~")
    (define-key map (kbd "C-<return>") "\e[13;5u")
    (define-key map (kbd "C-<right>") "\e[1;5C")
    (define-key map (kbd "C-<tab>") "\e[9;5u")
    (define-key map (kbd "C-<up>") "\e[1;5A")
    (define-key map (kbd "C-=") "\e[61;5u")
    (define-key map (kbd "C->") "\e[62;6u")
    (define-key map (kbd "C-?") "\e[63;6u")
    (define-key map (kbd "C-M-!") "\e[33;8u")
    (define-key map (kbd "C-M-#") "\e[35;8u")
    (define-key map (kbd "C-M-$") "\e[36;8u")
    (define-key map (kbd "C-M-%") "\e[37;8u")
    (define-key map (kbd "C-M-&") "\e[38;8u")
    (define-key map (kbd "C-M-'") "\e[39;7u")
    (define-key map (kbd "C-M-(") "\e[40;8u")
    (define-key map (kbd "C-M-)") "\e[41;8u")
    (define-key map (kbd "C-M-*") "\e[42;8u")
    (define-key map (kbd "C-M-+") "\e[43;8u")
    (define-key map (kbd "C-M-,") "\e[44;7u")
    (define-key map (kbd "C-M--") "\e[45;7u")
    (define-key map (kbd "C-M-.") "\e[46;7u")
    (define-key map (kbd "C-M-/") "\e[47;7u")
    (define-key map (kbd "C-M-0") "\e[48;7u")
    (define-key map (kbd "C-M-1") "\e[49;7u")
    (define-key map (kbd "C-M-2") "\e[50;7u")
    (define-key map (kbd "C-M-3") "\e[51;7u")
    (define-key map (kbd "C-M-4") "\e[52;7u")
    (define-key map (kbd "C-M-5") "\e[53;7u")
    (define-key map (kbd "C-M-6") "\e[54;7u")
    (define-key map (kbd "C-M-7") "\e[55;7u")
    (define-key map (kbd "C-M-8") "\e[56;7u")
    (define-key map (kbd "C-M-9") "\e[57;7u")
    (define-key map (kbd "C-M-:") "\e[58;8u")
    (define-key map (kbd "C-M-;") "\e[59;7u")
    (define-key map (kbd "C-M-<") "\e[60;8u")
    (define-key map (kbd "C-M-<delete>") "\e[3;7~")
    (define-key map (kbd "C-M-<down>") "\e[1;7B")
    (define-key map (kbd "C-M-<end>") "\e[1;7F")
    (define-key map (kbd "C-M-<home>") "\e[1;7H")
    (define-key map (kbd "C-M-<insert>") "\e[2;7~")
    (define-key map (kbd "C-M-<left>") "\e[1;7D")
    (define-key map (kbd "C-M-<next>") "\e[6;7~")
    (define-key map (kbd "C-M-<prior>") "\e[5;7~")
    (define-key map (kbd "C-M-<return>") "\e[13;7u")
    (define-key map (kbd "C-M-<right>") "\e[1;7C")
    (define-key map (kbd "C-M-<tab>") "\e[9;7u")
    (define-key map (kbd "C-M-<up>") "\e[1;7A")
    (define-key map (kbd "C-M-=") "\e[61;7u")
    (define-key map (kbd "C-M->") "\e[62;8u")
    (define-key map (kbd "C-M-?") "\e[63;8u")
    (define-key map (kbd "C-M-S-<delete>") "\e[3;8~")
    (define-key map (kbd "C-M-S-<down>") "\e[1;8B")
    (define-key map (kbd "C-M-S-<end>") "\e[1;8F")
    (define-key map (kbd "C-M-S-<home>") "\e[1;8H")
    (define-key map (kbd "C-M-S-<insert>") "\e[2;8~")
    (define-key map (kbd "C-M-S-<kp-0>") "\eO8p")
    (define-key map (kbd "C-M-S-<kp-1>") "\eO8q")
    (define-key map (kbd "C-M-S-<kp-2>") "\eO8r")
    (define-key map (kbd "C-M-S-<kp-3>") "\eO8s")
    (define-key map (kbd "C-M-S-<kp-4>") "\eO8t")
    (define-key map (kbd "C-M-S-<kp-5>") "\eO8u")
    (define-key map (kbd "C-M-S-<kp-6>") "\eO8v")
    (define-key map (kbd "C-M-S-<kp-7>") "\eO8w")
    (define-key map (kbd "C-M-S-<kp-8>") "\eO8x")
    (define-key map (kbd "C-M-S-<kp-9>") "\eO8y")
    (define-key map (kbd "C-M-S-<kp-add>") "\eO8k")
    (define-key map (kbd "C-M-S-<kp-divide>") "\eO8o")
    (define-key map (kbd "C-M-S-<kp-multiply>") "\eO8j")
    (define-key map (kbd "C-M-S-<kp-separator>") "\eO8l")
    (define-key map (kbd "C-M-S-<kp-subtract>") "\eO8m")
    (define-key map (kbd "C-M-S-<left>") "\e[1;8D")
    (define-key map (kbd "C-M-S-<next>") "\e[6;8~")
    (define-key map (kbd "C-M-S-<prior>") "\e[5;8~")
    (define-key map (kbd "C-M-S-<right>") "\e[1;8C")
    (define-key map (kbd "C-M-S-<up>") "\e[1;8A")
    (define-key map (kbd "C-M-SPC") "\e[32;7u")
    (define-key map (kbd "C-M-\"") "\e[34;8u")
    (define-key map (kbd "C-M-\\") "\e[92;7u")
    (define-key map (kbd "C-S-<delete>") "\e[3;6~")
    (define-key map (kbd "C-S-<down>") "\e[1;6B")
    (define-key map (kbd "C-S-<end>") "\e[1;6F")
    (define-key map (kbd "C-S-<f10>") "\e[21;6~")
    (define-key map (kbd "C-S-<f11>") "\e[23;6~")
    (define-key map (kbd "C-S-<f12>") "\e[24;6~")
    (define-key map (kbd "C-S-<f1>") "\eO6P")
    (define-key map (kbd "C-S-<f2>") "\eO6Q")
    (define-key map (kbd "C-S-<f3>") "\eO6R")
    (define-key map (kbd "C-S-<f4>") "\eO6S")
    (define-key map (kbd "C-S-<f5>") "\e[15;6~")
    (define-key map (kbd "C-S-<f6>") "\e[17;6~")
    (define-key map (kbd "C-S-<f7>") "\e[18;6~")
    (define-key map (kbd "C-S-<f8>") "\e[19;6~")
    (define-key map (kbd "C-S-<f9>") "\e[20;6~")
    (define-key map (kbd "C-S-<home>") "\e[1;6H")
    (define-key map (kbd "C-S-<insert>") "\e[2;6~")
    (define-key map (kbd "C-S-<kp-0>") "\eO6p")
    (define-key map (kbd "C-S-<kp-1>") "\eO6q")
    (define-key map (kbd "C-S-<kp-2>") "\eO6r")
    (define-key map (kbd "C-S-<kp-3>") "\eO6s")
    (define-key map (kbd "C-S-<kp-4>") "\eO6t")
    (define-key map (kbd "C-S-<kp-5>") "\eO6u")
    (define-key map (kbd "C-S-<kp-6>") "\eO6v")
    (define-key map (kbd "C-S-<kp-7>") "\eO6w")
    (define-key map (kbd "C-S-<kp-8>") "\eO6x")
    (define-key map (kbd "C-S-<kp-9>") "\eO6y")
    (define-key map (kbd "C-S-<kp-add>") "\eO6k")
    (define-key map (kbd "C-S-<kp-divide>") "\eO6o")
    (define-key map (kbd "C-S-<kp-multiply>") "\eO6j")
    (define-key map (kbd "C-S-<kp-separator>") "\eO6l")
    (define-key map (kbd "C-S-<kp-subtract>") "\eO6m")
    (define-key map (kbd "C-S-<left>") "\e[1;6D")
    (define-key map (kbd "C-S-<next>") "\e[6;6~")
    (define-key map (kbd "C-S-<prior>") "\e[5;6~")
    (define-key map (kbd "C-S-<return>") "\e[13;6u")
    (define-key map (kbd "C-S-<right>") "\e[1;6C")
    (define-key map (kbd "C-S-<tab>") "\e[9;6u")
    (define-key map (kbd "C-S-<up>") "\e[1;6A")
    (define-key map (kbd "C-\"") "\e[34;6u")
    (define-key map (kbd "C-\\") "\e[92;5u")
    (define-key map (kbd "M-<delete>") "\e[3;3~")
    (define-key map (kbd "M-<down>") "\e[1;3B")
    (define-key map (kbd "M-<end>") "\e[1;3F")
    (define-key map (kbd "M-<f10>") "\e[21;3~")
    (define-key map (kbd "M-<f11>") "\e[23;3~")
    (define-key map (kbd "M-<f12>") "\e[24;3~")
    (define-key map (kbd "M-<f1>") "\eO3P")
    (define-key map (kbd "M-<f2>") "\eO3Q")
    (define-key map (kbd "M-<f3>") "\eO3R")
    (define-key map (kbd "M-<f4>") "\eO3S")
    (define-key map (kbd "M-<f5>") "\e[15;3~")
    (define-key map (kbd "M-<f6>") "\e[17;3~")
    (define-key map (kbd "M-<f7>") "\e[18;3~")
    (define-key map (kbd "M-<f8>") "\e[19;3~")
    (define-key map (kbd "M-<f9>") "\e[20;3~")
    (define-key map (kbd "M-<home>") "\e[1;3H")
    (define-key map (kbd "M-<insert>") "\e[2;3~")
    (define-key map (kbd "M-<left>") "\e[1;3D")
    (define-key map (kbd "M-<next>") "\e[6;3~")
    (define-key map (kbd "M-<prior>") "\e[5;3~")
    (define-key map (kbd "M-<right>") "\e[1;3C")
    (define-key map (kbd "M-<up>") "\e[1;3A")
    (define-key map (kbd "M-S-<delete>") "\e[3;4~")
    (define-key map (kbd "M-S-<down>") "\e[1;4B")
    (define-key map (kbd "M-S-<end>") "\e[1;4F")
    (define-key map (kbd "M-S-<f10>") "\e[21;4~")
    (define-key map (kbd "M-S-<f11>") "\e[23;4~")
    (define-key map (kbd "M-S-<f12>") "\e[24;4~")
    (define-key map (kbd "M-S-<f1>") "\eO4P")
    (define-key map (kbd "M-S-<f2>") "\eO4Q")
    (define-key map (kbd "M-S-<f3>") "\eO4R")
    (define-key map (kbd "M-S-<f4>") "\eO4S")
    (define-key map (kbd "M-S-<f5>") "\e[15;4~")
    (define-key map (kbd "M-S-<f6>") "\e[17;4~")
    (define-key map (kbd "M-S-<f7>") "\e[18;4~")
    (define-key map (kbd "M-S-<f8>") "\e[19;4~")
    (define-key map (kbd "M-S-<f9>") "\e[20;4~")
    (define-key map (kbd "M-S-<home>") "\e[1;4H")
    (define-key map (kbd "M-S-<insert>") "\e[2;4~")
    (define-key map (kbd "M-S-<kp-0>") "\eO4p")
    (define-key map (kbd "M-S-<kp-1>") "\eO4q")
    (define-key map (kbd "M-S-<kp-2>") "\eO4r")
    (define-key map (kbd "M-S-<kp-3>") "\eO4s")
    (define-key map (kbd "M-S-<kp-4>") "\eO4t")
    (define-key map (kbd "M-S-<kp-5>") "\eO4u")
    (define-key map (kbd "M-S-<kp-6>") "\eO4v")
    (define-key map (kbd "M-S-<kp-7>") "\eO4w")
    (define-key map (kbd "M-S-<kp-8>") "\eO4x")
    (define-key map (kbd "M-S-<kp-9>") "\eO4y")
    (define-key map (kbd "M-S-<kp-add>") "\eO4k")
    (define-key map (kbd "M-S-<kp-divide>") "\eO4o")
    (define-key map (kbd "M-S-<kp-multiply>") "\eO4j")
    (define-key map (kbd "M-S-<kp-separator>") "\eO4l")
    (define-key map (kbd "M-S-<kp-subtract>") "\eO4m")
    (define-key map (kbd "M-S-<left>") "\e[1;4D")
    (define-key map (kbd "M-S-<next>") "\e[6;4~")
    (define-key map (kbd "M-S-<prior>") "\e[5;4~")
    (define-key map (kbd "M-S-<right>") "\e[1;4C")
    (define-key map (kbd "M-S-<up>") "\e[1;4A")
    (define-key map (kbd "S-<delete>") "\e[3;2~")
    (define-key map (kbd "S-<down>") "\e[1;2B")
    (define-key map (kbd "S-<end>") "\e[1;2F")
    (define-key map (kbd "S-<f10>") "\e[21;2~")
    (define-key map (kbd "S-<f11>") "\e[23;2~")
    (define-key map (kbd "S-<f12>") "\e[24;2~")
    (define-key map (kbd "S-<f1>") "\e[1;2P")
    (define-key map (kbd "S-<f2>") "\e[1;2Q")
    (define-key map (kbd "S-<f3>") "\e[1;2R")
    (define-key map (kbd "S-<f4>") "\e[1;2S")
    (define-key map (kbd "S-<f5>") "\e[15;2~")
    (define-key map (kbd "S-<f6>") "\e[17;2~")
    (define-key map (kbd "S-<f7>") "\e[18;2~")
    (define-key map (kbd "S-<f8>") "\e[19;2~")
    (define-key map (kbd "S-<f9>") "\e[20;2~")
    (define-key map (kbd "S-<home>") "\e[1;2H")
    (define-key map (kbd "S-<insert>") "\e[2;2~")
    (define-key map (kbd "S-<kp-0>") "\eO2p")
    (define-key map (kbd "S-<kp-1>") "\eO2q")
    (define-key map (kbd "S-<kp-2>") "\eO2r")
    (define-key map (kbd "S-<kp-3>") "\eO2s")
    (define-key map (kbd "S-<kp-4>") "\eO2t")
    (define-key map (kbd "S-<kp-5>") "\eO2u")
    (define-key map (kbd "S-<kp-6>") "\eO2v")
    (define-key map (kbd "S-<kp-7>") "\eO2w")
    (define-key map (kbd "S-<kp-8>") "\eO2x")
    (define-key map (kbd "S-<kp-9>") "\eO2y")
    (define-key map (kbd "S-<kp-add>") "\eO2k")
    (define-key map (kbd "S-<kp-divide>") "\eO2o")
    (define-key map (kbd "S-<kp-multiply>") "\eO2j")
    (define-key map (kbd "S-<kp-separator>") "\eO2l")
    (define-key map (kbd "S-<kp-subtract>") "\eO2m")
    (define-key map (kbd "S-<left>") "\e[1;2D")
    (define-key map (kbd "S-<next>") "\e[6;2~")
    (define-key map (kbd "S-<prior>") "\e[5;2~")
    (define-key map (kbd "S-<return>") "\e[13;2u")
    (define-key map (kbd "S-<right>") "\e[1;2C")
    (define-key map (kbd "S-<tab>") "\e[9;2u")
    (define-key map (kbd "S-<up>") "\e[1;2A")

    map)
"Maps keys to the corresponding sequence to send to the terminal.

This map is used by `mistty-send-key' to convert the key it
receives into something the commands attached to the terminal
might understand.

The default value of this map was created by applying
`mistty-reverse-input-decode-map', defined in
mistty-reverse-input-decode-map.el to `xterm-function-map'.")

(defvar-local mistty-bracketed-paste nil
  "Whether bracketed paste is enabled in the terminal.

This variable evaluates to true when bracketed paste is turned on
by the command that controls, to false otherwise.

This variable is available in both the work and term buffers.")

(defvar-local mistty--term-changed nil
  "Non-nil if the terminal was changed since last postprocess.

This is used to decide whether and on what region of the buffer
to call `mistty--term-postprocess'.")

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

(defvar-local mistty--scrolline-home nil
  "Base of scrolline numbers.")
(defvar-local mistty--scrolline-base nil
  "Scrolline number of `mistty--scrolline-home'.")

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

(defun mistty--emulate-terminal (proc str)
  "Handle process output as a terminal would.

This function accepts output from PROC included into STR and forwards
them to `term-emulate-terminal' with some modified functions, fix some
issues.

It also logs everything it receives to mistty-log.

This is meant as a drop-in replacement for `term-emulate-terminal' in
all situations, even when no work buffer is available."
  (cl-letf ((inhibit-read-only t) ;; allow modifications in char mode
            ;; Using term-buffer-vertical-motion causes strange
            ;; issues; avoid it. Additionally, it's not actually
            ;; necessary since term.el adds newlines instead of
            ;; relying on Emacs wrapping lines. Mistty makes sure of
            ;; that by forcing term-suppress-hard-newline off.
            ((symbol-function 'term-buffer-vertical-motion)
             (lambda (count)
               (let ((start-point (point))
                     (res (forward-line count)))
                 ;; Convert forward-line return value (lines left to
                 ;; go through) to vertical-motion's (lines gone
                 ;; through) with a workaround for forward-line
                 ;; special handling of the last line.
                 (setq res (- count res))
                 (when (and (> count 0)
                            (= (point) (point-max))
                            (> (point) start-point)
                            (not (eq ?\n (char-before (point-max)))))
                   (cl-decf res))
                 res)))
            ((symbol-function 'term--handle-colors-list)
             (let ((real-handle-colors-list (symbol-function 'term--handle-colors-list)))
               (lambda (parameters)
                 (funcall real-handle-colors-list parameters)
                 (setq term-current-face
                       (mistty--clear-term-face-value term-current-face))))))
    (mistty-log "RECV %S" str)
    (term-emulate-terminal proc str)))

(defun mistty--add-skip-unsupported (accum)
  "Skip some unsupported terminal sequences that confuse term.el.

This function adds processors to ACCUM to skip Application
Keypad (DECPAM) / Normal Keypad (DECPNM) Issued by Fish 4+ but just
ecoed by term.el."
  (mistty--accum-add-processor
   accum
   '(seq ESC (char "=>")) #'ignore))

(defun mistty--add-osc-detection (accum)
  "Handle OSC code in ACCUM.

Known OSC codes are passed down to handlers registered in
`mistty-osc-handlers'."
  (mistty--accum-add-processor-lambda accum
      (ctx '(seq OSC (let code Ps) ?\; (let text Pt) ST))
   (when-let* ((handler (cdr (assoc-string code mistty-osc-handlers))))
     (mistty--accum-ctx-flush ctx)
     (let ((inhibit-modification-hooks t)
           (inhibit-read-only t))
       (funcall handler code
                (decode-coding-string text locale-coding-system t))))))

(defun mistty--add-prompt-detection (accum)
  "Register processors to ACCUM for prompt detection.

Detected prompts can be found in `mistty-prompt'."
  (mistty--accum-add-post-processor
   accum #'mistty--term-postprocess-changed)
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
              (scrolline (mistty--term-scrolline)))
         (when (or (null prompt)
                   (not (mistty--prompt-contains prompt scrolline)))
           ;; zsh enables bracketed paste only after having printed
           ;; the prompt. Try to find the beginning of the prompt
           ;; from prompt_sp or assume a single-line prompt.
           (when-let* ((real-start
                       (catch 'mistty-prompt-start
                         (dolist (i '(0 -1 -2 -3))
                           (let ((pos (pos-eol i)))
                             (when (and (zerop (mistty--prompt-counter))
                                        (= pos 1) (> (point) (pos-bol)))
                               (mistty-log "extend first prompt [1-%s]" (point))
                               (throw 'mistty-prompt-start (point-min)))
                             (when (get-text-property pos 'mistty-prompt-sp)
                               (mistty-log "prompt_sp %s [%s-%s]" i (1+ pos) (point))
                               (remove-text-properties
                                pos (1+ pos) '(term-line-wrap t 'mistty-prompt-sp nil))
                               (throw 'mistty-prompt-start (1+ pos)))))))
                      (eol (pos-eol)))
             (when (> eol real-start)
               ;; mistty--changed is only called when bracketed
               ;; paste is on; mark past sections of the prompt
               ;; as changed, including to the eol to cover
               ;; right prompts, also written before.
               (mistty--changed real-start eol))
             (setq scrolline (mistty--term-scrolline-at real-start)))
           (setq prompt (mistty--make-prompt 'bracketed-paste scrolline))
           (mistty--term-remove-prompt_sp prompt)
           (mistty-log "Detected %s prompt #%s [%s-]"
                       (mistty--prompt-source prompt)
                       (mistty--prompt-input-id prompt)
                       (mistty--prompt-start prompt))
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
                                 (mistty--term-scrolline)
                               (1+ (mistty--term-scrolline)))))
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
       (when (or (and (= (1- term-width) (term-current-column))
                      (eq ?\  (char-before (point))))
                 (and (get-text-property (pos-eol 0) 'term-line-wrap)
                      (string-match "^ *$" (buffer-substring (pos-bol) (pos-eol)))))
         (let ((inhibit-modification-hooks t)
               (inhibit-read-only t)
               (pos (pos-eol 0)))
           (put-text-property pos (1+ pos) 'mistty-prompt-sp t))))

     (mistty--accum-ctx-push-down ctx "\r")))

  ;; Detect and mark moves with mistty-maybe-skip
  (mistty--accum-add-around-process-filter
   accum
   (lambda (func)
     (cl-letf ((inhibit-modification-hooks nil) ;; run mistty--after-change-on-term
               ((symbol-function 'term-delete-chars)
                (lambda (count)
                  (let ((save-point (point)))
                    (move-to-column (+ (term-current-column) count) t)
                    (delete-region save-point (point)))))
               ((symbol-function 'move-to-column)
                (let ((orig (symbol-function 'move-to-column)))
                  (lambda (&rest args)
                    (apply #'mistty--around-move-to-column orig args)))))
       (funcall func)))))

(defun mistty--term-remove-prompt_sp (prompt)
  "Clear the mistty-prompt-sp property in PROMPT.

This should be called for all newly-detected prompt to avoid confusing
future prompts."
  (when-let* ((start (mistty--term-scrolline-pos
                     (mistty--prompt-start prompt))))
    (when (< start (point-max))
      (remove-text-properties (max (point-min) (1- start))
                              (point-max) '
                              (mistty-prompt-sp nil)))))

(defun mistty--regexp-prompt-detector ()
  "Build a post-processor that look for a new prompt at cursor.

The return value is meant to be
`mistty--accum-add-post-processor'.

 The post-processor updates `mistty--prompt' after the content of the
terminal buffer has been updated."
  (let ((last-nonempty-scrolline 0))
    (lambda ()
      (let ((scrolline (mistty--term-scrolline)))
        ;; Only look at new lines
        (when (> scrolline
                 (prog1 last-nonempty-scrolline
                   ;; for next time
                   (setq last-nonempty-scrolline
                         (mistty--term-scrolline-at
                          (mistty--last-non-ws)))))
          (let ((cursor (point))
                (bos (mistty--beginning-of-scrolline-pos))
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

(defun mistty--create-term (name program args local-map width height)
  "Create a new term buffer with name NAME.

The buffer runs PROGRAM with the given ARGS.

LOCAL-MAP specifies a local map to be used as the char-mode map.

WIDTH and HEIGHT are the initial dimension of the terminal
reported to the remote process.

This function returns the newly-created buffer."
  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks)))
    (with-current-buffer term-buffer
      (term-mode)
      (font-lock-mode -1)
      (jit-lock-mode nil)
      (setq-local term-char-mode-buffer-read-only t)
      (setq-local term-char-mode-point-at-process-mark t)
      (setq-local term-buffer-maximum-size 0)
      (setq-local term-set-terminal-size t)
      (setq-local term-width width)
      (setq-local term-height height)
      (setq-local term-command-function #'mistty--term-command-hook)
      (setq-local mistty--scrolline-home (copy-marker (point-min)))
      (setq-local mistty--scrolline-base 0)
      (setq-local mistty--prompt-cell (mistty--make-prompt-cell))
      (setq-local scroll-margin 0)

      ;; This makes sure the obsolete option
      ;; term-suppress-hard-newline is not set, as MisTTY relies on
      ;; term.el inserting fake newlines marked with term-line-wrap.
      (let ((var 'term-suppress-hard-newline))
        (when (and (boundp var) (symbol-value var))
          (setf (buffer-local-value var (current-buffer)) nil)))

      (mistty-term--exec program args)
      (let ((proc (get-buffer-process term-buffer)))
        ;; TRAMP sets adjust-window-size-function to #'ignore, which
        ;; prevents normal terminal resizing from working. This turns
        ;; it on again.
        (process-put proc 'adjust-window-size-function nil)
        (set-process-window-size proc height width)
        (set-process-filter proc (mistty--make-accumulator
                                  #'mistty--emulate-terminal)))
      (setq-local term-raw-map local-map)
      (term-char-mode)
      (add-hook 'after-change-functions #'mistty--after-change-on-term nil t))

    term-buffer))

(defun mistty-term--exec (program args)
  "Execute PROGRAM with ARGS in the terminal buffer.

Must be called from the term buffer."
  (let ((buffer (current-buffer))
        (name (buffer-name))
        ;; Bash versions older than 4.4 only turn on directory
        ;; tracking if the env variable EMACS is set and contains
        ;; "term". To deal with that, term.el detects whether a
        ;; version of bash older than 4.4 is installed and if it is,
        ;; set this variable to 43. This logic doesn't work well on
        ;; remote hosts. MisTTY disables that and replaces it with
        ;; mistty-set-EMACS.
        (term--bash-needs-EMACS-status 0)
        (process-environment
         (if (with-connection-local-variables mistty-set-EMACS)
             (cons (format "EMACS=%s (term:%s)"
                           emacs-version term-protocol-version)
                   process-environment)
           process-environment)))

    (cl-letf*
        ;; On MacOS, the length of the termcap entry, heavily
        ;; escaped by TRAMP, plus the other env variables is enough
        ;; to hit the 1024 byte limit of the tty cache used in
        ;; canonical mode (on Linux, it is 4095, so there's no
        ;; problem.) Adding a newline to the termcap entry avoids
        ;; hitting that limit while remaining valid. An alternative
        ;; would be to have TRAMP disable canonical mode with stty
        ;; -icanon before sending out the command.
        ((term-termcap-format (concat term-termcap-format "\n"))

         ;; term.el calls start-process, which doesn't support starting
         ;; processes with TRAMP. The following intercepts replace
         ;; start-process with start-file process, which does support
         ;; TRAMP.
         (real-start-process (symbol-function 'start-process))
         (called nil)
         ((symbol-function 'start-process)
          (lambda (name buffer program &rest program-args)
            (if called
                (apply real-start-process name buffer program program-args)
              (setq called t)
              ;; Set erase to ^H or ^? to stty so the terminal is
              ;; expecting the right delete value. Issue #12
              (when-let* ((stty-command (nth 1 program-args))
                         (erase-char (pcase mistty-del
                                       ("\C-h" "^H")
                                       ("\d" "^?"))))
                (setq program-args (cl-copy-list program-args))
                (when (string-match "stty.*?sane" stty-command)
                  (setf (nth 1 program-args)
                        (concat (match-string 0 stty-command)
                                " erase "
                                erase-char
                                (substring stty-command (match-end 0))))))
              (let* ((process-environment
                      ;; TERMINFO references a local file. This is
                      ;; not useful on a remote host, so let's
                      ;; remove it. A description of the terminal is
                      ;; available in TERMCAP.
                      (if (file-remote-p default-directory)
                          (delq nil
                                (mapcar (lambda (var)
                                          (if (string-prefix-p "TERMINFO=" var)
                                              nil
                                            var))
                                        process-environment))
                        process-environment))
                     (proc (apply #'start-file-process name buffer program program-args)))

                ;; start-file-process doesn't always respect
                ;; coding-system-for-read set by term.el. Force it.
                (set-process-coding-system proc 'binary (cdr (process-coding-system proc)))
                proc)))))
      (term-exec buffer name program nil args))))

(defun mistty--after-change-on-term (beg end _old-length)
  "Function registered to `after-change-functions' by `mistty--create-term'.

BEG and END define the region that was modified."
  (let ((inhibit-modification-hooks t))
    (when (and mistty--term-properties-to-add-alist (> end beg))
      (when-let* ((props (apply #'append
                               (mapcar #'cdr mistty--term-properties-to-add-alist))))
        ;; Merge sections with same properties separated by
        ;; whitespaces. The problem with setting text properties based
        ;; on term state is that the terminal might just reuse spaces
        ;; or newlines that already exist - visually, it doesn't
        ;; matter - even though they're in a section that should get
        ;; these properties.
        (save-excursion
          (goto-char beg)
          (when (and (/= 0 (skip-chars-backward " \t\n"))
                     (> (point) (point-min))
                     (mistty--has-text-properties (1- (point)) props))
            (add-text-properties (point) beg props)))
        (add-text-properties beg end props)))

    (when mistty-bracketed-paste
      (mistty--changed beg end))))

(defun mistty--changed (beg end)
  "Mark text between BEG and END as changed, forcing postprocess."
  (setq mistty--term-changed (if mistty--term-changed
                                 (min mistty--term-changed beg)
                               beg))
  (let ((beg (mistty--bol beg))
        (end (mistty--eol end)))
    (when (> end beg)
      (put-text-property beg end 'mistty-changed t))))

(defun mistty--around-move-to-column (orig-fun &rest args)
  "Add property \\='mistty-maybe-skip t to spaces added when just moving.

ORIG-FUN is the original `move-to-column' function and ARGS are its
arguments."
  (let ((initial-end (line-end-position)))
    (apply orig-fun args)
    (when (> (point) initial-end)
      (put-text-property
       initial-end (point) 'mistty-maybe-skip t))))

(defun mistty--term-postprocess-changed ()
  "Process mistty-maybe-skip text properties.

This function turns mistty-maybe-skip into mistty-skip properties on the
lines that have changed, as detected by `mistty--term-changed'."
  (when (and mistty--term-changed (< mistty--term-changed (point-min)))
    (setq mistty--term-changed (point-min)))
  (when (and mistty--term-changed (>= mistty--term-changed (point-max)))
    (setq mistty--term-changed nil))
  (when-let* ((change-start
              (when mistty--term-changed
                (text-property-any
                 mistty--term-changed (point-max) 'mistty-changed t))))
    (mistty--term-postprocess change-start term-width))
  (setq mistty--term-changed nil))

(defun mistty--term-postprocess (region-start window-width)
  "Set mistty-skip and yank handlers after REGION-START.

WINDOW-WIDTH is used to detect right prompts.

This sets properties from the mistty-maybe-skip properties,
detecting regions looking at a complete line."
  (save-excursion
    (goto-char region-start)
    (goto-char (pos-bol))
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (remove-text-properties
       region-start (point-max)
       '(mistty-skip nil yank-handler nil mistty-changed nil))
      (while
          (progn
            (let ((bol (pos-bol))
                  (eol (pos-eol)))
              (when (> eol bol)
                (unless (mistty--detect-right-prompt bol eol window-width)
                  (let ((end (or (mistty--detect-continue-prompt bol)
                                 (mistty--detect-indent bol eol))))
                    (mistty--detect-trailing-spaces end eol)))))

            ;; process next line?
            (forward-line 1)
            (not (eobp))))
      (setq mistty--term-changed nil))))

(defun mistty--detect-right-prompt (bol eol window-width)
  "Detect right prompt and return its left position or nil.

BOL and EOL define the region to look in. WINDOW-WIDTH must be the width
of the terminal, usually `term-width'."
  (let ((pos (1- eol)))
    (when (and (>= 3 (- window-width (mistty--line-width)))
               (not (get-text-property pos 'mistty-maybe-skip)))
      (when-let* ((first-maybe-skip (previous-single-property-change eol 'mistty-maybe-skip nil bol)))
        (when (and (eq (char-before first-maybe-skip) ?\ )
                   (> first-maybe-skip bol))
          (setq pos (1- first-maybe-skip))
          (while (and (>= pos bol)
                      (eq (char-after pos) ?\ )
                      (get-text-property pos 'mistty-maybe-skip))
            (cl-decf pos))
          (cl-incf pos)
          (add-text-properties
           pos eol '(mistty-skip right-prompt
                     yank-handler (nil "" nil nil)))

          pos)))))

(defun mistty--detect-continue-prompt (bol)
  "Detect continue prompt and return its right position or nil.

BOL define the start of the region to look in."
  (catch 'mistty-return
    (save-excursion
      (goto-char bol)
      (dolist (prompt mistty-multi-line-continue-prompts)
        (when (looking-at prompt)
          (let ((end (match-end 0)))
            (when (> end bol)
              (add-text-properties
               bol end
               '(mistty-skip continue-prompt yank-handler (nil "" nil nil)))
              (throw 'mistty-return end))))))))

(defun mistty--detect-indent (bol eol)
  "Detect line indentation and return its right position or nil.

BOL and EOL define the region to look in."
  (let ((pos bol))
    (while (and (eq (char-after pos) ?\ )
                (get-text-property pos 'mistty-maybe-skip))
      (cl-incf pos))
    (when (> pos bol)
      (when (= pos eol)
        (setq pos (min pos (+ bol (mistty--previous-line-indent)))))
      (put-text-property bol pos 'mistty-skip 'indent))

    pos))

(defun mistty--detect-trailing-spaces (bol eol)
  "Detect trailing spaces the left position or nil.

BOL and EOL define the region to look in."
  (let ((pos (1- eol)))
    (while (and (>= pos bol)
                (eq (char-after pos) ?\ )
                (get-text-property pos 'mistty-maybe-skip))
      (cl-decf pos))
    (cl-incf pos)

    (when (< pos eol)
      (add-text-properties
       pos eol
       `(mistty-skip trailing yank-handler (nil "" nil nil))))

    pos))

(defun mistty--previous-line-indent ()
  "Return the indentation of the previous line.

This requires the text property mistty-skip to have been set on
the previous line."
  (or
   (save-excursion
     (when (= 0 (forward-line -1))
       (let* ((bol (pos-bol))
              (eol (pos-eol))
              (pos bol))
         (while (and (< pos eol)
                     (eq 'indent (get-text-property pos 'mistty-skip)))
           (cl-incf pos))
         (- pos bol))))
   0))

(defun mistty--maybe-bracketed-str (str)
  "Prepare STR to be sent, possibly bracketed, to the terminal.

If bracketed paste is enabled and STR contains control and
bracketed paste is enabled, this function returns STR with
bracketed paste brackets around it."
  (let ((str (string-replace "\t" (make-string tab-width ? ) str)))
    (cond
     ((not mistty-bracketed-paste) str)
     ((not (string-match "[[:cntrl:]]" str)) str)
     (t (concat "\e[200~" str "\e[201~")))))

(defun mistty-translate-key (key &optional n)
  "Generate string to sent to the terminal for KEY.

This function translates an Emacs key sequence, as returned by
`kbd', into a string that can be written to the terminal to
express that that key has been pressed in a way that commands
will hopefully understand.

The conversion can be configured by modifying
`mistty-term-key-map'.

If N is specified, the string is repeated N times."
  (let ((n (or n 1))
        (c (and (length= key 1) (elt key 0)))
        (non-meta)
        translated-key)
    (cond
     ;; DEL -> mistty-del
     ((eq c ?\d)
      (mistty--repeat-string n mistty-del))
     ;; Self-inserted characters
     ((and c (characterp c))
      (make-string n (elt key 0)))
     ;; M-char -> ESC char
     ((and c (numberp c)
           (characterp
            (setq non-meta (logand c (lognot #x8000000)))))
      (mistty--repeat-string n (format "\e%c" non-meta)))
     ;; Lookup in mistty-term-key-map
     ((setq translated-key (lookup-key mistty-term-key-map key))
      (mistty--repeat-string n (concat translated-key)))
     (t
      (error "Key unknown in mistty-term-key-map: %s"
             (key-description key))))))

(defun mistty--term-command-hook (string)
  "TRAMP-aware alternative to `term-command-hook'.

This function is meant to be bound to `term-command-function' to
catch Emacs-specific control sequences \\032...\\n. The STRING
argument includes everything between \\032 and \\n.

When `default-directory' is remote, this function interprets paths
sent by the terminal as being local to the TRAMP connection. The
result is that it sends remote paths to `cd'.

This works well with Bash which, by default, sends out directory paths
with every prompt if the env variable INSIDE_EMACS is set."
  (if (= (aref string 0) ?/)
      (let ((path (substring string 1)))
        (unless (file-remote-p path)
          (when-let* ((prefix (file-remote-p default-directory)))
            (setq path (concat prefix path))))
        ;; Not using cd here, to avoid a remote connection being made to
        ;; check the path.
        (setq path (file-name-as-directory path))
        (setq path (expand-file-name path))
        (setq default-directory path))

    ;; unknown or unsupported Emacs-specific control sequence.
    (term-command-hook string)))

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

(defun mistty--detect-dead-spaces-after-insert (content beg)
  "Mark dead trailing spaces left by the terminal after inserting CONTENT.

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
                  (mistty-log "@%s %s dead spaces, %s real"
                              eol (- eol (point)) real-trailing-ws)
                  (put-text-property (point) eol 'mistty-skip 'dead))))))))))

(defun mistty--term-reset-scrolline (scrolline)
  "Make the screen start at SCROLLINE.

This is useful after a reset, where scrolline have been lost. Generally,
this allows arbitrarily manipulating the alignment between the work and
terminal buffers. To avoid issues with prompt locations, it should only
be used to increase the value of `mistty--scrolline-base'."
  (setq mistty--scrolline-base scrolline)
  (move-marker mistty--scrolline-home
               (or (marker-position term-home-marker)
                   (point-min))))

(defun mistty--adjust-scrolline-base ()
  "Move the scrolline base to `term-home-marker'.

Call this before deleting any region before `term-home-marker'."
  (when (/= mistty--scrolline-home term-home-marker)
    (let ((delta (mistty--count-scrollines mistty--scrolline-home term-home-marker)))
      (setq mistty--scrolline-base (+ mistty--scrolline-base delta)))
    (move-marker mistty--scrolline-home (marker-position term-home-marker))))

(defun mistty--term-scrolline ()
  "Return the current scrolline.

The scrolline count starts at the very beginning of the virtual buffer
and doesn't change as the buffer scrolls up or the terminal size
changes.

Before using a scrolline, convert it to a screen row or point."
  (mistty--term-scrolline-at (point)))

(defun mistty--term-scrolline-at (pos)
  "Return the scrolline at POS.

The scrolline count starts at the very beginning of the virtual buffer
and doesn't change as the buffer scrolls up or the terminal size
changes.

Before using a scrolline, convert it to a screen row or point."
  (+ mistty--scrolline-base (mistty--count-scrollines mistty--scrolline-home pos)))

(defun mistty--term-scrolline-pos (scrolline)
  "Return the char position of the beginning of SCROLLINE.

Return nil if the row isn't reachable on the terminal."
  (save-excursion
    (goto-char mistty--scrolline-home)
    (when (zerop (mistty--go-down-scrollines (- scrolline mistty--scrolline-base)))
      (point))))

(defun mistty--term-scrolline-at-screen-start()
  "Scrolline at the top of the screen."
  (mistty--adjust-scrolline-base)
  mistty--scrolline-base)

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
  (when (and (length> osc-seq 0) (not (term-using-alternate-sub-buffer)))
    (let ((command-char (aref osc-seq 0)))
      (pcase command-char
        (?A ;; start a new command

         ;; Overwrite any other prompt source.
         (let ((prompt (mistty--make-prompt 'osc133 (mistty--term-scrolline))))
           (setf (mistty--prompt) prompt)
           (mistty--term-remove-prompt_sp prompt)
           (mistty-log "Detected %s prompt #%s [%s-]"
                       (mistty--prompt-source prompt)
                       (mistty--prompt-input-id prompt)
                       (mistty--prompt-start prompt))))

        (?B ;; end of prompt/start of user input
         (when-let* ((prompt (mistty--prompt)))
           (setf (mistty--prompt-user-input-start prompt)
                 (cons (mistty--term-scrolline)
                       (term-current-column)))))

        (?C ;; start of command output
         (when-let* ((prompt (mistty--prompt)))
           (when (eq 'osc133 (mistty--prompt-source prompt))
             (mistty-log "Closed %s prompt #%s [%s-]"
                         (mistty--prompt-source prompt)
                         (mistty--prompt-input-id prompt)
                         (mistty--prompt-start prompt))
             (setf (mistty--prompt-end prompt) (mistty--term-scrolline)))))

        (?D ;; end of command (possible anytime after ?A)
         (when-let* ((prompt (mistty--prompt)))
           (when (and (eq 'osc133 (mistty--prompt-source prompt))
                      (null (mistty--prompt-end prompt)))
             (mistty-log "Aborted %s prompt #%s [%s-]"
                         (mistty--prompt-source prompt)
                         (mistty--prompt-input-id prompt)
                         (mistty--prompt-start prompt))
             (setf (mistty--prompt-end prompt) (mistty--term-scrolline)))))))))

(provide 'mistty-term)

;;; mistty-term.el ends here
