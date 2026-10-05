;;; mistty-term-eterm.el --- Use term.el to create the terminal -*- lexical-binding: t -*-

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
;; on top of mistty-alacritty.el The resulting terminal is an alacritty
;; terminal (TERM=eterm).

(require 'cl-lib)
(require 'term)
;;; Code:

(defvar term-width) ; defined in term.el
(defvar term-height) ; defined in term.el
(defvar term-home-marker) ; defined in term.el

(require 'mistty-term-base)
(require 'mistty-term)
(require 'mistty-accum)
(require 'mistty-scrolline)
(eval-when-compile
  (require 'mistty-accum-macros))

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
the terminal buffer.

This option only works on eterm terminals. It has no effect on alacritty
terminals."
  :group 'mistty
  :type '(alist :key-type string :value-type function))

(defcustom mistty-term-mode-hook (list #'mistty-call-term-mode-hook)
  "Hook run in in `term-mode' buffers created by MisTTY.

This hook overrides `term-mode-hook' for term buffers started by MisTTY
to allow configuring MisTTY's term buffers differently from normal term
buffers.

The default includes `mistty-call-term-mode-hook', which calls the
original `term-mode-hook'.

If you'd like to have completely different configuration for normal `term-mode'
buffers and `term-mode' buffers started by Mistty, call:

  (remove-hook \\='mistty-term-mode-hook \\='mistty-call-term-mode-hook)

You might want to execute the above command as well if you have reasons
to think that some `term-mode' customization are interfering with MisTTY's
operations.

This option only works on eterm terminals. It has no effect on alacritty
terminals."
  :group 'mistty
  :type 'hook)

(define-obsolete-variable-alias
  'mistty-fullscreen-map  'mistty-term-mode-map "1.5.1snapshot")

(defvar-keymap mistty-term-mode-map
  :parent term-raw-map
  :doc "Keymap active in eterm terminal.

This keymap overwrites some bindings from `term-raw-map'.

`mistty-fullscreen-mode-map' is normally the map you want to configure.
While in fullscreen mode, `mistty-fullscreen-mode-map' is also enabled
on the term buffer in addition to this map.

This map is ignored when using alacritty as a terminal."

    ;; Mirror keybindings from mistty-mode-map, for consistency.
    "C-c C-c" #'mistty-send-last-key
    "C-c C-z" #'mistty-send-last-key
    "C-c C-\\" #'mistty-send-last-key
    "C-c C-g" #'mistty-send-last-key

    ;; This only applies if term-bind-function-keys is non-nil.
    "<remap> <term-send-function-key>" #'mistty-send-key

    ;; Disable the "Terminal" menu; nothing that it contains should be
    ;; used on Term buffers used by MisTTY.
    "<menu-bar> <terminal>" nil

    ;; switching the term buffer to line mode would cause issues.
    "<remap> <term-line-mode>" nil)

(defvar mistty--eterm-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<escape>") "\e")
    (define-key map (kbd "<tab>") "\t")
    (define-key map (kbd "<return>") "\C-m")
    (define-key map (kbd "<backspace>") "\e[3~")
    (define-key map (kbd "<delete>") "\C-d")

    ;; The following is a reversed copy of xterm-function-map from
    ;; term/xterm. Simulating xterm keys is generally convenient, as
    ;; most command-line tools support xterm.
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

Note that `mistty-translation-keymap' is applied before this map, to
standardize around the symbol-based representation for ESC, RET or
<backtab>, so this map doesn't need to worry about historical
or alternative representations.

The default value of this map was created by applying
`mistty-reverse-input-decode-map', defined in
mistty-reverse-input-decode-map.el to `xterm-function-map'.")

(defvar mistty-shadowed-term-mode-hook nil
  "Special variable under which hooks found it `term-mode-hook' are stored.

This is allows running `term-mode-hook' or not, from
`mistty-term-mode-hook'.")

(defvar-local mistty--term-changed nil
  "Non-nil if the terminal was changed since last postprocess.

This is used to decide whether and on what region of the buffer
to call `mistty--term-postprocess'.")

(cl-defstruct (mistty--term-eterm
               (:constructor mistty--make-term-eterm)
               (:copier nil))
  proc buf change-before-scrolline fs)

(cl-defmethod mistty--create-term
  ((_type (eql 'eterm)) name command
   &key width height
   enter-fullscreen
   leave-fullscreen
   active-prompt
   after-clear-screen
   sync-scrolline)
  "Create an eterm-based terminal called NAME.

COMMAND is run in the terminal and its size is set to WIDTH x HEIGHT,
defaulting to 80 x 24."
  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks)))
    (with-current-buffer term-buffer
      (let* ((mistty-shadowed-term-mode-hook term-mode-hook)
             (term-mode-hook mistty-term-mode-hook))
        (term-mode))
      (font-lock-mode -1)
      (jit-lock-mode nil)
      (setq-local term-char-mode-buffer-read-only t)
      (setq-local term-char-mode-point-at-process-mark t)
      (setq-local term-buffer-maximum-size 0)
      (setq-local term-set-terminal-size t)
      (setq-local term-width width)
      (setq-local term-height height)
      (setq-local term-command-function #'mistty--term-command-hook)
      (setq-local mistty--translate-key-function #'mistty--eterm-translate-key)
      (setq-local mistty--prompt-cell (mistty--make-prompt-cell))
      (setq-local scroll-margin 0)

      ;; This makes sure the obsolete option
      ;; term-suppress-hard-newline is not set, as MisTTY relies on
      ;; term.el inserting fake newlines marked with term-line-wrap.
      (with-suppressed-warnings ((obsolete term-suppress-hard-newline))
        (setq term-suppress-hard-newline nil))

      (mistty-term--exec (car command) (cdr command))
      (let* ((proc (get-buffer-process term-buffer))
             (term (mistty--make-term-eterm :buf term-buffer :proc proc))
             (accum (mistty--make-accumulator
                     #'mistty--emulate-terminal)))

        ;; TRAMP sets adjust-window-size-function to #'ignore, which
        ;; prevents normal terminal resizing from working. This turns
        ;; it on again.
        (process-put proc 'adjust-window-size-function nil)
        (process-put proc 'mistty-term term)
        (set-process-window-size proc height width)
        (set-process-filter proc accum)
        (setq-local term-raw-map mistty-term-mode-map)
        (term-char-mode)
        (add-hook 'after-change-functions #'mistty--after-change-on-term nil t)

        (mistty--term-postprocess-changed accum term)
        (mistty--add-prompt-detection accum term) ;; CHECK

        (mistty--add-da1 accum)
        (mistty--add-skip-unsupported accum)
        (mistty--add-osc-detection accum)
        (unless enter-fullscreen (error ":enter-fullscreen required"))
        (mistty--accum-add-processor
         accum
         '(seq CSI (or "47" "?47" "?1047" "?1049") ?h)
         (lambda (ctx _str)
           (unless (mistty--term-eterm-fs term)
             (mistty--accum-ctx-flush ctx)
             (funcall enter-fullscreen 'split)
             (setf (mistty--term-eterm-fs term) t))
           (mistty--accum-ctx-push-down ctx "\e[47h")))

        (unless leave-fullscreen (error ":leave-fullscreen required"))
        (let ((end (copy-marker (point-max))))
          (mistty--accum-add-processor
           accum
           '(seq CSI (or "47" "?47" "?1047" "?1049") ?l)
           (lambda (ctx _str)
             (mistty--accum-ctx-push-down ctx "\e[47l")
             (when (mistty--term-eterm-fs term)
               (mistty--accum-ctx-flush ctx)
               ;; When handling CSI 47 h, term.el sometimes add a newline
               ;; that is not removed after handling CSI 47 l. This
               ;; manifests as extra newlines, especially visible when
               ;; launching recent versions of fish. This code works around
               ;; the problem by deleting anything after the position that
               ;; was end-of-buffer just before CSI 47 h was handled.
               (mistty--with-live-buffer (mistty--term-eterm-buf term)
                 (when (and end
                            (< end (point-max))
                            (eq ?\n (char-after end)))
                   (let ((inhibit-read-only t))
                     (delete-region end (point-max)))
                   (move-marker end nil))
                 (setq end nil))
               (setf (mistty--term-eterm-fs term) nil)
               (funcall leave-fullscreen)))))

        (unless active-prompt (error ":active-prompt required"))
        (mistty--accum-add-processor
         accum
         ;; CSI H CSI J is the exact sequence sent by 'clear'. We're going to
         ;; handle it like 2J and clear the screen.
         '(or (seq CSI ?2 ?J)
              (seq CSI ?H CSI ?J))
         (lambda (ctx str)
           (if (mistty--term-eterm-fs term)
               (mistty--accum-ctx-push-down ctx str)

             (let ((goto-home (equal str "\e[H\e[J")))
               (mistty--accum-ctx-flush ctx)
               (if (when-let* ((p (funcall active-prompt)))
                     (equal
                      (mistty--prompt-start p)
                      (mistty--with-live-buffer (mistty--term-eterm-buf term)
                        mistty--scrolline-home-num)))
                   (progn
                     (mistty-log "CLEAR PROMPT (%S)" str)
                     (mistty--accum-ctx-push-down
                      ctx (if goto-home "\e[H\e[0J" "\e[1J\e[0J")))
                 (mistty-log "CLEAR SCREEN (%S)" str)
                 (mistty--accum-ctx-push-down
                  ctx (if goto-home "\e[H\e[2J" "\e[2J"))
                 (mistty--accum-ctx-flush ctx)
                 (when after-clear-screen
                   (funcall after-clear-screen)))))))

        (mistty--accum-add-around-process-filter
         accum
         (lambda (func)
           (if (mistty--term-eterm-fs term)
               (funcall func)

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

        ;; Detect changes made to the terminal above the sync scrolline, which
        ;; means that the sync scrolline needs to be updated.
        (mistty--accum-add-around-process-filter
         accum
         (lambda (func)
           (if (mistty--term-eterm-fs term)
               (funcall func)

             (let ((limit (funcall sync-scrolline)))
               (if (or (null limit) (<= limit 0))
                   (funcall func)
               (when (mistty--detect-change-before-scrolline
                      func (mistty--term-eterm-buf term) limit)
                 (mistty-log "DETECTED BUFFER CHANGE, above %s" limit)
                 (setf (mistty--term-eterm-change-before-scrolline term) t)))))))

        term))))

(cl-defmethod mistty--term-buf ((term mistty--term-eterm))
  "Return TERM's `term-mode' buffer."
  (mistty--term-eterm-buf term))

(cl-defmethod mistty--term-proc ((term mistty--term-eterm))
  "Return TERM's process."
  (mistty--term-eterm-proc term))

(cl-defmethod mistty--term-screen-top-pos ((term mistty--term-eterm))
  "Return the position of the top of the screen in TERM's process buffer."
  (with-current-buffer (mistty--term-eterm-buf term)
    term-home-marker))

(cl-defmethod mistty--term-screen-top-scrolline ((term mistty--term-eterm))
  "Return the scrolline displayed in the first line of TERM's terminal."
  (with-current-buffer (mistty--term-eterm-buf term)
    (mistty--scrolline-at term-home-marker)))

(cl-defmethod mistty--term-alt-screen-p ((term mistty--term-eterm))
  "Return non-nil when the TERM's terminal is showing the alt buffer."
  (with-current-buffer (mistty--term-eterm-buf term)
    (term-using-alternate-sub-buffer)))

(cl-defmethod mistty--term-detect-prompt-p ((term mistty--term-eterm))
  "Return non-nil prompt detection should be enabled in TERM."
  (not (mistty--term-eterm-fs term)))

(cl-defmethod mistty--term-lines ((term mistty--term-eterm))
  "Return TERM's height."
  (with-current-buffer (mistty--term-eterm-buf term)
    term-height))

(cl-defmethod mistty--term-columns ((term mistty--term-eterm))
  "Return TERM's width."
  (with-current-buffer (mistty--term-eterm-buf term)
    term-width))

(cl-defmethod mistty--term-cursor-linecol ((term mistty--term-eterm))
  "Return TERM's cursor position in the terminal."
  (with-current-buffer (mistty--term-eterm-buf term)
    (cons (term-current-row) (term-current-column))))

(cl-defmethod mistty--term-sentinel-func ((_term mistty--term-eterm))
  "Return the default sentinel for `term-mode' processes."
  #'term-sentinel)

(cl-defmethod mistty--term-filter-func ((_term mistty--term-eterm))
  "Return the default filter for `term-mode' processes."
  #'mistty--emulate-terminal)

(cl-defmethod mistty--term-resize ((term mistty--term-eterm) width height)
  "Resize TERM's terminal to WIDTH x HEIGHT."
  (set-process-window-size (mistty--term-eterm-proc term) height width)
  (with-current-buffer (mistty--term-eterm-buf term)
    (term-reset-size height width)))

(cl-defmethod mistty--term-autoresize ((_term mistty--term-eterm) _enable)
  "Does nothing.

Autoresize is always enabled.")

(cl-defmethod mistty--term-setup-buffer ((_term mistty--term-eterm) &optional fullscreen)
  "Setup the terminal buffer in normal or fullscreen mode.

FULLSCREEN is non-nil in fullscreen mode."
  (if fullscreen
      (progn
        (jit-lock-mode t)
        (turn-on-font-lock))
    (font-lock-mode -1)
    (jit-lock-mode nil)))

(cl-defmethod mistty--term-sync
  ((term mistty--term-eterm) dest-buffer sync-pos sync-scrolline keep-markers
   cursor-marker)
  (mistty--with-live-buffer (mistty--term-eterm-buf term)
    (let ((home-marker term-home-marker)
          (home-scrolline (mistty--scrolline-at term-home-marker))
          (proc (mistty--term-eterm-proc term))
          (source-buffer (current-buffer)))
      ;; Detect shenanigans and update sync-pos and sync-scrolline accordingly
      (cond
       ((< sync-scrolline home-scrolline)
        (pcase-setq
         `(,sync-pos . ,sync-scrolline)
         (mistty--catchup home-marker home-scrolline dest-buffer sync-pos sync-scrolline)))
       ((mistty--term-eterm-change-before-scrolline term)
        (mistty-log "Detected terminal change above sync mark, at scrolline %s"
                    mistty--scrolline-home-num)
        (pcase-setq
         `(,sync-pos . ,sync-scrolline)
         (mistty--realign-buffers
          source-buffer home-scrolline dest-buffer sync-pos sync-scrolline))))

      (setf (mistty--term-eterm-change-before-scrolline term) nil)

      (let ((source-sync-pos (mistty--find-scrolline sync-scrolline))
            (column-count term-width))
        (mistty--sync-buffer
         source-buffer
         source-sync-pos
         dest-buffer
         sync-pos
         keep-markers)


        (mistty--with-live-buffer dest-buffer
          (set-marker cursor-marker
                      (+ sync-pos
                         (- (process-mark proc) source-sync-pos)))

          (mistty--hide-line-wraps sync-pos (point-max) column-count)
          (mistty--mark-empty-line-at-eob sync-pos)))

      (cons sync-pos sync-scrolline))))

(cl-defmethod mistty--term-clear-to-eol ((_term mistty--term-eterm) _pos)
  "Does nothing.

This is not needed as it's enough to clear the text properties for eterm.")


(cl-defmethod mistty--term-cleanup-prompt-sp ((_term mistty--term-eterm) _pos)
  "Does nothing.

This is not needed as it's enough to clear the text properties for eterm.")

(cl-defmethod mistty--term-changed ((_term mistty--term-eterm) beg end)
  "Report that the region between BEG and END changed on the terminal buffer."
  (mistty--changed beg end))

(cl-defmethod mistty--term-truncate-buffer ((term mistty--term-eterm) scrolline-limit)
  "Truncate the terminal buffer, if necessary.

Always keep SCROLLINE-LIMIT and below."
  (with-current-buffer (mistty--term-eterm-buf term)
    (when (>= scrolline-limit mistty--scrolline-home-num)
      (mistty--truncate-buffer term-home-marker))))

(cl-defmethod mistty--term-clear-scrollback ((term mistty--term-eterm))
  "Clear any scrollback still stored in the process buffer or vterm."
  (with-current-buffer (mistty--term-eterm-buf term)
    (when (> term-home-marker (point-min))
      (let ((inhibit-read-only t)
            (inhibit-modification-hooks t))
        (delete-region (point-min) term-home-marker)))))

(cl-defmethod mistty--term-translate-key ((_term mistty--term-eterm) key n)
  "Generate the key byte sequence for TERM.

KEY is an Emacs key event and n the number of repetition for that event.

The function returns the byte sequence appropriate for sending that key
to the terminal."
  (mistty--eterm-translate-key key n))

(defun mistty--eterm-translate-key (key n)
  "Translate KEY n times for eterm."
  (mistty--translate-key-default key n mistty--eterm-key-map))

(defun mistty--term-postprocess-changed (accum term)
  "Set \\='mistty-skip on the regions changed since last call.

This function registers a post processor on ACCUM that works with the
given TERM."
  (mistty--accum-add-post-processor
   accum
   (lambda ()
     (unless (mistty--term-eterm-fs term)
       (with-current-buffer (mistty--term-eterm-buf term)
         (when (and mistty--term-changed (< mistty--term-changed (point-min)))
           (setq mistty--term-changed (point-min)))
         (when (and mistty--term-changed (>= mistty--term-changed (point-max)))
           (setq mistty--term-changed nil))
         (when-let* ((change-start
                      (when mistty--term-changed
                        (text-property-any
                         mistty--term-changed (point-max) 'mistty-updated t))))
           (mistty--term-postprocess change-start term-width)
           (remove-text-properties
            change-start (point-max) '(mistty-updated t))
           (setq mistty--term-changed nil)))))))

(defun mistty--add-skip-unsupported (accum)
  "Skip some unsupported terminal sequences that confuse term.el.

This function adds processors to ACCUM to skip Application
Keypad (DECPAM) / Normal Keypad (DECPNM) Issued by Fish 4+ but just
ecoed by term.el."
  (mistty--accum-add-processor
   accum
   '(seq ESC (char "=>")) #'ignore))


(defun mistty--add-da1 (accum)
  "Configur ACCUM to handle DA1 Primary Device Detection code.

This implementation detects and answers primary device detection
requests from the application attached to the terminal. This is
here mostly to keep fish 4.1 and later happy."
  (mistty--accum-add-processor
   accum
   '(seq CSI (or "0c" "c"))
   (lambda (_ _)
     (process-send-string (get-buffer-process (current-buffer))
                          "\e[?64;1;18;21;22c"))))

(defun mistty-call-term-mode-hook ()
  "Call the functions registered to `term-mode-hook'.

Remove this hook from `mistty-term-mode-hook' to allow the terminal
modes started by MisTTY to have a completely separate setup from normal
terminal modes. See the documentation of `mistty-term-mode-hook' for
details."
  (run-hooks 'mistty-shadowed-term-mode-hook))


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
      (put-text-property beg end 'mistty-updated t))))

(defun mistty--around-move-to-column (orig-fun &rest args)
  "Add property \\='mistty-clear t to spaces added when just moving.

ORIG-FUN is the original `move-to-column' function and ARGS are its
arguments."
  (let ((initial-end (line-end-position)))
    (apply orig-fun args)
    (when (> (point) initial-end)
      (put-text-property
       initial-end (point) 'mistty-clear t))))


(defun mistty--emulate-terminal (proc str)
  "Handle process output as a terminal would.

This function accepts output from PROC included into STR and forwards
them to `term-emulate-terminal' with some modified functions, fix some
issues.

It also logs everything it receives to the function `mistty-log'.

This is meant as a drop-in replacement for `term-emulate-terminal' in
all situations, even when no work buffer is available."
  (cl-letf* ((inhibit-read-only t) ;; allow modifications in char mode
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
                        (mistty--clear-term-face-value term-current-face)))))

             ;; Save screen content in scrollback before clearing it.
             ((symbol-function 'term-erase-in-display)
              (let ((realfunc (symbol-function 'term-erase-in-display)))
                (lambda (arg)
                  (cond
                   ((equal 3 arg)) ;; clear scrollback; handled in work buffer
                   ((and
                     (not (term-using-alternate-sub-buffer))
                     (or (equal 2 arg)))
                    (term-handle-deferred-scroll)
                    (term-goto (1- term-height) 0)
                    (let ((lines (save-excursion
                                   (goto-char (point-max))
                                   (skip-chars-backward "[:blank:]\n\r")
                                   (when (> (point) term-home-marker)
                                     (mistty--count-lines term-home-marker (point))))))
                      (when lines
                        (mistty-log "[term] CLEAR SCREEN (kept %s lines)" lines)
                        (term-down (+ 1 lines))))
                    (term-goto-home)
                    (funcall realfunc 0))

                   (t
                    (funcall realfunc arg))))))

             ;; Save screen content in scrollback before a reset
             ((symbol-function 'term-reset-terminal)
              (lambda ()
                  (term-erase-in-display 2)
                  (term-ansi-reset)
                  (setq term-insert-mode nil))))
    (mistty-log "RECV %S" str)
    (term-emulate-terminal proc str)

    (mistty--with-live-buffer (process-buffer proc)
      (mistty--adjust-scrolline-base)

      ;; term-emulate-terminal sometimes ends up with the process mark
      ;; coming before the home marker or the point not at the process
      ;; mark. This is a workaround.
      (move-marker
       (process-mark proc)
       (max (process-mark proc) term-home-marker))
      (goto-char (process-mark proc)))))

(defun mistty--adjust-scrolline-base ()
  "Move the scrolline base to `term-home-marker'.

Call this before deleting any region before `term-home-marker'."
  (when (markerp term-home-marker)
    (mistty--update-scrolline
     term-home-marker (mistty--scrolline-at term-home-marker))))


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


(defun mistty--term-command-hook (string)
  "TRAMP-aware alternative to the hook variable `term-command-hook'.

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

(defun mistty--term-postprocess (region-start window-width)
  "Set mistty-skip and yank handlers between REGION-START and REGION-END.

WINDOW-WIDTH is used to detect right prompts.

This sets properties from the \\='mistty-clear properties,
detecting regions looking at a complete line."
  (save-excursion
    (let ((inhibit-read-only t)
          (inhibit-modification-hooks t))
      (goto-char region-start)
      (goto-char (pos-bol))
      (setq region-start (point))
      (remove-text-properties
       region-start (point-max)
       '(mistty-skip nil yank-handler nil mistty-updated nil))
      (goto-char region-start)
      (while
          (progn
            (let ((bol (pos-bol))
                  (eol (pos-eol)))
              (when (> eol bol)
                (unless (mistty--detect-right-prompt bol eol window-width)
                  (let ((end (mistty--detect-indent bol eol)))
                    (mistty--detect-trailing-spaces end eol)))))

            ;; process next line?
            (forward-line 1)
            (< (point) (point-max)))))))

(defun mistty--detect-right-prompt (bol eol window-width)
  "Detect right prompt and return its left position or nil.

BOL and EOL define the region to look in. WINDOW-WIDTH must be the width
of the terminal, usually `mistty-alacritty-columns'."
  (let ((pos (1- eol)) in-prompt)
    (when (and (< (abs (- window-width (mistty--column-count))) 3)
               (setq in-prompt (text-property-not-all (max bol (- eol 3)) eol 'mistty-clear t)))
      (when-let* ((rightmost-nonclear (previous-single-property-change in-prompt 'mistty-clear nil bol)))
        (when (and (eq (char-before rightmost-nonclear) ?\ )
                   (> rightmost-nonclear bol))
          (setq pos (1- rightmost-nonclear))
          (while (and (>= pos bol)
                      (eq (char-after pos) ?\ )
                      (get-text-property pos 'mistty-clear))
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
                (get-text-property pos 'mistty-clear))
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
                (get-text-property pos 'mistty-clear))
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

(defun mistty--hide-line-wraps (beg end column-count)
  "Make fake newlines invisible between BEG and END.

They're not really visible. to begin with, since they're at the end of
the window, but marking them invisible allows `kill-line' to go through
them, as it should.

Only the newlines at COLUMN-COUNT are actually modified."
  (save-excursion
    (goto-char beg)
    (while (and (< (point) end)
                (search-forward "\n" end 'noerror))
      (when (and
             (get-text-property (match-beginning 0) 'term-line-wrap)
             (zerop (% (save-excursion
                         (goto-char (match-beginning 0))
                         (current-column))
                       column-count)))
        (add-text-properties
         (1- (point)) (point)
         '(invisible term-line-wrap yank-handler (nil "" nil nil)))))))

(defun mistty--mark-empty-line-at-eob (beg)
  "Mark empty lines at EOB with mistty-skip empty-line-at-eob.

When using eterm, this must be done on the work buffer after refreshing
and not on the term buffer, because newlines tend to stick around in the
term buffer and could end up having a confusing text property.

Start searching after BEG."
  (let ((pos (point-max)))
    (while (and (> pos beg)
                (eq ?\n (char-before pos)))
      (cl-decf pos))
    (when (< pos (point-max))
      (add-text-properties pos (point-max)
                           '(mistty-skip empty-lines-at-eob yank-handler (nil "" nil nil))))))

(provide 'mistty-term-eterm)

;;; mistty-term-eterm.el ends here
