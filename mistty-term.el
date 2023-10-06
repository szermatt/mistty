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
(require 'subr-x)

(require 'mistty-util)

;;; Code:

(autoload 'mistty-osc7 "mistty-osc7")
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
    ("2" . ansi-osc-window-title-handler)
    ("8" . ansi-osc-hyperlink-handler))
  "Hook run when unknown OSC sequences have been received.

This hook is run on the `term-mode' buffer. It is passed the
content of OSC sequence - everything between OSC (ESC ]) and
ST (ESC \\ or \\a) and may choose to handle them.

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

(defconst mistty-right-str "\eOC"
  "Sequence to send to the process when the rightarrow is pressed.")

(defconst mistty-left-str "\eOD"
  "Sequence to send to the process when the left arrow is pressed.")

(defvar mistty-term-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>") "\t")
    (define-key map (kbd "<return>") "\C-m")
    (define-key map (kbd "<backspace>") "\d")
    (define-key map (kbd "<esc>") "\e")

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

This variable is available in the work buffer.")


(defvar-local mistty--term-properties-to-add-alist nil
  "An alist of id to text properties to add to the term buffer.

This variable associates arbitrary symbols to property lists. It
is set by `mistty-register-text-properties' and read whenever
text is written to the terminal.

This variable is available in the work buffer.")

(defvar-local mistty--undecoded-bytes nil
  "Bytes leftover in the last call to `mistty--emulate-terminal'.

They'll be processed once more data is passed to the next call.")

(defun mistty--emulate-terminal (proc str work-buffer)
  "Handle process output as a terminal would.

This function accepts output from PROC included into STR and
forwards them to `term-emulate-terminal'.

Some special sequences are interrupted and pre-processed:

- enabling and disabling bracketed paste, which means that
bracketed paste strings can be sent to PROC. The state is stored
into `mistty-bracketed-paste' in the buffer WORK-BUFFER.

- making cursor visible or invisible in WORK-BUFFER

- OSC sequences, either fed to `mistty-osc-handlers' or ignored."
  (cl-letf ((inhibit-modification-hooks nil) ;; run mistty--after-change-on-term
            (inhibit-read-only t) ;; allow modifications in char mode
            (start 0)
            ;; Using term-buffer-vertical-motion causes strange
            ;; issues; avoid it. Using mistty's window to compute
            ;; vertical motion is correct since the window dimension
            ;; are kept in sync with the terminal size. Falling back
            ;; to using the selected window, on the other hand, is
            ;; questionable.
            ((symbol-function 'term-buffer-vertical-motion)
             (lambda (count)
               (vertical-motion count (or (get-buffer-window work-buffer)
                                          (selected-window))))))
    (mistty--with-live-buffer (process-buffer proc)
      (when mistty--undecoded-bytes
        (setq str (concat mistty--undecoded-bytes str))
        (setq mistty--undecoded-bytes nil))
      ;; Note on OSC content: ECMA 48 8.3.89 only allows 0x08-0x0d
      ;; 0x20-7e. That would disallow all non-US-ASCII characters,
      ;; often used in file names, which would then need to be
      ;; encoded. This would be inconvenient and error-prone, so we
      ;; disallow the US-ASCII characters disallowed by ECMA 48 and
      ;; allow all non-US-ASCII chars (usually multibyte UTF-8).
      (while (string-match "\e\\(?1:\\[\\?\\(?:2004\\|25\\)[hl]\\|\\]\\(?2:\\(?:\\(?3:[a-zA-Z0-9]+\\);\\)?[^\x00-\x07\x0e-\x1f\x7f]*\\)\\(?4:\e\\\\\\|\a\\|\\'\\)\\)" str start)
        (let ((ext (match-string 1 str))
              (osc (match-string 2 str))
              (osc-code (match-string 3 str))
              (osc-terminator (match-string 4 str))
              (seq-start (match-beginning 0))
              (seq-end (match-end 0)))
          (cond
           ((equal ext "[?2004h") ; enable bracketed paste
            (term-emulate-terminal proc (substring str start seq-end))
            (let* ((id (mistty--next-id))
                   (props `(mistty-input-id ,id)))
              ;; zsh enables bracketed paste only after having printed
              ;; the prompt.
              (unless (eq ?\n (char-before (point)))
                (add-text-properties (mistty--bol (point)) (point) props))
              (mistty-register-text-properties 'mistty-bracketed-paste props))
            (mistty--with-live-buffer work-buffer
              (setq mistty-bracketed-paste t)))
           ((equal ext "[?2004l") ; disable bracketed paste
            (term-emulate-terminal proc (substring str start seq-end))
            (mistty-unregister-text-properties 'mistty-bracketed-paste)
            (mistty--with-live-buffer work-buffer
              (setq mistty-bracketed-paste nil)))
           ((equal ext "[?25h") ; make cursor visible
            (term-emulate-terminal proc (substring str start seq-end))
            (mistty--with-live-buffer work-buffer
              (setq cursor-type t)))
           ((equal ext "[?25l") ; make cursor invisible
            (term-emulate-terminal proc (substring str start seq-end))
            (mistty--with-live-buffer work-buffer
              (setq cursor-type nil)))
           ((and osc (length= osc-terminator 0))
            (term-emulate-terminal proc (substring str start seq-start))
            ;; sequence is not finished; save it for later
            (setq mistty--undecoded-bytes (substring str seq-start)))
           (osc
            (term-emulate-terminal proc (substring str start seq-start))
            (when-let ((code osc-code)
                       (handler (cdr (assoc-string code mistty-osc-handlers))))
              (funcall handler code
                       (decode-coding-string
                        (string-remove-prefix (concat code ";") osc)
                        locale-coding-system t)))))
          (setq start seq-end)))
      (let ((split (mistty--split-incomplete-chars (substring str start))))
        (when (length> (cdr split) 0)
          (setq mistty--undecoded-bytes (cdr split)))
        (term-emulate-terminal proc (car split))))))

(defun mistty--split-incomplete-chars (str)
  "Extract incomplete multibyte chars at the end of STR.

This function detects multibyte chars that couldn't be decoded at
the end of STR and splits it into a cons of complete string and
remaining bytes.

term.el is meant to do that, but it fails, because `char-charset'
alone doesn't behave the way term.el assumes (anymore?). This is
hopefully a temporary workaround."
  (let* ((len (length str))
         (end (substring str (max 0 (- len 8))))
         (decoded-end (decode-coding-string end locale-coding-system t))
         (undecoded-count 0)
         (i (1- (length decoded-end))))
    (while (and (>= i 0) (mistty--eight-bit-char-p decoded-end i))
      (cl-incf undecoded-count)
      (cl-decf i))
    (if (zerop undecoded-count)
        (cons str "")
      (cons
       (substring str 0 (- len undecoded-count))
       (substring str (- len undecoded-count))))))

(defun mistty--eight-bit-char-p (str index)
  "Check whether char in STR at INDEX has been decoded."
  ;; logic taken from Emacs 29 describe-char
  (let ((c (aref str index)))
    (eq 'eight-bit
        (if (and (not enable-multibyte-characters) (>= c 128))
            'eight-bit
          (or (get-text-property index 'charset str)
              (char-charset c))))))

(defun mistty-register-text-properties (id props)
  "Add PROPS to any text written to the terminal.

Call `mistty-unregister-text-properties' with the same ID to turn
that off.

If this function is called more than once with the same ID, only
the last set of properties to be registered is applied."
  (unless (eq 'term-mode major-mode) (error "Requires a term-mode buffer"))
  (if-let ((cell (assq id mistty--term-properties-to-add-alist)))
      (setcdr cell props)
    (push (cons id props) mistty--term-properties-to-add-alist)))

(defun mistty-unregister-text-properties (id)
  "Stop applying properties previously registered with ID."
  (unless (eq 'term-mode major-mode) (error "Requires a term-mode buffer"))
  (when-let ((cell (assq id mistty--term-properties-to-add-alist)))
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
      (setq-local term-char-mode-buffer-read-only t)
      (setq-local term-char-mode-point-at-process-mark t)
      (setq-local term-buffer-maximum-size 0)
      (setq-local term-set-terminal-size t)
      (setq-local term-width width)
      (setq-local term-height height)
      (term-exec term-buffer (buffer-name term-buffer) program nil args)
      (set-process-window-size (get-buffer-process term-buffer) height width)
      (setq-local term-raw-map local-map)
      (term-char-mode)
      (advice-add 'move-to-column :around #'mistty--around-move-to-column)
      (add-hook 'after-change-functions #'mistty--after-change-on-term nil t))

    term-buffer))

(defun mistty--after-change-on-term (beg end _old-length)
  "Function registered to `after-change-functions' by `mistty--create-term'.

BEG and END define the region that was modified."
  (when (and mistty--term-properties-to-add-alist (> end beg))
    (when-let ((props (apply #'append
                       (mapcar #'cdr mistty--term-properties-to-add-alist))))
      (add-text-properties beg end props)))

  ;; Add property to fake newlines so they're not yanked.
  (while-let ((pos (text-property-any beg end 'term-line-wrap t)))
    (let ((fake-nl-end (next-single-property-change pos 'term-line-wrap nil end)))
      (put-text-property pos fake-nl-end 'yank-handler '(nil "" nil nil))
      (setq beg fake-nl-end)))

  ;; Detect and mark right prompts.
  (let ((bol (mistty--bol beg))
        (eol (mistty--eol beg)))
    (when (and (> beg bol)
               (<= end eol)
               (get-text-property (1- beg) 'mistty-skip)
               (not (get-text-property bol 'mistty-skip)))
      (add-text-properties
       beg end '(mistty-skip t mistty-right-prompt t yank-handler (nil "" nil nil))))))

(defun mistty--around-move-to-column (orig-fun &rest args)
  "Add property \\='mistty-skip t to spaces added when just moving.

ORIG-FUN is the original `move-to-column' function that's being
advised and ARGS are its arguments."
  (if (eq 'term-mode major-mode)
    (let ((initial-end (line-end-position)))
      (apply orig-fun args)
      (when (> (point) initial-end)
        (add-text-properties
         initial-end (point)
         '(mistty-skip t yank-handler (nil "" nil nil)))))
    (apply orig-fun args)))

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

(provide 'mistty-term)

;;; mistty-term.el ends here
