;;; mistty-term.el --- Extensions for term.el for MisTTY -*- lexical-binding: t -*-

(require 'term)
(require 'subr-x)

(require 'mistty-util)

(autoload 'mistty-osc7 "mistty-osc7")

(defvar mistty-osc-hook (list #'mistty-osc7)
  "Hook run when unknown OSC sequences have been received.

This hook is run on the term-mode buffer. It is passed the
content of OSC sequence - everything between OSC (ESC ]) and
ST (ESC \\ or \\a) and may chooose to handle or ignore them.

The current buffer is set to the term-mode buffer. The hook is
allowed to modify it, to add text properties, for example. In
such case, consider using `mistty-register-text-properties'.")

(defconst mistty-left-str "\eOD"
  "Sequence to send to the process when the left arrow is pressed.")

(defconst mistty-right-str "\eOC"
  "Sequence to send to the process when the rightarrow is pressed.")


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
  "Bytes leftover in the last call to `mistty-emulate-terminal'.

They'll be processed once more data is passed to the next call.")

(defun mistty-emulate-terminal (proc str work-buffer)
  "Handle special terminal codes, then call `term-emulate-terminal'.

This functions intercepts some extented sequences term.el. This
all should rightly be part of term.el."
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
      (while (string-match "\e\\(\\[\\?\\(2004\\|25\\)[hl]\\|\\]\\(.*?\\)\\(\e\\\\\\|\a\\|\\'\\)\\)" str start)
        (let ((ext (match-string 1 str))
              (osc (match-string 3 str))
              (osc-terminator (match-string 4 str))
              (seq-start (match-beginning 0))
              (seq-end (match-end 0)))
          (cond
           ((equal ext "[?2004h") ; enable bracketed paste
            (term-emulate-terminal proc (substring str start seq-end))
            (let ((props `(mistty-prompt-id ,(mistty--next-id))))
              ;; zsh enables bracketed paste only after having printed
              ;; the prompt.
              (unless (eq ?\n (char-before (point)))
                (add-text-properties (mistty--bol-pos-from (point)) (point) props))
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
           (osc
            (term-emulate-terminal proc (substring str start seq-start))
            (if (length= osc-terminator 0)
                ;; sequence is not finished; save it for later
                (setq mistty--undecoded-bytes (substring str seq-start))
              (run-hook-with-args
               'mistty-osc-hook
               (decode-coding-string osc locale-coding-system t)))))
          (setq start seq-end)))
      (let ((final-str (substring str start)))
        (unless (length= final-str 0)
          (term-emulate-terminal proc final-str))))))

(defun mistty-register-text-properties (id props)
  (unless (eq 'term-mode major-mode) (error "requires a term-mode buffer"))
  (if-let ((cell (assq id mistty--term-properties-to-add-alist)))
      (setcdr cell props)
    (push (cons id props) mistty--term-properties-to-add-alist)))

(defun mistty-unregister-text-properties (id)
  (unless (eq 'term-mode major-mode) (error "requires a term-mode buffer"))
  (when-let ((cell (assq id mistty--term-properties-to-add-alist)))
    (setq mistty--term-properties-to-add-alist 
          (delq cell
                mistty--term-properties-to-add-alist))))

(defun mistty--create-term (name program args width height)
  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks)))
    (with-current-buffer term-buffer
      (term-mode)
      (setq-local term-char-mode-buffer-read-only t
                  term-char-mode-point-at-process-mark t
                  term-buffer-maximum-size 0
                  term-height height
                  term-width width)
      (term--reset-scroll-region)
      (term-exec term-buffer (buffer-name term-buffer) program nil args)
      (term-char-mode)
      (add-hook 'after-change-functions #'mistty--after-change-on-term nil t))
    term-buffer))

(defun mistty--after-change-on-term (beg end _old-length)
  (when (and mistty--term-properties-to-add-alist (> end beg))
    (when-let ((props (apply #'append
                       (mapcar #'cdr mistty--term-properties-to-add-alist))))
      (add-text-properties beg end props))))


(defun mistty--maybe-bracketed-str (str)
  (let ((str (string-replace "\t" (make-string tab-width ? ) str)))
    (cond
     ((not mistty-bracketed-paste) str)
     ((not (string-match "[[:cntrl:]]" str)) str)
     (t (concat "\e[200~"
                str
                "\e[201~"
                mistty-left-str
                mistty-right-str)))))

(defun mistty-translate-key (key &optional n)
  "Generate string to sent to the terminal for KEY.

This function translates an Emacs key sequence, as returned by
`kbd', into a string that can be written to the terminal to
express that that key has been pressed in a way that commands
will hopefully understand.

The conversion can be configured by modifying
`mistty-term-key-map'.

If N is specified, the string is repeated N times."
  (let ((n (or n 1)))
    (if (and (length= key 1) (characterp (elt key 0)))
        (make-string n (elt key 0))
      (if-let ((translated-key (lookup-key mistty-term-key-map key)))
          (mistty--repeat-string n (concat translated-key))
        (error "Key unknown in mistty-term-key-map: %s"
               (key-description key))))))

(provide 'mistty-term)
