;;; Tests mistty-osc-colors -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-x)
(require 'tramp)

(require 'mistty)
(require 'mistty-osc-colors)

(require 'mistty-testing)
(require 'turtles)

(turtles-ert-deftest mistty-test-osc-10-11 (:instance 'mistty)
  (mistty-with-test-buffer (:selected t)
    (set-face-foreground 'default "#ffff00")
    (set-face-background 'default "#0000ff")

    (mistty--send-string mistty-proc "read -rs -d \\\\ -p $'\\e]10;?\\e\\\\'  fg; ")
    (mistty--send-string mistty-proc "read -rs -d \\\\ -p $'\\e]11;?\\e\\\\'  bg; ")
    (mistty-send-and-wait-for-prompt)

    (mistty-send-text "echo $fg | strings")
    (should (equal "]10;rgb:ffff/ffff/0000"
                   (mistty-send-and-capture-command-output)))

    (mistty-send-text "echo $bg | strings")
    (should (equal "]11;rgb:0000/0000/ffff"
                   (mistty-send-and-capture-command-output)))))
