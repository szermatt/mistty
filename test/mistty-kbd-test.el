;;; Tests mistty-kbd.el -*- lexical-binding: t -*-

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

(require 'mistty-kbd)
(require 'ert)
(require 'ert-x)

(ert-deftest mistty-kbd-translate-key ()
  (should (equal "a" (mistty-translate-key (kbd "a") 1)))
  (should (equal "aaa" (mistty-translate-key (kbd "a") 3)))

  (should (equal "\C-a" (mistty-translate-key (kbd "C-a") 1)))

  (should (equal "\ea" (mistty-translate-key (kbd "M-a") 1)))
  (should (equal "\ea\ea\ea" (mistty-translate-key (kbd "M-a") 3)))
  (should (equal "\ea" (mistty-translate-key (kbd "\ea") 1)))
  (should (equal "\ea\ea" (mistty-translate-key (kbd "\ea") 2)))

  (should (equal mistty-left-str (mistty-translate-key (kbd "<left>") 1)))
  (should (equal mistty-right-str (mistty-translate-key (kbd "<right>") 1)))

  (should (equal mistty-up-str (mistty-translate-key (kbd "<up>") 1)))
  (should (equal mistty-down-str (mistty-translate-key (kbd "<down>") 1))))

(ert-deftest mistty-kbd-translate-key-escape ()
  (should (equal "\e" (mistty-translate-key (kbd "<escape>"))))
  (should (equal "\e" (mistty-translate-key "\e"))))

(ert-deftest mistty-kbd-key-override ()
  (let* ((map (copy-keymap mistty-term-key-map))
         (mistty-term-key-map map))
    (should (equal "\ea" (mistty-translate-key (kbd "M-a") 1)))

    ;; override the default translation of M-a to \ea by adding it to the map
    (define-key map (kbd "M-a") "foobar")
    (should (equal "foobar" (mistty-translate-key (kbd "M-a") 1)))))
