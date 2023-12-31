;;; Tests mistty-term.el -*- lexical-binding: t -*-

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

(require 'mistty-term)
(require 'ert)
(require 'ert-x)

(ert-deftest mistty-term-test-split-incomplete-chars ()
  (ert-with-test-buffer ()
    (should (equal '("foo" . "")
                   (mistty--split-incomplete-chars "foo")))

    (should (equal '("foo bar abcde\342\224\200" . "")
                   (mistty--split-incomplete-chars
                    "foo bar abcde\342\224\200")))

    (should (equal '("foo bar abcde\342\224\200" . "\342\224")
                   (mistty--split-incomplete-chars
                    "foo bar abcde\342\224\200\342\224")))))

(ert-deftest mistty-term-translate-key ()
  (should (equal "a" (mistty-translate-key (kbd "a") 1)))
  (should (equal "aaa" (mistty-translate-key (kbd "a") 3)))

  (should (equal "\C-a" (mistty-translate-key (kbd "C-a") 1)))

  (should (equal "\ea" (mistty-translate-key (kbd "M-a") 1)))
  (should (equal "\ea\ea\ea" (mistty-translate-key (kbd "M-a") 3)))

  (should (equal mistty-left-str (mistty-translate-key (kbd "<left>") 1)))
  (should (equal mistty-right-str (mistty-translate-key (kbd "<right>") 1))))
