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

(ert-deftest mistty-prompt-contains-open-ended ()
  (let ((p (mistty--make-prompt :start 10)))
    (should (mistty--prompt-contains p 10))
    (should-not (mistty--prompt-contains p 9))
    (should-not (mistty--prompt-contains p 1))
    (should (mistty--prompt-contains p 11))
    (should (mistty--prompt-contains p 100))))

(ert-deftest mistty-prompt-contains-closed ()
  (let ((p (mistty--make-prompt :start 10 :end 12)))
    (should (mistty--prompt-contains p 10))
    (should (mistty--prompt-contains p 11))
    (should-not (mistty--prompt-contains p 9))
    (should-not (mistty--prompt-contains p 12))
    (should-not (mistty--prompt-contains p 1))
    (should-not (mistty--prompt-contains p 100))))

(ert-deftest mistty-prompt-overlaps ()
  (pcase-dolist (`(,start-a ,end-a ,start-b ,end-b ,overlaps)
                 '((10 12 9 11 t)
                   (10 12 9 nil t)
                   (10 nil 9 11 t)
                   (10 nil 9 10 nil)
                   (10 12 9 10 nil)
                   (10 12 8 9 nil)
                   (10 nil 8 9)))
    (should (equal
             overlaps
             (mistty--prompt-overlaps
              (mistty--make-prompt :start start-a :end end-a)
              (mistty--make-prompt :start start-b :end end-b))))
    (should (equal
             overlaps
             (mistty--prompt-overlaps
              (mistty--make-prompt :start start-b :end end-b)
              (mistty--make-prompt :start start-a :end end-a))))))
