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
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    (should (mistty--prompt-contains (mistty--make-prompt 'test 10) 10))
    (should-not (mistty--prompt-contains (mistty--make-prompt 'test 10) 9))
    (should-not (mistty--prompt-contains (mistty--make-prompt 'test 10) 1))
    (should (mistty--prompt-contains (mistty--make-prompt 'test 10) 11))
    (should (mistty--prompt-contains (mistty--make-prompt 'test 10) 100))))

(ert-deftest mistty-prompt-contains-closed ()
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    (should (mistty--prompt-contains (mistty--make-prompt 'test 10 12) 10))
    (should (mistty--prompt-contains (mistty--make-prompt 'test 10 12) 11))
    (should-not (mistty--prompt-contains (mistty--make-prompt 'test 10 12) 9))
    (should-not (mistty--prompt-contains (mistty--make-prompt 'test 10 12) 12))
    (should-not (mistty--prompt-contains (mistty--make-prompt 'test 10 12) 1))
    (should-not (mistty--prompt-contains (mistty--make-prompt 'test 10 12) 100))))

(ert-deftest mistty-add-to-prompt-archive ()
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    (setf (mistty--prompt) (mistty--make-prompt 'test 1))
    (setf (mistty--prompt) (mistty--make-prompt 'test 2))
    (setf (mistty--prompt) (mistty--make-prompt 'test 3))
    (should (equal 3 (mistty--prompt-start (mistty--prompt))))
    (should (equal '(2 1) (mapcar #'mistty--prompt-start (mistty--prompt-archive))))

    (should (equal (mistty--prompt-cell-current mistty--prompt-cell)
                   (mistty--prompt)))
    (should (equal (mistty--prompt-cell-archive mistty--prompt-cell)
                   (mistty--prompt-archive)))
    (should (equal (mistty--prompt-cell-counter mistty--prompt-cell)
                   3))

    (setf (mistty--prompt) nil)
    (should (null (mistty--prompt)))
    (should (equal '(3 2 1) (mapcar #'mistty--prompt-start (mistty--prompt-archive))))))

(ert-deftest mistty-set-prompt-archive ()
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    (setf (mistty--prompt-archive) (list (mistty--make-prompt 'test 1)
                                         (mistty--make-prompt 'test 2)))
    (should (equal '(1 2) (mapcar #'mistty--prompt-start (mistty--prompt-archive))))
    (setf (mistty--prompt-archive) nil)
    (should (null (mistty--prompt-archive)))))
