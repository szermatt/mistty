;;; Tests mistty-term-eterm.el -*- lexical-binding: t -*-

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

(require 'mistty-term-eterm)
(require 'test/mistty-testing)
(require 'ert)
(require 'ert-x)

(ert-deftest mistty-test-postprocess-indent-and-end ()
  (ert-with-test-buffer ()
    (insert (concat "$ for i in a b c " (propertize "    " 'mistty-clear t) "\n"))
    (insert (concat (propertize "    " 'mistty-clear t) "echo ok " (propertize "  " 'mistty-clear t) "\n"))
    (insert (concat "end" (propertize "    " 'mistty-clear t)))

    (mistty--term-postprocess (point-min) 80)

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should (equal (concat "$ for i in a b c\n"
                           "[    ]echo ok\n"
                           "end")
                   (mistty-test-content :show-property '(mistty-skip indent))))
    (should (equal (concat "$ for i in a b c [    ]\n"
                           "    echo ok [  ]\n"
                           "end[    ]")
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-postprocess-indent-empty-lines ()
  (ert-with-test-buffer ()
    (insert "$ for i in a b c\n")
    (insert (concat (propertize "    " 'mistty-clear t) "\n"))
    (insert (concat (propertize "    " 'mistty-clear t) "echo foo\n"))
    (insert (concat (propertize "" 'mistty-clear t) "\n"))
    (insert (concat (propertize "    " 'mistty-clear t) "echo bar\n"))
    (insert (concat (propertize "                       " 'mistty-clear t) "\n"))
    (insert (concat (propertize "                       " 'mistty-clear t) "\n"))
    (insert (concat "end" (propertize "    " 'mistty-clear t)))

    (mistty--term-postprocess (point-min) 80)

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should (equal (concat "$ for i in a b c\n"
                           "\n"
                           "[    ]echo foo\n"
                           "\n"
                           "[    ]echo bar\n"
                           "[    ]\n"
                           "[    ]\n"
                           "end")
                   (mistty-test-content :show-property '(mistty-skip indent))))
    (should (equal (concat "$ for i in a b c\n"
                           "[    ]\n"
                           "    echo foo\n"
                           "\n"
                           "    echo bar\n"
                           "    [                   ]\n"
                           "    [                   ]\n"
                           "end[    ]")
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-postprocess-ignore-skip-in-the-middle ()
  (ert-with-test-buffer ()
    (insert (concat "$ echo " (propertize "  " 'mistty-clear t) "ok " (propertize "    " 'mistty-clear t) "\n"))

    (mistty--term-postprocess (point-min) 80)

    (should (equal "$ echo   ok [    ]"
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-postprocess-ignore-nonws ()
  (ert-with-test-buffer ()
    (insert (propertize "$ echo foo bar" 'mistty-clear t))

    (mistty--term-postprocess (point-min) 80)

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'trailing))))

(ert-deftest mistty-test-postprocess-right-prompt ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (delete-other-windows)

    (let* ((w 80)
           (left-prompt " left > ")
           (right-prompt " < right ")
           (spaces (- w (length left-prompt) (length right-prompt))))
      (insert left-prompt)
      (insert (propertize (make-string spaces ?\ ) 'mistty-clear t))
      (insert right-prompt)
      (should (= (current-column) w))
      (insert "\n")

      (mistty--term-postprocess (point-min) w))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'trailing))
    (should (string-match "^ left > \\[ + < right \\]$"
                          (mistty-test-content :show-property '(mistty-skip right-prompt))))))

(ert-deftest mistty-test-postprocess-right-prompt-with-tolerance ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (delete-other-windows)

    (let* ((w 80)
           (left-prompt " left > ")
           (right-prompt " < right ")
           (spaces (- w (length left-prompt) (length right-prompt) 2)))
      (insert left-prompt)
      (insert (propertize (make-string spaces ?\ ) 'mistty-clear t))
      (insert right-prompt)
      (insert "\n")

      (mistty--term-postprocess (point-min) w))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'trailing))
    (should (string-match "^ left > \\[ + < right \\]$"
                          (mistty-test-content :show-property '(mistty-skip right-prompt))))))

(ert-deftest mistty-test-postprocess-empty-right-prompt ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (delete-other-windows)

    (let* ((w 80)
           (right-prompt " < right ")
           (spaces (- w (length right-prompt))))
      (insert (propertize (make-string spaces ?\ ) 'mistty-clear t))
      (insert right-prompt)
      (should (= (current-column) w))
      (insert "\n")

      (mistty--term-postprocess (point-min) w))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'trailing))
    (should (string-match "^\\[ + < right \\]$"
                          (mistty-test-content :show-property '(mistty-skip right-prompt))))))
