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
  (should (equal mistty-right-str (mistty-translate-key (kbd "<right>") 1)))

  (should (equal mistty-up-str (mistty-translate-key (kbd "<up>") 1)))
  (should (equal mistty-down-str (mistty-translate-key (kbd "<down>") 1))))

(ert-deftest mistty-test-postprocess-indent-and-end ()
  (ert-with-test-buffer ()
    (insert (concat "$ for i in a b c " (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat (propertize "    " 'mistty-maybe-skip t) "echo ok " (propertize "  " 'mistty-maybe-skip t) "\n"))
    (insert (concat "end" (propertize "    " 'mistty-maybe-skip t)))

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
    (insert (concat (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat (propertize "    " 'mistty-maybe-skip t) "echo foo\n"))
    (insert (concat (propertize "" 'mistty-maybe-skip t) "\n"))
    (insert (concat (propertize "    " 'mistty-maybe-skip t) "echo bar\n"))
    (insert (concat (propertize "                       " 'mistty-maybe-skip t) "\n"))
    (insert (concat (propertize "                       " 'mistty-maybe-skip t) "\n"))
    (insert (concat "end" (propertize "    " 'mistty-maybe-skip t)))

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
    (insert (concat "$ echo " (propertize "  " 'mistty-maybe-skip t) "ok " (propertize "    " 'mistty-maybe-skip t) "\n"))

    (mistty--term-postprocess (point-min) 80)

    (should (equal "$ echo   ok [    ]"
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-postprocess-ignore-nonws ()
  (ert-with-test-buffer ()
    (insert (propertize "$ echo foo bar" 'mistty-maybe-skip t))

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
      (insert (propertize (make-string spaces ?\ ) 'mistty-maybe-skip t))
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
      (insert (propertize (make-string spaces ?\ ) 'mistty-maybe-skip t))
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
      (insert (propertize (make-string spaces ?\ ) 'mistty-maybe-skip t))
      (insert right-prompt)
      (should (= (current-column) w))
      (insert "\n")

      (mistty--term-postprocess (point-min) w))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'trailing))
    (should (string-match "^\\[ + < right \\]$"
                          (mistty-test-content :show-property '(mistty-skip right-prompt))))))

(ert-deftest mistty-test-postprocess-ipython-continue-prompt ()
  (ert-with-test-buffer ()
    (insert (concat "In [3]: for i in (1, 2, 3):" (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat "   ...:   if i > 1:  " (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat "   ...:     print(i)  " (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat "In [133]: for i in (1, 2, 3):\n"))
    (insert (concat "     ...:     print(i)\n"))

    (mistty--term-postprocess (point-min) 80)

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should (equal
             (concat "In [3]: for i in (1, 2, 3):\n"
                     "[   ...: ]  if i > 1:\n"
                     "[   ...: ]    print(i)\n"
                     "In [133]: for i in (1, 2, 3):\n"
                     "[     ...: ]    print(i)")
                   (mistty-test-content :show-property '(mistty-skip continue-prompt))))))

(ert-deftest mistty-test-bridge-ws-with-props ()
  (ert-with-test-buffer ()
    (term-mode)
    (add-hook 'after-change-functions #'mistty--after-change-on-term nil t)
    (insert "  \n\n")
    (goto-char (point-min))
    (insert " ")
    (mistty-register-text-properties 'test '(myprop 1))
    (insert "foo")
    (goto-char (1- (point-max)))
    (insert "bar")

    (should (equal " [foo  \nbar]\n"
                   (mistty-test-content :show-property '(myprop 1) :trim nil)))))
