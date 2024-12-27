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

(ert-deftest mistty-test-prepare-term-for-refresh-indent-and-end ()
  (ert-with-test-buffer ()
    (setq-local term-width 80)

    (insert (concat "$ for i in a b c " (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat (propertize "    " 'mistty-maybe-skip t) "echo ok " (propertize "  " 'mistty-maybe-skip t) "\n"))
    (insert (concat "end" (propertize "    " 'mistty-maybe-skip t)))

    (put-text-property (point-min) (point-max) 'mistty-changed t)
    (mistty--prepare-term-for-refresh (current-buffer) (point-min))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should (equal (concat "$ for i in a b c\n"
                           "[    ]echo ok\n"
                           "end")
                   (mistty-test-content :show-property '(mistty-skip indent))))
    (should (equal (concat "$ for i in a b c [    ]\n"
                           "    echo ok [  ]\n"
                           "end[    ]")
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-prepare-term-for-refresh-ignore-skip-in-the-middle ()
  (ert-with-test-buffer ()
    (setq-local term-width 80)

    (insert (concat "$ echo " (propertize "  " 'mistty-maybe-skip t) "ok " (propertize "    " 'mistty-maybe-skip t) "\n"))

    (put-text-property (point-min) (point-max) 'mistty-changed t)
    (mistty--prepare-term-for-refresh (current-buffer) (point-min))

    (should (equal "$ echo   ok [    ]"
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-prepare-term-for-refresh-ignore-unchanged ()
  (ert-with-test-buffer ()
    (setq-local term-width 80)

    (insert (concat "$ for i in a b c " (propertize "    " 'mistty-maybe-skip t) "\n"))
    (insert (concat (propertize "    " 'mistty-maybe-skip t) "echo ok " (propertize "  " 'mistty-maybe-skip t) "\n"))
    (insert (concat "end" (propertize "    " 'mistty-maybe-skip t)))

    (goto-char (point-min))
    (put-text-property (search-forward "end") (point-max) 'mistty-changed t)
    (mistty--prepare-term-for-refresh (current-buffer) (point-min))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should (equal (concat "$ for i in a b c\n"
                           "    echo ok\n"
                           "end[    ]")
                   (mistty-test-content :show-property '(mistty-skip trailing))))))

(ert-deftest mistty-test-prepare-term-for-refresh-ignore-nonws ()
  (ert-with-test-buffer ()
    (setq-local term-width 80)

    (insert (propertize "$ echo foo bar" 'mistty-maybe-skip t))

    (put-text-property (point-min) (point-max) 'mistty-changed t)
    (mistty--prepare-term-for-refresh (current-buffer) (point-min))

    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'indent))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'right-prompt))
    (should-not (text-property-any (point-min) (point-max) 'mistty-skip 'trailing))))

(ert-deftest mistty-test-prepare-term-for-refresh-right-prompt ()
  (ert-with-test-buffer ()
    (select-window (display-buffer (current-buffer)))
    (delete-other-windows)
    (setq-local term-width 80)

    (let* ((w term-width)
           (left-prompt " left > ")
           (right-prompt " < right ")
           (spaces (- w (length left-prompt) (length right-prompt))))
      (insert left-prompt)
      (insert (propertize (make-string spaces ?\ ) 'mistty-maybe-skip t))
      (insert right-prompt)
      (should (= (current-column) w))
      (insert "\n")

      (put-text-property (point-min) (point-max) 'mistty-changed t)
      (mistty--prepare-term-for-refresh (current-buffer) (point-min)))

    (should (string-match "^ left > \\[ +\\] < right"
                          (mistty-test-content :show-property '(mistty-skip trailing))))
    (should (string-match "^ left >  +\\[ < right \\]$"
                          (mistty-test-content :show-property '(mistty-skip right-prompt))))))
