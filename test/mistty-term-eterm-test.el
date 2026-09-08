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

(ert-deftest test-mistty-hide-line-wraps ()
  (ert-with-test-buffer ()
    (let (line1 line2 line3 line4 line5 line6 (mistty-log t))
      (insert "abcdef" fakenl)
      (setq line1 (1- (point)))
      (insert "ghijkl\n") ;; not fake
      (setq line2 (1- (point)))
      (insert "mnopqr" fakenl)
      (setq line3 (1- (point)))
      (insert "stuvwx" fakenl)
      (setq line4 (1- (point)))
      (insert "yz...." fakenl)
      (setq line5 (1- (point)))
      (insert "the" fakenl)  ;; not at right column
      (setq line6 (1- (point)))
      (insert "end.\n")

      (mistty-log "lines: %s %s %s %s %s %s" line1 line2 line3 line4 line5 line6)

      (mistty--hide-line-wraps 1 1 6) ;; do nothing, but doesn't fail
      (mistty--hide-line-wraps 16 10 6) ;; do nothing, but doesn't fail
      (should (eq nil (get-text-property line1 'invisible)))
      (should (eq nil (get-text-property line2 'invisible)))
      (should (eq nil (get-text-property line3 'invisible)))
      (should (eq nil (get-text-property line4 'invisible)))
      (should (eq nil (get-text-property line5 'invisible)))
      (should (eq nil (get-text-property line6 'invisible)))

      ;; limit change to line3
      (mistty--hide-line-wraps
       (save-excursion (goto-char (point-min)) (search-forward "ghi"))
       (save-excursion (goto-char (point-min)) (search-forward "stu"))
       6)
      (should
       (equal
        (concat "abcdef\n"
                "ghijkl\n"
                "mnopqr[\n]"
                "stuvwx\n"
                "yz....\n"
                "the\n"
                "end.")
        (mistty-test-content :show-property '(invisible term-line-wrap))))
      (should (eq nil (get-text-property line6 'invisible)))

      ;; apply changes to the whole buffer
      (mistty--hide-line-wraps (point-min) (point-max) 6)

      (should
       (equal
        (concat "abcdef[\n]"
                "ghijkl\n"
                "mnopqr[\n]"
                "stuvwx[\n]"
                "yz....[\n]"
                "the\n""end.")
        (mistty-test-content :show-property '(invisible term-line-wrap))))

      (should (eq 'term-line-wrap (get-text-property line1 'invisible)))
      (should (eq nil (get-text-property line2 'invisible)))
      (should (eq 'term-line-wrap (get-text-property line3 'invisible)))
      (should (eq 'term-line-wrap (get-text-property line4 'invisible)))
      (should (eq 'term-line-wrap (get-text-property line5 'invisible)))
      (should (eq nil (get-text-property line6 'invisible)))

      (should (equal '(nil "" nil nil)
                     (get-text-property line1 'yank-handler))))))
