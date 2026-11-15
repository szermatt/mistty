;;; Tests mistty-eterm.el -*- lexical-binding: t -*-

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

(require 'mistty-eterm)
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

(ert-deftest mistty-register-prompt ()
  (let* ((cell (mistty--make-prompt-cell))
         (mistty--eterm (mistty--make-term-eterm :prompt-cell cell)))
    ;; All these will be merged
    (mistty--register-prompt 'regexp 10 :end 11 :text "prompt>")
    (mistty--register-prompt 'prompt_sp 9)
    (mistty--register-prompt 'bracketed-paste 10)
    (mistty--register-prompt 'osc133 10 :user-input-start '(11 . 3))

    (should (equal 1 (mistty--prompt-input-id (mistty--get-prompt cell))))
    (should (equal 'osc133 (mistty--prompt-source (mistty--get-prompt cell))))
    (should (equal 9 (mistty--prompt-start (mistty--get-prompt cell))))
    (should (equal nil (mistty--prompt-end (mistty--get-prompt cell))))
    (should (equal nil (mistty--prompt-text (mistty--get-prompt cell))))
    (should (equal '(11 . 3) (mistty--prompt-user-input-start (mistty--get-prompt cell))))

    ;; Prompt is properly ended and a new one starts in its place
    (setf (mistty--prompt-end (mistty--get-prompt cell)) 12)
    (mistty--register-prompt 'bracketed-paste 12)
    (should (equal 2 (mistty--prompt-input-id (mistty--get-prompt cell))))
    (should (equal 'bracketed-paste (mistty--prompt-source (mistty--get-prompt cell))))
    (should (equal 12 (mistty--prompt-start (mistty--get-prompt cell))))))


(ert-deftest mistty--prompt-merge-from ()
  ;; :input-id, unmodified
  (let ((p (mistty--make-prompt :input-id 1 :start 10))
        (other (mistty--make-prompt :input-id 2 :start 10)))
    (mistty--prompt-merge-from p other)
    (should (= (mistty--prompt-input-id p) 1)))

  ;; :source, most trusted
  (let ((p (mistty--make-prompt :source 'regexp :start 10))
        (other (mistty--make-prompt :source 'osc133 :start 10)))
    (mistty--prompt-merge-from p other)
    (should (eq (mistty--prompt-source p) 'osc133)))

  (let ((p (mistty--make-prompt :source 'osc133 :start 10))
        (other (mistty--make-prompt :source 'regexp :start 10)))
    (mistty--prompt-merge-from p other)
    (should (eq (mistty--prompt-source p) 'osc133)))

  (let ((p (mistty--make-prompt :source 'bracketed-paste :start 10))
        (other (mistty--make-prompt :source 'prompt_sp :start 10)))
    (mistty--prompt-merge-from p other)
    (should (eq (mistty--prompt-source p) 'bracketed-paste)))

  ;; :start, minimum
  (let ((p (mistty--make-prompt :start 10))
        (other (mistty--make-prompt :start 5)))
    (mistty--prompt-merge-from p other)
    (should (= (mistty--prompt-start p) 5)))

  (let ((p (mistty--make-prompt :start 5))
        (other (mistty--make-prompt :start 10)))
    (mistty--prompt-merge-from p other)
    (should (= (mistty--prompt-start p) 5)))

  ;; :end, maximum
  (let ((p (mistty--make-prompt :start 10 :end 12))
        (other (mistty--make-prompt :start 10 :end 15)))
    (mistty--prompt-merge-from p other)
    (should (= (mistty--prompt-end p) 15)))

  (let ((p (mistty--make-prompt :start 10 :end 15))
        (other (mistty--make-prompt :start 10 :end 12)))
    (mistty--prompt-merge-from p other)
    (should (= (mistty--prompt-end p) 15)))

  (let ((p (mistty--make-prompt :start 10 :end 12))
        (other (mistty--make-prompt :start 10 :end nil)))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-end p))))

  (let ((p (mistty--make-prompt :start 10 :end nil))
        (other (mistty--make-prompt :start 10 :end 12)))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-end p))))

  (let ((p (mistty--make-prompt :start 10 :end nil))
        (other (mistty--make-prompt :start 10 :end nil)))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-end p))))

  ;; :text, keep original but only for 'regexp
  (let ((p (mistty--make-prompt :source 'regexp :start 10 :text "prompt> "))
        (other (mistty--make-prompt :source 'regexp :start 10 :text "prompt> ")))
    (mistty--prompt-merge-from p other)
    (should (equal (mistty--prompt-text p) "prompt> ")))

  (let ((p (mistty--make-prompt :source 'regexp :start 10 :text "prompt> "))
        (other (mistty--make-prompt :source 'regexp :start 10 :text nil)))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-text p))))

  (let ((p (mistty--make-prompt :source 'regexp :start 10 :text nil))
        (other (mistty--make-prompt :source 'regexp :start 10 :text "prompt> ")))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-text p))))

  (let ((p (mistty--make-prompt :source 'osc133 :start 10 :text "prompt> "))
        (other (mistty--make-prompt :source 'osc133 :start 10 :text "prompt> ")))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-text p))))

  (let ((p (mistty--make-prompt :source 'regexp :start 10 :text "prompt> "))
        (other (mistty--make-prompt :source 'osc133 :start 10 :text "prompt> ")))
    (mistty--prompt-merge-from p other)
    (should (null (mistty--prompt-text p))))

  ;; :user-input-start, most recent value
  (let ((p (mistty--make-prompt :start 10 :user-input-start '(10 . 2)))
        (other (mistty--make-prompt :start 10 :user-input-start '(10 . 5))))
    (mistty--prompt-merge-from p other)
    (should (equal (mistty--prompt-user-input-start p) '(10 . 5))))

  (let ((p (mistty--make-prompt :start 10 :user-input-start '(10 . 2)))
        (other (mistty--make-prompt :start 10 :user-input-start nil)))
    (mistty--prompt-merge-from p other)
    (should (equal (mistty--prompt-user-input-start p) '(10 . 2)))))

(ert-deftest mistty-set-prompt ()
  (let ((cell (mistty--make-prompt-cell)))
    (mistty--set-prompt cell (mistty--make-prompt :input-id 1))
    (mistty--set-prompt cell (mistty--make-prompt :input-id 2))
    (mistty--set-prompt cell (mistty--make-prompt :input-id 3))
    (should (equal 3 (mistty--prompt-input-id (mistty--get-prompt cell))))
    (should (equal '(2 1) (mapcar #'mistty--prompt-input-id
                                  (mistty--prompt-cell-archive cell))))

    (should (equal (mistty--prompt-cell-current cell)
                   (mistty--get-prompt cell)))
    (should (equal (mistty--prompt-cell-counter cell) 3))

    (mistty--set-prompt cell nil)
    (should (null (mistty--get-prompt cell)))
    (should (equal '(3 2 1) (mapcar #'mistty--prompt-input-id
                                    (mistty--prompt-cell-archive cell))))))

(ert-deftest mistty-set-prompt-archive ()
  (let ((cell (mistty--make-prompt-cell)))
    (setf (mistty--prompt-cell-archive cell)
          (list (mistty--make-prompt :input-id 1)
                (mistty--make-prompt :input-id 2)))
    (should (equal '(1 2) (mapcar #'mistty--prompt-input-id
                                  (mistty--prompt-cell-archive cell))))
    (setf (mistty--prompt-cell-archive cell) nil)
    (should (null (mistty--prompt-cell-archive cell)))))

(ert-deftest mistty--prompt-source-order ()
  (should (mistty--prompt-source< 'osc133 'bracketed-paste))
  (should (mistty--prompt-source< 'osc133 'prompt_sp))
  (should (mistty--prompt-source< 'osc133 'regexp))
  (should (mistty--prompt-source< 'osc133 'unknown))

  (should-not (mistty--prompt-source< 'bracketed-paste 'osc133))
  (should (mistty--prompt-source< 'bracketed-paste 'prompt_sp))
  (should (mistty--prompt-source< 'bracketed-paste 'regexp))
  (should (mistty--prompt-source< 'bracketed-paste 'unknown))

  (should-not (mistty--prompt-source< 'prompt_sp 'osc133))
  (should-not (mistty--prompt-source< 'prompt_sp 'bracketed-paste))
  (should (mistty--prompt-source< 'prompt_sp 'regexp))
  (should (mistty--prompt-source< 'prompt_sp 'unknown))

  (should-not (mistty--prompt-source< 'regexp 'osc133))
  (should-not (mistty--prompt-source< 'regexp 'bracketed-paste))
  (should-not (mistty--prompt-source< 'regexp 'prompt_sp))
  (should (mistty--prompt-source< 'prompt_sp 'unknown))

  (should-not (mistty--prompt-source< 'unknown 'osc133))
  (should-not (mistty--prompt-source< 'unknown 'bracketed-paste))
  (should-not (mistty--prompt-source< 'unknown 'prompt_sp))
  (should-not (mistty--prompt-source< 'unknown 'regexp)))

(ert-deftest mistty-eterm-osc133-command-finished ()
  (ert-with-test-buffer ()
    (setq mistty--eterm (mistty--make-term-eterm
                         :buf (current-buffer)
                         :prompt-cell (mistty--make-prompt-cell)))
    (insert "line 0\nline 1\nline 2")
    (mistty-osc133 "133" "D")
    (mistty-osc133 "133" "D;0")
    (mistty-osc133 "133" "D;42")
    (mistty-osc133 "133" "D;7;aid=123")
    (mistty-osc133 "133" "D;;aid=123")
    (mistty-osc133 "133" "D;aid=123")
    ;; Ignored in fullscreen mode
    (setf (mistty--term-eterm-fs mistty--eterm) t)
    (mistty-osc133 "133" "D;99")
    (should (equal '((command-finished nil 2)
                     (command-finished 0 2)
                     (command-finished 42 2)
                     (command-finished 7 2)
                     (command-finished nil 2)
                     (command-finished nil 2))
                   (nreverse (mistty--term-eterm-events mistty--eterm))))))
