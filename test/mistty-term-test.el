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

(ert-deftest mistty-add-to-prompt-archive ()
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    (setf (mistty--prompt) (mistty--make-prompt :input-id 1))
    (setf (mistty--prompt) (mistty--make-prompt :input-id 2))
    (setf (mistty--prompt) (mistty--make-prompt :input-id 3))
    (should (equal 3 (mistty--prompt-input-id (mistty--prompt))))
    (should (equal '(2 1) (mapcar #'mistty--prompt-input-id (mistty--prompt-archive))))

    (should (equal (mistty--prompt-cell-current mistty--prompt-cell)
                   (mistty--prompt)))
    (should (equal (mistty--prompt-cell-archive mistty--prompt-cell)
                   (mistty--prompt-archive)))
    (should (equal (mistty--prompt-cell-counter mistty--prompt-cell)
                   3))

    (setf (mistty--prompt) nil)
    (should (null (mistty--prompt)))
    (should (equal '(3 2 1) (mapcar #'mistty--prompt-input-id (mistty--prompt-archive))))))

(ert-deftest mistty-set-prompt-archive ()
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    (setf (mistty--prompt-archive) (list (mistty--make-prompt :input-id 1)
                                         (mistty--make-prompt :input-id 2)))
    (should (equal '(1 2) (mapcar #'mistty--prompt-input-id (mistty--prompt-archive))))
    (setf (mistty--prompt-archive) nil)
    (should (null (mistty--prompt-archive)))))

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

(ert-deftest mistty-register-prompt ()
  (let ((mistty--prompt-cell (mistty--make-prompt-cell)))
    ;; All these will be merged
    (mistty--register-prompt 'regexp 10 :end 11 :text "prompt>")
    (mistty--register-prompt 'prompt_sp 9)
    (mistty--register-prompt 'bracketed-paste 10)
    (mistty--register-prompt 'osc133 10 :user-input-start '(11 . 3))

    (should (equal 1 (mistty--prompt-input-id (mistty--prompt))))
    (should (equal 'osc133 (mistty--prompt-source (mistty--prompt))))
    (should (equal 9 (mistty--prompt-start (mistty--prompt))))
    (should (equal nil (mistty--prompt-end (mistty--prompt))))
    (should (equal nil (mistty--prompt-text (mistty--prompt))))
    (should (equal '(11 . 3) (mistty--prompt-user-input-start (mistty--prompt))))

    ;; Prompt is properly ended and a new one starts in its place
    (setf (mistty--prompt-end (mistty--prompt)) 12)
    (mistty--register-prompt 'bracketed-paste 12)
    (should (equal 2 (mistty--prompt-input-id (mistty--prompt))))
    (should (equal 'bracketed-paste (mistty--prompt-source (mistty--prompt))))
    (should (equal 12 (mistty--prompt-start (mistty--prompt))))))
