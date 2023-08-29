;;; Tests mistty-util.el -*- lexical-binding: t -*-

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

(require 'mistty-util)
(require 'ert)
(require 'ert-x)

(ert-deftest mistty-util-test-linecol ()
  (ert-with-test-buffer ()
    (insert "abcd\n")
    (insert "efgh\n")
    (insert "ijkl\n")

    (should (equal 0 (mistty--col (point-min))))
    (should (equal 0 (mistty--line (point-min))))
    
    (should (equal 2 (mistty--col (mistty-test-pos "c"))))
    (should (equal 0 (mistty--line (mistty-test-pos "c"))))

    (should (equal 2 (mistty--col (mistty-test-pos "g"))))
    (should (equal 1 (mistty--line (mistty-test-pos "g"))))

    (should (equal 1 (mistty--col (mistty-test-pos "j"))))
    (should (equal 2 (mistty--line (mistty-test-pos "j"))))))

(ert-deftest mistty-util-test-lines ()
  (ert-with-test-buffer ()
    (insert "abcd\n")
    (insert "efgh\n")
    (insert "ijkl")

    (should (equal (list 1 6 11)
                   (mapcar #'marker-position (mistty--lines))))))

(ert-deftest mistty-util-test-line-length ()
  (ert-with-test-buffer ()
    (insert "abc\n")
    (insert "def ghi\n")
    (insert "j\n")
    (insert "kl   mno p")

    (should (equal 3 (mistty--line-length (point-min))))
    (should (equal 7 (mistty--line-length (mistty-test-pos "g"))))
    (should (equal 1 (mistty--line-length (mistty-test-pos "j"))))
    (should (equal 10 (mistty--line-length (mistty-test-pos "n"))))))

(ert-deftest mistty-util-test-bol-skipping-fakes ()
  (ert-with-test-buffer ()
    (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
      
      (insert "abc" fake-nl "def" fake-nl "ghi\n")
      (insert "jkl" fake-nl "mno" fake-nl "pqr\n")
      (insert "stu" fake-nl "vwx" fake-nl "yz\n")

      (should
       (equal
        (point-min)
        (mistty--bol-skipping-fakes (mistty-test-pos "b"))))

      (should
       (equal
        (point-min)
        (mistty--bol-skipping-fakes (mistty-test-pos "e"))))

      (should
       (equal
        (point-min)
        (mistty--bol-skipping-fakes (mistty-test-pos "h"))))

      (should
       (equal
        (point-min)
        (mistty--bol-skipping-fakes (mistty-test-pos "h"))))

      (should
       (equal
        (point-min) 
        (mistty--bol-skipping-fakes (1+ (mistty-test-pos "h")))))

      (should
       (equal
        (mistty-test-pos "jkl")
        (mistty--bol-skipping-fakes (mistty-test-pos "j"))))

      (should
       (equal
        (mistty-test-pos "jkl")
        (mistty--bol-skipping-fakes (mistty-test-pos "r"))))

      (should
       (equal
        (mistty-test-pos "stu")
        (mistty--bol-skipping-fakes (mistty-test-pos "t"))))

      (should
       (equal
        (mistty-test-pos "stu")
        (mistty--bol-skipping-fakes (mistty-test-pos "z"))))

      ;; point must not have been moved after insert
      (should (equal (point-max) (point))))))

(ert-deftest mistty-util-test-indent ()
  (ert-with-test-buffer ()
    (insert "   abc\n")
    (insert "  def\n")
    (insert " ghi\n")
    (insert "jkl")

    (should (equal 3 (mistty--line-indent (mistty-test-pos "abc"))))
    (should (equal 2 (mistty--line-indent (mistty-test-pos "def"))))
    (should (equal 1 (mistty--line-indent (mistty-test-pos "ghi"))))
    (should (equal 0 (mistty--line-indent (mistty-test-pos "jkl"))))))

(ert-deftest test-mistty-distance-with-fake-nl ()
  (ert-with-test-buffer ()
   (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
     (insert "echo one tw" fake-nl
             "o three fou" fake-nl
             "r five six " fake-nl
             "seven eight" fake-nl
             "nine")

     (should (equal 4 (mistty--distance (mistty-test-pos "one")
                                        (mistty-test-pos "tw"))))
     (should (equal -4 (mistty--distance (mistty-test-pos "tw")
                                         (mistty-test-pos "one"))))

     (should (equal 4 (mistty--distance (mistty-test-pos "tw")
                                        (mistty-test-pos "three"))))
     (should (equal -4 (mistty--distance (mistty-test-pos "three")
                                         (mistty-test-pos "tw"))))

     (should (equal 19 (mistty--distance (mistty-test-pos "one")
                                         (mistty-test-pos "five"))))

     (should (equal 0 (mistty--distance (+ 3 (mistty-test-pos "fou"))
                                        (+ 4 (mistty-test-pos "fou"))))))))

(ert-deftest test-mistty-distance-skipped-spaces ()
  (ert-with-test-buffer ()
   (insert "$ for i in (seq 10)" (propertize "  " 'mistty-skip t)"\n"
           (propertize "      " 'mistty-skip t) "echo line $i\n"
           (propertize "  " 'mistty-skip t) "end")

   (should (equal 1 (mistty--distance (+ 3 (mistty-test-pos "10)"))
                                      (mistty-test-pos "echo"))))

   (should (equal 1 (mistty--distance (+ 3 (mistty-test-pos "10)"))
                                      (1- (mistty-test-pos "echo")))))

   (should (equal 1 (mistty--distance (+ 4 (mistty-test-pos "10)"))
                                      (mistty-test-pos "echo"))))
   (should (equal 1 (mistty--distance (+ 4 (mistty-test-pos "10)"))
                                        (1- (mistty-test-pos "echo")))))

   (should (equal 31 (mistty--distance (mistty-test-pos "for")
                                       (mistty-test-pos "end"))))))

(ert-deftest mistty-util-test-same-line ()
  (ert-with-test-buffer ()
    (insert "abc\n")
    (insert "def\n")

    (should (mistty--same-line-p
             (mistty-test-pos "a")
             (mistty-test-pos "a")))
    (should (mistty--same-line-p
             (mistty-test-pos "a")
             (1+ (mistty-test-pos "c"))))
    (should (not (mistty--same-line-p
                  (1+ (mistty-test-pos "c"))
                  (mistty-test-pos "d"))))
    (should (not (mistty--same-line-p
                  (mistty-test-pos "a")
                  (mistty-test-pos "d"))))))

(defun mistty-test-pos (text)
  (save-excursion
    (goto-char (point-min))
    (search-forward text)
    (match-beginning 0)))
