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
(require 'mistty-testing)
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

(ert-deftest mistty-util-test-remove-fake-nl ()
  (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
    (insert fake-nl "abc" fake-nl fake-nl "def" fake-nl "ghi\n" fake-nl )

    (mistty--remove-text-with-property 'term-line-wrap)
    (should (equal "abcdefghi\n"
                   (mistty--safe-bufstring (point-min) (point-max))))))

(ert-deftest mistty-util-test-remove-fake-nl-in-range ()
  (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
    (insert fake-nl "abc" fake-nl fake-nl "def" fake-nl "gh" fake-nl "i\n" fake-nl )

    (mistty--remove-fake-newlines
     (mistty-test-pos "abc") (mistty-test-pos "gh"))
    (should (equal (concat fake-nl "abcdefgh" fake-nl "i\n" fake-nl)
                   (buffer-string)))))

(ert-deftest mistty-util-test-remove-skipped-spaces ()
  (insert (propertize "   " 'mistty-skip t) "abc "
          (propertize "   " 'mistty-skip t) "def"
          (propertize "   " 'mistty-skip t))

  (mistty--remove-text-with-property 'mistty-skip)
  (should (equal "abc def"
                 (mistty--safe-bufstring (point-min) (point-max)))))

(ert-deftest mistty-util-test-remove-specific-val ()
  (insert (propertize "---" 'mistty-skip 'indent) "abc "
          (propertize "..." 'mistty-skip 'trailing) "def"
          (propertize "<<<" 'mistty-skip 'right-prompt))

  (mistty--remove-text-with-property 'mistty-skip (lambda (val) (eq 'trailing val)))
  (should (equal "---abc def<<<"
                 (mistty--safe-bufstring (point-min) (point-max)))))

(ert-deftest mistty-util-test-truncate-string ()
  (should (equal "abcd" (mistty--truncate-string "abcd" 5)))
  (should (equal "abcd" (mistty--truncate-string "abcd" 4)))
  (should (equal "abc..." (mistty--truncate-string "abcd" 3))))

(ert-deftest mistty-util-test-line-width ()
  (ert-with-test-buffer ()
    (insert "line 1: \n")
    (insert "line 2: 123\n")
    (insert "line 3: 123         \n")
    (insert "line 4: 123                  \n")

    (goto-char (point-min))
    (while (= 0 (forward-line 1))
      (should (equal (length (buffer-substring (pos-bol) (pos-eol)))
                     (mistty--line-width))))))

(ert-deftest mistty-util-has-text-pproperties ()
  (ert-with-test-buffer ()
    (insert (propertize "foo" 'a "a" 'b "b"))
    (insert (propertize "bar" 'b "c" 'd "d"))

    (should-not (mistty--has-text-properties 1 nil))
    (should (mistty--has-text-properties 1 '(a "a")))
    (should (mistty--has-text-properties 1 '(b "b")))
    (should (mistty--has-text-properties 1 '(a "a" b "b")))
    (should (mistty--has-text-properties 1 '(b "b" a "a")))
    (should-not (mistty--has-text-properties 1 '(a "b")))
    (should-not (mistty--has-text-properties 1 '(b "a")))
    (should-not (mistty--has-text-properties 1 '(a "a" b "c")))

    (should (mistty--has-text-properties 4 '(b "c" d "d")))))

(ert-deftest mistty-util-count-lines ()
  (ert-with-test-buffer ()
    (dotimes (i 10)
      (insert (format "line %s\n" i)))
    (should (equal 10 (mistty--count-lines (point-min) (point-max))))

    (should (equal 0 (mistty--count-lines (mistty-test-pos "line 1") (mistty-test-pos "line 1"))))

    (should (equal 3 (mistty--count-lines (mistty-test-pos "line 1") (mistty-test-pos "line 4"))))
    (should (equal -3 (mistty--count-lines (mistty-test-pos "line 4") (mistty-test-pos "line 1"))))))

(ert-deftest mistty-util-count-lines-pred ()
  (ert-with-test-buffer ()
    (dotimes (i 10)
      (insert (format "line %s.\nline %s!\n" (* i 2) (1+ (* i 2)))))

    (should (equal 20 (mistty--count-lines (point-min) (point-max))))
    (should (equal 10 (mistty--count-lines (point-min) (point-max)
                                           (lambda (pos)
                                             (eq ?. (char-before pos))))))))

(ert-deftest mistty-fake-nl-p ()
  (ert-with-test-buffer ()
    (insert "\n")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "\n")

    (should-not (mistty--fake-nl-p 1))
    (should (mistty--fake-nl-p 2))
    (should-not (mistty--fake-nl-p 3))

    (goto-char 2)
    (should (mistty--fake-nl-p))
    (goto-char 1)
    (should-not (mistty--fake-nl-p))))

(ert-deftest mistty-real-nl-p ()
  (ert-with-test-buffer ()
    (insert "a\n")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "\n")

    (should-not (mistty--real-nl-p 1)) ;; not nl
    (should (mistty--real-nl-p 2)) ;; nl
    (should-not (mistty--real-nl-p 3)) ;; fake
    (should (mistty--real-nl-p 4))

    (goto-char 2)
    (should (mistty--real-nl-p))
    (goto-char 3)
    (should-not (mistty--real-nl-p))))

(ert-deftest mistty-count-scrollrows ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl\n")

    (should (equal 0 (mistty--count-scrollrows (mistty-test-pos "a")
                                               (mistty-test-pos-after "c"))))
    (should (equal 0 (mistty--count-scrollrows (mistty-test-pos "abc")
                                          (mistty-test-pos-after "de"))))
    (should (equal 1 (mistty--count-scrollrows (mistty-test-pos "abc")
                                          (mistty-test-pos-after "def\n"))))

    (should (equal 1 (mistty--count-scrollrows (mistty-test-pos "abc")
                                               (mistty-test-pos "jkl"))))

    (should (equal 2 (mistty--count-scrollrows (point-min) (point-max))))))


(ert-deftest mistty--go-beginning-of-scrollrow ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr\n")

    (mistty-test-goto-after "abc")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "a" (char-to-string (char-after (point)))))

    (mistty-test-goto-after "def")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "a" (char-to-string (char-after (point)))))

    (mistty-test-goto "ghi")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto-after "ghi")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto-after "jk")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto-after "pqr")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "g" (char-to-string (char-after (point)))))

    ;; When at a newline, BOL is the char after.This is consistent
    ;; with pos-bol and forward-line,
    (mistty-test-goto-after "def\n")
    (mistty--go-beginning-of-scrollrow)
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto-after "pqr\n")
    (mistty--go-beginning-of-scrollrow)
    (should (equal (point-max) (point)))))

(ert-deftest mistty--go-beginning-of-scrollrow-pos ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl\n")

    (mistty-test-goto "def")
    (should (equal (point-min) (mistty--beginning-of-scrollrow-pos)))
    (should (equal "d" (char-to-string (char-after (point)))))))

(ert-deftest mistty--go-end-of-scrollrow ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr\n")

    (mistty-test-goto "abc")
    (mistty--go-end-of-scrollrow)
    (should (equal "f" (char-to-string (char-before (point)))))

    (mistty-test-goto "def")
    (mistty--go-end-of-scrollrow)
    (should (equal "f" (char-to-string (char-before (point)))))

    (mistty-test-goto "ghi")
    (mistty--go-end-of-scrollrow)
    (should (equal "r" (char-to-string (char-before (point)))))

    (mistty-test-goto "ghi")
    (mistty--go-end-of-scrollrow)
    (should (equal "r" (char-to-string (char-before (point)))))

    (mistty-test-goto "jk")
    (mistty--go-end-of-scrollrow)
    (should (equal "r" (char-to-string (char-before (point)))))

    (mistty-test-goto-after "pqr")
    (mistty--go-end-of-scrollrow)
    (should (equal "r" (char-to-string (char-before (point)))))

    (mistty-test-goto-after "def")
    (mistty--go-end-of-scrollrow)
    (should (equal "f" (char-to-string (char-before (point)))))

    (mistty-test-goto-after "pqr\n")
    (mistty--go-end-of-scrollrow)
    (should (equal (point-max) (point)))))

(ert-deftest mistty--go-end-of-scrollrow-pos ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")

    (mistty-test-goto "abc")
    (should (equal (mistty-test-pos-after "def") (mistty--end-of-scrollrow-pos)))
    (should (equal "a" (char-to-string (char-after (point)))))))

(ert-deftest mistty--go-down-scrollrows-0 ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr\n")
    (insert "stu")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "vwx")

    ;; n=0: beginning of scrollrow
    (goto-char (point-min))
    (should (zerop (mistty--go-down-scrollrows 0)))
    (should (equal (point) (point-min)))

    (mistty-test-goto "def")
    (should (zerop (mistty--go-down-scrollrows 0)))
    (should (equal (point) (point-min)))))


(ert-deftest mistty--go-down-scrollrows-positive ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr\n")
    (insert "stu")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "vwx\n")

    ;; n > 0: go down
    (mistty-test-goto "abc")
    (should (zerop (mistty--go-down-scrollrows 1)))
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto "def")
    (should (zerop (mistty--go-down-scrollrows 1)))
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto "ghi")
    (should (zerop (mistty--go-down-scrollrows 1)))
    (should (equal "s" (char-to-string (char-after (point)))))

    (mistty-test-goto "abc")
    (should (zerop (mistty--go-down-scrollrows 2)))
    (should (equal "s" (char-to-string (char-after (point)))))

    (mistty-test-goto "abc")
    (should (zerop (mistty--go-down-scrollrows 3)))
    (should (equal (point-max) (point)))

    (mistty-test-goto "abc")
    (should (equal 1 (mistty--go-down-scrollrows 4)))
    (should (equal (point-max) (point)))

    (mistty-test-goto "abc")
    (should (equal 2 (mistty--go-down-scrollrows 5)))
    (should (equal (point-max) (point)))))

(ert-deftest mistty--go-down-scrollrows-negative ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr\n")
    (insert "stu")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "vwx\n")

    ;; n < 0: go up
    (mistty-test-goto "vwx")
    (should (zerop (mistty--go-down-scrollrows -1)))
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto "stu")
    (should (zerop (mistty--go-down-scrollrows -1)))
    (should (equal "g" (char-to-string (char-after (point)))))

    (mistty-test-goto "vwx")
    (should (zerop (mistty--go-down-scrollrows -2)))
    (should (equal "a" (char-to-string (char-after (point)))))

    (mistty-test-goto "vwx")
    (should (equal -1 (mistty--go-down-scrollrows -3)))
    (should (equal "a" (char-to-string (char-after (point)))))

    (mistty-test-goto "vwx")
    (should (equal -2 (mistty--go-down-scrollrows -4)))
    (should (equal "a" (char-to-string (char-after (point)))))))

(ert-deftest mistty--go-up-scrollrows ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr\n")
    (insert "stu")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "vwx")

    (mistty-test-goto "vwx")
    (should (equal 2 (mistty--go-up-scrollrows 4)))

    (mistty-test-goto "abc")
    (should (equal -2 (mistty--go-up-scrollrows -4)))))

(ert-deftest mistty--current-scrollrow-text()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr")

    (mistty-test-goto "def")
    (should (equal "abcdef" (mistty--current-scrollrow-text)))

    (mistty-test-goto "mno")
    (should (equal "ghijklmnopqr" (mistty--current-scrollrow-text)))


    (mistty-test-goto "def")
    (put-text-property (mistty-test-pos "def")
                       (mistty-test-pos-after "def")
                       'mistty-test 'foobar)
    (should (equal 'foobar
                   (get-text-property 3 'mistty-test
                                      (mistty--current-scrollrow-text))))
    (should (equal nil
                   (get-text-property 3 'mistty-test
                                      (mistty--current-scrollrow-text 'no-properties))))))

(ert-deftest mistty--scrollrow-text-before-point ()
  (ert-with-test-buffer ()
    (insert "abc")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "def\n")
    (insert "ghi")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "jkl")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "mno")
    (insert (propertize "\n" 'term-line-wrap t))
    (insert "pqr")

    (mistty-test-goto-after "de")
    (should (equal "abcde" (mistty--scrollrow-text-before-point)))

    (mistty-test-goto "ef")
    (should (equal "abcd" (mistty--scrollrow-text-before-point)))))
