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
