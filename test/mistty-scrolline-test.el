;;; Tests mistty-scrolline.el -*- lexical-binding: t -*-

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

(require 'mistty-scrolline)

(require 'ert)
(require 'ert-x)
(require 'pcase)

(defconst mistty-test-fakenl (propertize "\n" 'term-line-wrap t)
  "A fake newline.")

(defun --word-after ()
  "Return the word after point."
  (save-excursion
    (let ((pos (point)))
      (goto-char pos)
      (forward-word)
      (buffer-substring-no-properties pos (point)))))

(defun --chars-after (count)
  "Return COUNT chars, after point."
  (buffer-substring-no-properties
   (point) (min (point-max) (+ count (point)))))

(ert-deftest mistty-scrolline-goto-start ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz")
    (goto-char (point-min))

    (save-excursion
      (mistty--goto-scrolline-start)
      (should (equal (point) (point-min))))

    (save-excursion
      (search-forward "abc")
      (mistty--goto-scrolline-start)
      (should (equal (point) (point-min))))

    (save-excursion
      (search-forward "def")
      (mistty--goto-scrolline-start)
      (should (equal (point) (point-min))))

    (save-excursion
      (search-forward "ghi")
      (mistty--goto-scrolline-start)
      (should (equal (point) (point-min))))

    (save-excursion
      (search-forward "mno")
      (mistty--goto-scrolline-start)
      (should (equal "jkl" (--word-after))))

    (save-excursion
      (search-forward "vwx")
      (mistty--goto-scrolline-start)
      (should (equal "stu" (--word-after))))

    (save-excursion
      (search-forward "yz")
      (mistty--goto-scrolline-start)
      (should (equal "stu" (--word-after))))))

(ert-deftest mistty-scrolline-goto-start-empty-buffer ()
  (ert-with-test-buffer ()
    (mistty--goto-scrolline-start)
    (should (equal (point) (point-min)))))

(ert-deftest mistty-scrolline-goto-start-nl-at-eob ()
  (ert-with-test-buffer ()
    (insert "foo" mistty-test-fakenl "bar\n")
    (goto-char (point-max))
    (mistty--goto-scrolline-start)
    (should (equal (point) (point-max)))))

(ert-deftest mistty-scrolline-goto-start-empty-lines ()
  (ert-with-test-buffer ()
    (insert "foo" mistty-test-fakenl "bar\n\nend")
    (goto-char (point-min))
    (save-excursion
      (search-forward "end")
      (mistty--goto-scrolline-start)
      (should (equal "end" (--chars-after 3))))

    (save-excursion
      (search-forward "bar\n")
      (mistty--goto-scrolline-start)
      (should (equal "\nend" (--chars-after 4))))

    (save-excursion
      (search-forward "bar")
      (mistty--goto-scrolline-start)
      (should (equal "foo" (--chars-after 3))))))

(ert-deftest mistty-scrolline-start-pos ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def\njkl\n")
    (goto-char (point-min))

    (save-excursion
      (search-forward "def")
      (let ((orig-point (point)))
        (should (equal (point-min) (mistty--scrolline-start-pos)))
        (should (equal orig-point (point)))))

    (save-excursion
      (search-forward "jkl")
      (let ((line-start (match-beginning 0))
            (orig-point (point)))
        (should (equal line-start (mistty--scrolline-start-pos)))
        (should (equal orig-point (point)))))))

(ert-deftest mistty-scrolline-goto-end ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (save-excursion
      (mistty--goto-scrolline-end)
      (should (equal "\njkl" (--chars-after 4))))

    (save-excursion
      (search-forward "ab")
      (mistty--goto-scrolline-end)
      (should (equal "\njkl" (--chars-after 4))))

    (save-excursion
      (search-forward "abc")
      (mistty--goto-scrolline-end)
      (should (equal "\njkl" (--chars-after 4))))

    (save-excursion
      (search-forward "de")
      (mistty--goto-scrolline-end)
      (should (equal "\njkl" (--chars-after 4))))

    (save-excursion
      (search-forward "gh")
      (mistty--goto-scrolline-end)
      (should (equal "\njkl" (--chars-after 4))))

    (save-excursion
      (search-forward "ghi")
      (mistty--goto-scrolline-end)
      (should (equal "\njkl" (--chars-after 4))))

    (save-excursion
      (search-forward "ghi\n")
      (mistty--goto-scrolline-end)
      (should (equal "\nstu" (--chars-after 4))))

    (save-excursion
      (search-forward "mno")
      (mistty--goto-scrolline-end)
      (should (equal "\nstu" (--chars-after 4))))

    (save-excursion
      (search-forward "yz")
      (mistty--goto-scrolline-end)
      (should (equal (1- (point-max)) (point))))

    (save-excursion
      (goto-char (point-max))
      (mistty--goto-scrolline-end)
      (should (equal (point-max) (point))))))

(ert-deftest mistty-scrolline-goto-end-empty-buffer ()
  (ert-with-test-buffer ()
    (mistty--goto-scrolline-end)
    (should (equal (point) (point-min)))))

(ert-deftest mistty-scrolline-goto-end-no-nl-at-eob ()
  (ert-with-test-buffer ()
    (insert "foo" mistty-test-fakenl "bar")
    (search-backward "foo")
    (mistty--goto-scrolline-end)
    (should (equal (point) (point-max)))))

(ert-deftest mistty-scrolline-end-pos ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def\njkl")
    (goto-char (point-min))

    (save-excursion
      (let ((orig-point (point)))
        (should (equal (save-excursion
                         (search-forward "def"))
                       (mistty--scrolline-end-pos)))
        (should (equal orig-point (point)))))

    (save-excursion
      (search-forward "jk")
      (let ((orig-point (point)))
        (should (equal (point-max) (mistty--scrolline-end-pos)))
        (should (equal orig-point (point)))))))


(ert-deftest mistty-scrolline-range ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz")
    (goto-char (point-min))

    (save-excursion
      (search-forward "e")
      (should (equal "abc\ndef\nghi"
                     (pcase-let* ((`(,beg . ,end) (mistty--scrolline-range)))
                       (buffer-substring-no-properties beg end)))))

    (save-excursion
      (search-forward "n")
      (should (equal "jkl\nmno\npqr"
                     (pcase-let* ((`(,beg . ,end) (mistty--scrolline-range)))
                       (buffer-substring-no-properties beg end)))))

    (save-excursion
      (search-forward "w")
      (should (equal "stu\nvwx\nyz"
                     (pcase-let* ((`(,beg . ,end) (mistty--scrolline-range)))
                       (buffer-substring-no-properties beg end)))))))

(ert-deftest mistty-scrolline-range-eob ()
  (insert "foobar")

  (should (equal (cons (point-min) (point-max)) (mistty--scrolline-range)))

  (insert "\n")
  (should (equal (cons (point-max) (point-max)) (mistty--scrolline-range))))

(ert-deftest mistty-scrolline-move-scrollines-0 ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (save-excursion
      (search-forward "mno")
      (mistty--move-scrollines 0)
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "jkl")
      (mistty--move-scrollines 0)
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "def")
      (mistty--move-scrollines 0)
      (should (equal "abc" (--chars-after 3))))))

(ert-deftest mistty-scrolline-move-scrollines-forward ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (save-excursion
      (search-forward "abc")
      (should (equal 0 (mistty--move-scrollines 1)))
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "def")
      (should (equal 0 (mistty--move-scrollines 1)))
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "ghi")
      (should (equal 0 (mistty--move-scrollines 1)))
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "mno")
      (should (equal 0 (mistty--move-scrollines 1)))
      (should (equal "stu" (--chars-after 3))))

    (save-excursion
      (search-forward "stu")
      (should (equal 0 (mistty--move-scrollines 1)))
      (should (equal (point-max) (point))))

    (save-excursion
      (search-forward "abc")
      (should (equal 0 (mistty--move-scrollines 2)))
      (should (equal "stu" (--chars-after 3))))))

(ert-deftest mistty-scrolline-move-scrollines-forward-too-far ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (save-excursion
      (search-forward "abc")
      (should (equal 0 (mistty--move-scrollines 3)))
      (should (equal (point-max) (point))))

    (save-excursion
      (search-forward "abc")
      (should (equal 1 (mistty--move-scrollines 4)))
      (should (equal (point-max) (point))))

    (save-excursion
      (search-forward "abc")
      (should (equal 2 (mistty--move-scrollines 5)))
      (should (equal (point-max) (point))))))

(ert-deftest mistty-scrolline-move-scrollines-backward ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (save-excursion
      (search-forward "y")
      (should (equal 0 (mistty--move-scrollines -1)))
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "w")
      (should (equal 0 (mistty--move-scrollines -1)))
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (search-forward "vwx")
      (should (equal 0 (mistty--move-scrollines -2)))
      (should (equal "abc" (--chars-after 3))))

    (save-excursion
      (goto-char (point-max))
      (should (equal 0 (mistty--move-scrollines -1)))
      (should (equal "stu" (--chars-after 3))))

    (save-excursion
      (goto-char (point-max))
      (should (equal 0 (mistty--move-scrollines -2)))
      (should (equal "jkl" (--chars-after 3))))

    (save-excursion
      (goto-char (point-max))
      (should (equal 0 (mistty--move-scrollines -3)))
      (should (equal "abc" (--chars-after 3))))))

(ert-deftest mistty-scrolline-move-scrollines-backward-too-far ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (save-excursion
      (search-forward "y")
      (should (equal 1 (mistty--move-scrollines -3)))
      (should (equal (point-min) (point))))

    (save-excursion
      (search-forward "y")
      (should (equal 2 (mistty--move-scrollines -4)))
      (should (equal (point-min) (point))))

    (save-excursion
      (search-forward "y")
      (should (equal 3 (mistty--move-scrollines -5)))
      (should (equal (point-min) (point))))

    (save-excursion
      (should (equal 1 (mistty--move-scrollines -1)))
      (should (equal (point-min) (point))))

    (save-excursion
      (should (equal 2 (mistty--move-scrollines -2)))
      (should (equal (point-min) (point))))))

(ert-deftest mistty-scrolline-count ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (should (equal 0 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "a")
                        (point))
                      (save-excursion
                        (search-forward "f")
                        (point)))))

    (should (equal 0 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "a")
                        (point))
                      (save-excursion
                        (search-forward "i")
                        (point)))))

    (should (equal 1 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "a")
                        (point))
                      (save-excursion
                        (search-forward "n")
                        (point)))))

    (should (equal 2 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "a")
                        (point))
                      (save-excursion
                        (search-forward "y")
                        (point)))))

    (should (equal 3 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "a")
                        (point))
                      (point-max))))

    (should (equal 3 (mistty--count-scrollines
                      (point-min)
                      (point-max))))

    (should (equal 1 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "n")
                        (point))
                      (save-excursion
                        (search-forward "w")
                        (point)))))))

(ert-deftest mistty-scrolline-count-backward ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (should (equal 0 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "f")
                        (point))
                      (save-excursion
                        (search-forward "a")
                        (point)))))

    (should (equal 0 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "i")
                        (point))
                      (save-excursion
                        (search-forward "a")
                        (point)))))

    (should (equal -1 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "n")
                        (point))
                      (save-excursion
                        (search-forward "a")
                        (point)))))

    (should (equal -2 (mistty--count-scrollines
                      (save-excursion
                        (search-forward "w")
                        (point))
                      (save-excursion
                        (search-forward "a")
                        (point)))))

    (should (equal -3 (mistty--count-scrollines
                       (point-max)
                       (point-min))))))

(ert-deftest mistty-scrolline-unwrap-lines-partial ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (should (equal
             4
             (mistty--unwrap-lines
              (save-excursion (search-forward "e")
                              (point))
              (save-excursion (search-forward "w")
                              (point)))))

    (should (equal (concat "abc\ndefghi\n"
                           "jklmnopqr\n"
                           "stuvwx\nyz\n")
                   (buffer-string)))))

(ert-deftest mistty-scrolline-unwrap-lines-full ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (should (equal 6 (mistty--unwrap-lines (point-min) (point-max))))

    (should (equal (concat "abcdefghi\n"
                           "jklmnopqr\n"
                           "stuvwxyz\n")
                   (buffer-string)))))

(ert-deftest mistty-scrolline-unwrap-lines-backward ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (should (equal 4
                   (mistty--unwrap-lines
                    (save-excursion (search-forward "w")
                                    (point))
                    (save-excursion (search-forward "e")
                                    (point)))))

    (should (equal (concat "abc\ndefghi\n"
                           "jklmnopqr\n"
                           "stuvwx\nyz\n")
                   (buffer-string)))))

(ert-deftest mistty-scrolline-unwrap-lines-boundaries ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (goto-char (point-min))

    ;; beg is on a mistty-test-fakenl; it'll be removed (inclusive)
    ;; end is also on a mistty-test-fakenl; it won't be removed (exclusive)
    (mistty--unwrap-lines
     (save-excursion (search-forward "f")
                     (point))
     (save-excursion (search-forward "o")
                     (point)))

    (should (equal (concat "abc\ndefghi\n"
                           "jklmno\npqr\n")
                   (buffer-string)))))

(ert-deftest mistty-scrolline-at ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (equal 0 (mistty--scrolline-at-point))
    (equal 0 (mistty--scrolline-at (save-excursion
                                    (search-forward "def")
                                    (match-beginning 0))))
    (equal 0 (mistty--scrolline-at (save-excursion
                                    (search-forward "ghi")
                                    (match-end 0))))
    (equal 1 (mistty--scrolline-at (save-excursion
                                    (search-forward "jkl")
                                    (match-beginning 0))))
    (equal 1 (mistty--scrolline-at (save-excursion
                                    (search-forward "mno")
                                    (match-beginning 0))))
    (equal 1 (mistty--scrolline-at (save-excursion
                                    (search-forward "vwx")
                                    (match-beginning 0))))))

(ert-deftest mistty-scrolline-at-with-home ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (mistty--update-scrolline (save-excursion
                                (search-forward "mno")
                                (match-beginning 0))
                              10)

    (equal 9 (mistty--scrolline-at-point))
    (equal 9 (mistty--scrolline-at (save-excursion
                                    (search-forward "def")
                                    (match-beginning 0))))
    (equal 10 (mistty--scrolline-at (save-excursion
                                     (search-forward "jkl")
                                     (match-beginning 0))))
    (equal 10 (mistty--scrolline-at (save-excursion
                                     (search-forward "pqr")
                                     (match-beginning 0))))
    (equal 11 (mistty--scrolline-at (save-excursion
                                     (search-forward "vwx")
                                     (match-beginning 0))))

    (mistty--update-scrolline (save-excursion
                                (search-forward "yz")
                                (match-end 0))
                              100)
    (equal 98 (mistty--scrolline-at (save-excursion
                                     (search-forward "def")
                                     (match-beginning 0))))
    (equal 99 (mistty--scrolline-at (save-excursion
                                     (search-forward "jkl")
                                     (match-beginning 0))))
    (equal 100 (mistty--scrolline-at (save-excursion
                                      (search-forward "vwx")
                                      (match-beginning 0))))

    (mistty--update-scrolline (point-max) 200)
    (equal 197 (mistty--scrolline-at (save-excursion
                                     (search-forward "def")
                                     (match-beginning 0))))
    (equal 198 (mistty--scrolline-at (save-excursion
                                      (search-forward "jkl")
                                     (match-beginning 0))))
    (equal 199 (mistty--scrolline-at (save-excursion
                                      (search-forward "vwx")
                                      (match-beginning 0))))))

(ert-deftest mistty-scrolline-find ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (should (equal (point-min) (mistty--find-scrolline 0)))
    (should (equal (save-excursion
                     (search-forward "jkl")
                     (match-beginning 0))
                   (mistty--find-scrolline 1)))
    (should (equal (save-excursion
                     (search-forward "stu")
                     (match-beginning 0))
                   (mistty--find-scrolline 2)))

    (should (equal (point-max) (mistty--find-scrolline 3)))
    (should (equal nil (mistty--find-scrolline 10)))
    (should (equal nil (mistty--find-scrolline -1)))
    (should (equal nil (mistty--find-scrolline -10)))))

(ert-deftest mistty-scrolline-find-with-home ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (mistty--update-scrolline (save-excursion
                                (search-forward "mno")
                                (match-beginning 0))
                              10)

    (should (equal nil (mistty--find-scrolline 0)))
    (should (equal nil (mistty--find-scrolline 8)))
    (should (equal (point-min) (mistty--find-scrolline 9)))
    (should (equal (save-excursion
                     (search-forward "jkl")
                     (match-beginning 0))
                   (mistty--find-scrolline 10)))
    (should (equal (save-excursion
                     (search-forward "stu")
                     (match-beginning 0))
                   (mistty--find-scrolline 11)))
    (should (equal (point-max) (mistty--find-scrolline 12)))
    (should (equal nil (mistty--find-scrolline 13)))
    (should (equal nil (mistty--find-scrolline 100)))))

(ert-deftest mistty-scrolline-for-each ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (let ((capture (list)))
      (mistty--for-each-scrolline
       (lambda (beg end)
         (push (buffer-substring-no-properties beg end) capture)))
      (should (equal (list "abc\ndef\nghi\n"
                           "\n"
                           "jkl\nmno\npqr\n"
                           "stu\nvwx\nyz\n")
                     (nreverse capture))))))

(ert-deftest mistty-scrolline-for-each-no-final-nl ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr")
    (goto-char (point-min))

    (let ((capture (list)))
      (mistty--for-each-scrolline
       (lambda (beg end)
         (push (buffer-substring-no-properties beg end) capture)))
      (should (equal (list "abc\ndef\nghi\n"
                           "jkl\nmno\npqr")
                     (nreverse capture))))))

(ert-deftest mistty-scrolline-for-each-partial ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (let ((capture (list)))
      (mistty--for-each-scrolline
       (lambda (beg end)
         (push (buffer-substring-no-properties beg end) capture))
       (save-excursion
         (search-forward "e")
         (match-beginning 0))
       (save-excursion
         (search-forward "w")
         (match-beginning 0)))
      (should (equal (list "ef\nghi\n"
                           "jkl\nmno\npqr\n"
                           "stu\nv")
                     (nreverse capture))))))

(ert-deftest mistty-unwrapped-scrolline-text ()
  (ert-with-test-buffer ()
    (insert "abc" mistty-test-fakenl "def" mistty-test-fakenl "ghi\n")
    (insert "jkl" mistty-test-fakenl "mno" mistty-test-fakenl "pqr\n")
    (insert "stu" mistty-test-fakenl "vwx" mistty-test-fakenl "yz\n")
    (goto-char (point-min))

    (search-forward "n")
    (should
     (equal "jklmnopqr" (mistty--unwrapped-scrolline-text)))
    (should
     (equal "jklmn" (mistty--unwrapped-scrolline-text-to-point)))))

;;; mistty-scrolline-test.el ends here
