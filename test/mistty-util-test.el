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
  (ert-with-test-buffer ()
    (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
    (insert fake-nl "abc" fake-nl fake-nl "def" fake-nl "ghi\n" fake-nl )

    (mistty--remove-text-with-property 'term-line-wrap)
    (should (equal "abcdefghi\n"
                   (mistty--safe-bufstring (point-min) (point-max)))))))

(ert-deftest mistty-util-test-cleanup-fake-nl-for-scrollback ()
  (ert-with-test-buffer ()
    (let ((fake-nl (propertize "\n" 'term-line-wrap t)))
      (insert fake-nl "abc" fake-nl fake-nl "def" fake-nl "gh" fake-nl "i\n" fake-nl )

      (mistty--cleanup-scrollback
       (mistty-test-pos "abc") (mistty-test-pos "gh"))
      (should (equal (concat fake-nl "abcdefgh" fake-nl "i\n" fake-nl)
                     (buffer-string))))))

(ert-deftest mistty-util-test-cleanup-trailing-spaces-for-scrollback ()
  (ert-with-test-buffer ()
    (insert "ignore before start" (propertize "    " 'mistty-skip 'trailing) "\n")
    (insert "hello " (propertize "     " 'mistty-skip 'trailing) "\n")
    (insert "world" (propertize "     " 'mistty-skip 'trailing) "\n")
    ;; The line below is incorrect on purpose. It makes sure that only trailing
    ;; spaces are deleted.
    (insert (propertize " !     " 'mistty-skip 'trailing) "\n")
    ;; The line below makes sure that unmarked spaces are left alone.
    (insert "  \n")
    (insert "ignore after end" (propertize "    " 'mistty-skip 'trailing) "\n")

    (mistty--cleanup-scrollback
     (mistty-test-pos "hello") (mistty-test-pos "ignore after end"))

    (should
     (equal
      (concat "ignore before start    \n"
              "hello \n"
              "world\n"
              " !\n"
              "  \n"
              "ignore after end    \n")
      (buffer-substring-no-properties (point-min) (point-max))))))

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
                     (mistty--column-count))))))

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

(ert-deftest mistty-test-fifo-one-element ()
  (let ((fifo (mistty--make-fifo)))
    (mistty--fifo-enqueue fifo 1)

    (should (equal 1 (mistty--fifo-dequeue fifo)))
    (should (equal nil (mistty--fifo-dequeue fifo)))))

(ert-deftest mistty-test-fifo-multiple-elements ()
  (let ((fifo (mistty--make-fifo)))
    (mistty--fifo-enqueue fifo 1)
    (mistty--fifo-enqueue fifo 2)
    (mistty--fifo-enqueue fifo 3)

    (should (equal 1 (mistty--fifo-dequeue fifo)))
    (should (equal 2 (mistty--fifo-dequeue fifo)))
    (should (equal 3 (mistty--fifo-dequeue fifo)))
    (should (equal nil (mistty--fifo-dequeue fifo)))))

(ert-deftest mistty-test-fifo-empty ()
  (let ((fifo (mistty--make-fifo)))
    (should (mistty--fifo-empty-p fifo))
    (mistty--fifo-enqueue fifo 1)
    (should-not (mistty--fifo-empty-p fifo))
    (mistty--fifo-dequeue fifo)
    (should (mistty--fifo-empty-p fifo))))

(ert-deftest mistty-test-fifo-clear ()
  (let ((fifo (mistty--make-fifo)))
    (mistty--fifo-enqueue fifo 1)
    (mistty--fifo-enqueue fifo 2)
    (mistty--fifo-clear fifo)
    (should-not (mistty--fifo-dequeue fifo))
    (should (mistty--fifo-empty-p fifo))))

(ert-deftest mistty-test-fifo-to-list ()
  (let ((fifo (mistty--make-fifo)))
    (mistty--fifo-enqueue fifo 1)
    (mistty--fifo-enqueue fifo 2)
    (mistty--fifo-enqueue fifo 3)
    (should (equal '(1 2 3) (mistty--fifo-to-list fifo)))
    (should (mistty--fifo-empty-p fifo))))

(ert-deftest mistty-test-blank-end-start ()
  (ert-with-test-buffer ()
    (insert "foo\n"
            "bar " (propertize "   " 'mistty-skip 'trailing-whitespace)
            "\n\n\n")

    (goto-char (point-min))
    (should (equal (search-forward "bar ")
                   (mistty--blank-end-start)))))

(ert-deftest mistty-test-blank-end-start-empty ()
  (ert-with-test-buffer ()
    (should (equal (point-min) (mistty--blank-end-start)))

    (insert (propertize "   " 'mistty-skip 'trailing-whitespace)
            "\n\n\n")

    (should (equal (point-min) (mistty--blank-end-start)))))
