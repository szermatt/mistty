;;; Tests mistty-term.el -*- lexical-binding: t -*-

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
