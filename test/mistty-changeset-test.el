;;; Tests mistty-changeset.el -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-x)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty)
(require 'mistty-testing)
(require 'mistty-changeset)

(ert-deftest mistty-changeset-test-collect-modifications-delete-after-replace ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 9)
    (goto-char 6)
    (insert "new-value")

    (delete-region 18 21)

    (should (equal "$ abcnew-valueghimno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))

    ;; The deletion is reported first, even though it was applied
    ;; last. If we did the reverse and a newline was inserted in the
    ;; middle of new-value, the deletion would not apply to the right
    ;; region.
    (should (equal '((12 "" 3) (6 "new-value" 3)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-collect-modifications-delete-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 (point-max))

    (should (equal "$ abc" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((6 "" -1)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-collect-modifications-insert-then-delete-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 (point-max))
    (goto-char 6)
    (insert "new-value")

    (should (equal "$ abcnew-value" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((6 "new-value" -1)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-collect-modifications-insert-skip-then-delete-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 15 (point-max))
    (delete-region 9 12)
    (goto-char 6)
    (insert "new-value")

    (should (equal "$ abcnew-valuedefjkl" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((15 "" -1) (9 "" 3) (6 "new-value" 0)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-collect-modifications-inserts ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)
    
    (goto-char 12)
    (insert "NEW")

    (goto-char 9)
    (insert "NEW")

    (goto-char 6)
    (insert "NEW")

    (should (equal "$ abcNEWdefNEWghiNEWjklmno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((12 "NEW" 0) (9 "NEW" 0) (6 "NEW" 0)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-collect-modifications-insert-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdef")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (goto-char 9)
    (insert "NEW")

    (should (equal "$ abcdefNEW" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((9 "NEW" 0)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-collect-modifications-replaces ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (goto-char 12)
    (delete-region 12 15)
    (insert "NEW")

    (goto-char 6)
    (delete-region 6 9)
    (insert "NEW")

    (should (equal "$ abcNEWghiNEWmno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))

    (should (equal '((12 "NEW" 3) (6 "NEW" 3)) (mistty--changeset-modifications (mistty--active-changeset))))))

(ert-deftest mistty-changeset-test-restrict-intervals ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 9)
    (goto-char 6)
    (insert "new-value")

    (delete-region 18 21)

    (should (equal "$ abcnew-valueghimno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    ;;             "$ abcdefg      hijklmno<<end>>"
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted)
                       (15 shift -6)
                       (18 shift -3)) (mistty--changeset-collect cs)))
      (should (equal -6 (mistty--changeset-restrict cs 16)))
      (should (equal '((16 shift 0) (18 shift 3)) (mistty--changeset-intervals cs)))
      (should (equal '((18 "" 3)) (mistty--changeset-modifications cs))))))

(ert-deftest mistty-changeset-test-restrict-intervals-before-changes ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcd<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (goto-char 6)
    (insert "new-value")

    (should (equal "$ abcnew-valued<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 shift -9)) (mistty--changeset-collect cs)))
      (should (equal 0 (mistty--changeset-restrict cs 4)))
      (should (equal '((6 inserted) (15 shift -9)) (mistty--changeset-intervals cs))))))

(ert-deftest mistty-changeset-test-restrict-intervals-exactly-before-insert ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcd<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 7)
    (goto-char 6)
    (insert "new-value")

    (should (equal "$ abcnew-value<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted)
                       (15 shift -8)) (mistty--changeset-collect cs)))
      (should (equal 0 (mistty--changeset-restrict cs 6)))
      (should (equal '((6 inserted) (15 shift -8)) (mistty--changeset-intervals cs))))))

(ert-deftest mistty-changeset-test-restrict-intervals-exactly-before-shift ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcd<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 7)

    (should (equal "$ abc<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 shift 1)) (mistty--changeset-collect cs)))
      (should (equal 0 (mistty--changeset-restrict cs 6)))
      (should (equal '((6 shift 1)) (mistty--changeset-intervals cs))))))

(ert-deftest mistty-changeset-test-restrict-intervals-starts-within-insert ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 9)
    (goto-char 6)
    (insert "new-value")

    (delete-region 18 21)

    (should (equal "$ abcnew-valueghimno<<end>>" (buffer-substring-no-properties (point-min) (point-max))))
    ;;             "$ abcdef ghijklmno<<end>>"
    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 shift -6) (18 shift -3)) (mistty--changeset-collect cs)))
      (should (equal -1 (mistty--changeset-restrict cs 10)))
      (should (equal '((10 inserted) (15 shift -5) (18 shift -2)) (mistty--changeset-intervals cs)))
      (should (equal '((13 "" 3) (10 "value" 0)) (mistty--changeset-modifications cs))))))

(ert-deftest mistty-changeset-test-restrict-intervals-starts-within-insert-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdef")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (goto-char 9)
    (insert "NEW")

    (should (equal "$ abcdefNEW" (buffer-substring-no-properties (point-min) (point-max))))
    ;;             "$ abcdef"
    (let ((cs (mistty--active-changeset)))
      (should (equal '((9 inserted)) (mistty--changeset-collect cs)))
      (should (equal nil (mistty--changeset-restrict cs 10))))))

(ert-deftest mistty-changeset-test-restrict-intervals-within-insert-then-delete-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 (point-max))
    (goto-char 6)
    (insert "new-value")

    (should (equal "$ abcnew-value" (buffer-substring-no-properties (point-min) (point-max))))

    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 deleted-to-end)) (mistty--changeset-collect cs)))
      (should (equal nil (mistty--changeset-restrict cs 10))))))

(ert-deftest mistty-changeset-test-restrict-intervals-within-delete-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcdefghijklmno<<end>>")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (delete-region 6 (point-max))
    (goto-char 6)
    (insert "new-value")

    (should (equal "$ abcnew-value" (buffer-substring-no-properties (point-min) (point-max))))

    (let ((cs (mistty--active-changeset)))
      (should (equal '((6 inserted) (15 deleted-to-end)) (mistty--changeset-collect cs)))
      (should (equal nil (mistty--changeset-restrict cs 15))))))

(ert-deftest mistty-changeset-test-single-insert ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abcd")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (goto-char 6)
    (insert "new")

    (should (equal "$ abcnewd" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal "new" (mistty--changeset-single-insert cs)))
      (should (eq 6 (mistty--changeset-beg cs))))))


(ert-deftest mistty-changeset-test-single-insert-at-end ()
  (ert-with-test-buffer ()
    (insert "$ ")
    (setq mistty-sync-marker (point))

    (insert "abc")
    (add-hook 'after-change-functions #'mistty--after-change-on-work nil t)

    (goto-char 6)
    (insert "at-end")

    (should (equal "$ abcat-end" (buffer-substring-no-properties (point-min) (point-max))))
    (let ((cs (mistty--active-changeset)))
      (should (equal "at-end" (mistty--changeset-single-insert cs)))
      (should (eq 6 (mistty--changeset-beg cs))))))

