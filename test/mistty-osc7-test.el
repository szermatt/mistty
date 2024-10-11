;;; Tests MisTTY's OSC7 support -*- lexical-binding: t -*-

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

(require 'mistty)
(require 'mistty-osc7)

(ert-deftest mistty-test-osc7-local-path ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7))))
      ;; Not using bash, because it sends \032 dirtrack, which would
      ;; interfere with this test.
      (mistty-test-send-osc7 (system-name) "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/var/log/" default-directory))

      (mistty-test-send-osc7 "localhost" "/home")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/home/" default-directory))

      (mistty-test-send-osc7 "" "/")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/" default-directory)))))

(ert-deftest mistty-test-osc7-utf8-path ()
  (mistty-with-test-buffer (:shell zsh)
    (ert-with-temp-directory tempdir
      (let ((utf8-dir (file-name-as-directory (expand-file-name "αβγδ" tempdir))))
        (make-directory utf8-dir)
        (mistty-test-send-osc7 (system-name) (concat tempdir (url-hexify-string "αβγδ")))
        (mistty-send-and-wait-for-prompt)
        (should (equal utf8-dir default-directory))))))

(ert-deftest mistty-test-osc7-remote-path ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7)))
          (mistty-allow-tramp-paths t)
          (tramp-default-method "ssh")
          (tramp-default-method-alist nil))
      (mistty-test-send-osc7 "testmachine.example" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/ssh:testmachine.example:/var/log/" default-directory)))))

(ert-deftest mistty-test-osc7-remote-path-disallowed ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7)))
          (mistty-allow-tramp-paths nil)
          (orig-default-directory default-directory))
      (mistty-test-send-osc7 "testmachine.example" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal orig-default-directory default-directory)))))

(ert-deftest mistty-test-osc7-remote-path-with-host-default ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7)))
          (mistty-allow-tramp-paths t)
          (tramp-default-method "ssh")
          (tramp-default-method-alist '(("testmachine.example" nil "scp"))))
      (mistty-test-send-osc7 "testmachine.example" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/scp:testmachine.example:/var/log/" default-directory)))))

(ert-deftest mistty-test-osc7-host-to-tramp-path ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7)))
          (mistty-allow-tramp-paths t)
          (tramp-default-method "ssh")
          (mistty-host-to-tramp-path-alist '(("testmachine" . "/scp:testmachine.example:"))))
      (mistty-test-send-osc7 "testmachine" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/scp:testmachine.example:/var/log/" default-directory)))))

(ert-deftest mistty-test-osc7-ignore-invalid-host-to-tramp-path ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7)))
          (mistty-allow-tramp-paths t)
          (tramp-default-method "ssh")
          (mistty-host-to-tramp-path-alist '(("testmachine" . "scp:testmachine.example/"))))
      (mistty-test-send-osc7 "testmachine" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal "/ssh:testmachine:/var/log/" default-directory)))))

(ert-deftest mistty-test-osc7-disable-host ()
  (mistty-with-test-buffer (:shell zsh)
    (let ((mistty-osc-handlers '(("7" . mistty-osc7)))
          (mistty-allow-tramp-paths t)
          (tramp-default-method "ssh")
          (mistty-host-to-tramp-path-alist '(("testmachine" . nil))))
      (mistty-test-send-osc7 "testmachine" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should-not (file-remote-p default-directory)))))

(ert-deftest mistty-test-osc7-keep-prefix-when-host-matches ()
  (let* ((sg-prefix (mistty-test-sg-prefix))
         (default-directory (concat sg-prefix "/")))
    (mistty-with-test-buffer (:shell zsh)

      (mistty-test-send-osc7 (system-name) "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal (concat sg-prefix "/var/log/") default-directory))

      (mistty-test-send-osc7 (system-name) "/")
      (mistty-send-and-wait-for-prompt)
      (should (equal (concat sg-prefix "/") default-directory)))))

(ert-deftest mistty-test-osc7-keep-prefix-when-localhost ()
  (let* ((sg-prefix (mistty-test-sg-prefix))
         (default-directory (concat sg-prefix "/")))
    (mistty-with-test-buffer (:shell zsh)

      (mistty-test-send-osc7 "" "/var/log")
      (mistty-send-and-wait-for-prompt)
      (should (equal (concat sg-prefix "/var/log/") default-directory))

      (mistty-test-send-osc7 "localhost" "/")
      (mistty-send-and-wait-for-prompt)
      (should (equal (concat sg-prefix "/") default-directory)))))

(defun mistty-test-send-osc7 (host path)
  (mistty--send-string
   mistty-proc
   (concat "printf '\\e]7;file://%s%s\\e\\\\\\\n' '" host "' '" path "'")))
