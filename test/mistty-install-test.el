;;; Tests the module mistty-install -*- lexical-binding: t -*-

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
(require 'mistty-alacritty)
(require 'mistty-install)
(require 'mistty-testing)
(require 'web-server)

(ert-deftest mistty-install-download-module-issues ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (system-type 'darwin)
        (mistty-alacritty-arch "aarch64"))
    (should (eq nil (mistty--download-module-issues)))

    (let ((mistty-alacritty-version "dev"))
      (should (eq 'development-version (mistty--download-module-issues))))
    (let ((mistty-alacritty-arch "m68000"))
      (should (eq 'unsupported-system (mistty--download-module-issues))))
    (let ((exec-path nil))
      (should (eq 'curl-not-installed (mistty--download-module-issues))))))

(ert-deftest mistty-install-download-source-issues ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (system-type 'darwin)
        (mistty-alacritty-arch "aarch64"))
    (should (eq nil (mistty--download-source-issues)))

    (let ((mistty-alacritty-version "dev"))
      (should (eq 'development-version (mistty--download-module-issues))))
    (let ((exec-path nil))
      (should (eq 'curl-not-installed (mistty--download-module-issues))))))

(ert-deftest mistty-install-compile-module-issues ()
  (skip-unless (executable-find "cargo"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (src-dir mistty-install-src-dir))
    (should (eq nil (mistty--compile-module-issues src-dir)))

    (should (eq 'no-sources (mistty--compile-module-issues "/var/tmp")))
    (let ((exec-path nil))
      (should (eq 'cargo-not-installed
                  (mistty--compile-module-issues src-dir))))))

(ert-deftest mistty-install-compile-module ()
  :tags '(:slow)
  (skip-unless (executable-find "cargo"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (src-dir mistty-install-src-dir))
    (ert-with-temp-directory tempdir
      (let ((install-dir (expand-file-name "install" tempdir))
            (target-dir (expand-file-name "target" tempdir)))
        (make-directory install-dir)
        (ert-with-test-buffer ()
          (let ((dest (expand-file-name (mistty-alacritty-modulename) install-dir))
                result output)
            (setq result (mistty--compile-module
                            src-dir target-dir install-dir (current-buffer)))
            (setq output (mistty-test-content))
            (when (null result)
              (message "OUT<<EOF\n%sEOF" (mistty-test-content)))

            (should (equal dest result))
            (should (file-exists-p dest))

            (should (string-match-p "Compiling module\.\.\.$" output))
            (should (string-match-p "Finished" output))))))))

(ert-deftest mistty-install-compile-module-failed ()
  (skip-unless (executable-find "cargo"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (src-dir mistty-install-src-dir))
    (ert-with-temp-directory install-dir
      (let ((target-dir null-device))
        (ert-with-test-buffer ()
          (let ((dest (expand-file-name (mistty-alacritty-modulename) install-dir))
                result output)
            (setq result (mistty--compile-module
                            src-dir target-dir install-dir (current-buffer)))
            (setq output (mistty-test-content))
            (should (null result))))))))

(defun mistty-run-test-server (handler-func test-func)
  "Run a test web server for the duration of the test.

This function runs a web server with HANDLER-FUNC as handler, then calls
TEST-FUNC, passing it the address of the web server and kills the server
once that function returns."
  (let ((server (ws-start handler-func t)))
    (unwind-protect
        (funcall test-func (format "127.0.0.1:%s" (process-contact (process server) :service)))
      (ws-stop server))))

(ert-deftest mistty-install-download-module ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (mistty-alacritty-release "v2.0.0")
        (system-type 'darwin)
        (mistty-alacritty-arch "aarch64")
        requested-url)
    (ert-with-temp-directory install-dir
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (setq requested-url (cdr (assoc :GET headers)))
           (ws-response-header process 200
                               '("Content-Type" . "application/octet-stream"))
           (process-send-string process "dummy-module-binary-content")))
       (lambda (address)
         (let* ((mistty-install-url (concat "http://" address "/download/%r/mistty-alacritty-vt-%v-%a%e"))
                (dest (expand-file-name (mistty-alacritty-modulename) install-dir))
                (expected-path (format "/download/v2.0.0/mistty-alacritty-vt-2.0.0-aarch64.dylib")))
           (ert-with-test-buffer ()
             (let ((result (mistty--download-module install-dir (current-buffer)))
                   (output (mistty-test-content)))
               (unless result
                 (message "OUT<<EOF\n%sEOF" (mistty-test-content)))

               (should (equal dest result))
               (should (file-exists-p dest))
               (should (equal "dummy-module-binary-content"
                              (with-temp-buffer
                                (insert-file-contents-literally dest)
                                (buffer-string))))
               (should (equal expected-path requested-url))
               (should (string-match-p "Downloading module version 2\.0\.0\.\.\." output))
               (should-not (string-match-p "ERROR:" output))))))))))

(ert-deftest mistty-install-download-module-server-error ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (system-type 'darwin)
        (mistty-alacritty-arch "aarch64"))
    (ert-with-temp-directory install-dir
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (ws-send-404 process "Not Found")))
       (lambda (address)
         (let* ((mistty-install-url (concat "http://" address "/mistty-alacritty-vt-%v-%a%e")))
           (ert-with-test-buffer ()
             (let ((result (mistty--download-module install-dir (current-buffer)))
                   (output (mistty-test-content)))
               (unless (null result)
                 (message "OUT<<EOF\n%sEOF" (mistty-test-content)))

               (should (null result))
               (should (string-match-p "ERROR:" output))))))))))

(ert-deftest mistty-install-download-source ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0")
        (mistty-alacritty-release "v2.0.0")
        (system-type 'darwin)
        (mistty-alacritty-arch "aarch64")
        (data (let ((default-directory mistty-install-src-dir))
                (shell-command-to-string "tar czf - Cargo.toml src/lib.rs")))
        requested-url)
    (ert-with-temp-directory dest-dir
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (setq requested-url (cdr (assoc :GET headers)))
           (ws-response-header process 200
                               '("Content-Type" . "application/octet-stream"))
           (process-send-string process data)))
       (lambda (address)
         (let* ((mistty-source-url (concat "http://" address "/archive/refs/tags/%v.tar.gz")))
           (ert-with-test-buffer ()
             (let ((result (mistty--download-source dest-dir (current-buffer)))
                   (output (mistty-test-content)))
               (unless result
                 (message "OUT<<EOF\n%sEOF" (mistty-test-content)))
               (should (file-exists-p (expand-file-name "Cargo.toml" dest-dir)))
               (should (file-exists-p (expand-file-name "src/lib.rs" dest-dir)))
               (should (equal nil (mistty--compile-module-issues dest-dir)))))))))))

(ert-deftest mistty-install-download-source-failed ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (let ((mistty-alacritty-version "2.0.0"))
    (ert-with-temp-directory dest-dir
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (ws-send-404 process "Not Found")))
       (lambda (address)
         (let* ((mistty-source-url (concat "http://" address "/archive/refs/tags/%v.tar.gz")))
           (ert-with-test-buffer ()
             (should-not (mistty--download-source dest-dir (current-buffer))))))))))
