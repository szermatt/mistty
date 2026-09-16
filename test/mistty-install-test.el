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
  (ert-with-temp-directory tempdir
    (let ((mistty-alacritty-version "2.0.0")
          (mistty-install-keep-output t)
          (src-dir mistty-install-src-dir)
          (mistty-install-dir tempdir))
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (condition-case err
          (mistty--interactive-compile)
        (error (message "OUT<<EOF\n%sEOF"
                        (with-current-buffer mistty-install-buffer
                          (mistty-test-content)))
               (signal err)))
      (setq output (with-current-buffer mistty-install-buffer
                     (mistty-test-content)))
      (should (file-exists-p
               (expand-file-name (mistty-alacritty-modulename)
                                 mistty-install-dir)))
      (should (string-match "Compiling module\.\.\.$" output))
      (should (string-match "Finished" output)))))

(ert-deftest mistty-install-compile-module-no-source ()
  :tags '(:slow)
  (skip-unless (executable-find "cargo"))
  (mistty-test-running)
  (ert-with-temp-directory tempdir
    (let ((mistty-alacritty-version "2.0.0")
          (mistty-install-dir (expand-file-name "install" tempdir))
          (mistty-install-src-dir (expand-file-name "src" tempdir)))
      (make-directory mistty-install-dir)
      (make-directory mistty-install-src-dir)
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (should-error (mistty--interactive-compile))
      (setq output (with-current-buffer mistty-install-buffer
                     (mistty-test-content)))
      (should (string-match "compilation failed" output))
      (should-not (file-exists-p
                   (expand-file-name (mistty-alacritty-modulename)
                                     mistty-install-dir))))))

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
  (ert-with-temp-directory install-dir
    (let ((mistty-alacritty-version "2.0.0")
          (mistty-alacritty-release "v2.0.0")
          (mistty-install-keep-output t)
          (system-type 'darwin)
          (mistty-alacritty-arch "aarch64")
          (mistty-install-dir install-dir))
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (let ((requested-url (cdr (assoc :GET headers))))
           (cond
            ((string= requested-url
                      (concat "/download/v2.0.0/mistty-alacritty-vt-2.0.0-aarch64"
                              module-file-suffix))
             ;; We use a redirect, like github does, to make sure this
             ;; works.
             (ws-response-header process 301 '("Location" . "/module"))
             (process-send-string process ""))
            ((string= requested-url "/module")
             (ws-response-header process 200
                                 '("Content-Type" . "application/octet-stream"))
             (process-send-string process "dummy-module-binary-content"))
            (t
             (ws-send-404 process "Not Found"))))))
       (lambda (address)
         (let* ((mistty-install-url (concat "http://" address "/download/%r/mistty-alacritty-vt-%v-%a%e"))
                (dest (expand-file-name (mistty-alacritty-modulename) install-dir)))
           (condition-case err
               (mistty--interactive-download)
             (error (message "OUT<<EOF\n%sEOF"
                             (with-current-buffer mistty-install-buffer
                               (mistty-test-content)))
                    (signal err)))
           (let ((output (with-current-buffer mistty-install-buffer
                           (mistty-test-content))))
             (should (file-exists-p dest))
             (should (equal "dummy-module-binary-content"
                            (with-temp-buffer
                              (insert-file-contents-literally dest)
                              (buffer-string))))
             (should (string-match "Downloading module version 2\.0\.0\.\.\." output))
             (should-not (string-match "ERROR" output)))))))))

(ert-deftest mistty-install-download-module-fail ()
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (ert-with-temp-directory install-dir
    (let ((mistty-alacritty-version "2.0.0")
          (mistty-alacritty-release "v2.0.0")
          (system-type 'darwin)
          (mistty-alacritty-arch "aarch64")
          (mistty-install-dir install-dir)
          requested-url)
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (ws-send-404 process "Not Found")))
       (lambda (address)
         (let* ((mistty-install-url (concat "http://" address "/download/%r/mistty-alacritty-vt-%v-%a%e"))
                (dest (expand-file-name (mistty-alacritty-modulename) install-dir)))
           (should-error (mistty--interactive-download))
           (let ((output (with-current-buffer mistty-install-buffer
                           (mistty-test-content))))
             (should-not (file-exists-p dest))
             (should (string-match "Downloading module version 2\.0\.0\.\.\." output))
             (should (string-match "ERROR" output)))))))))

(ert-deftest mistty-install-download-source ()
  :tags '(:slow)
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (ert-with-temp-directory dest-dir
    (let ((mistty-alacritty-version "2.0.0")
          (mistty-alacritty-release "v2.0.0")
          (mistty-install-dir dest-dir)
          (mistty-install-keep-output t)
          (system-type 'darwin)
          (mistty-alacritty-arch "aarch64")
          (data (let ((default-directory mistty-install-src-dir))
                  (shell-command-to-string "tar czf - Cargo.* src/*.rs"))))
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (let ((requested-url (cdr (assoc :GET headers))))
             (cond
              ((string= requested-url "/archive/refs/tags/2.0.0.tar.gz")
               (ws-response-header process 301 '("Location" . "/src"))
               (process-send-string process ""))
              ((string= requested-url "/src")
               (ws-response-header process 200
                                   '("Content-Type" . "application/octet-stream"))
               (process-send-string process data))
              (t
               (ws-send-404 process "Not Found"))))))
       (lambda (address)
         (let ((mistty-source-url (concat "http://" address "/archive/refs/tags/%v.tar.gz"))
               (dest (expand-file-name (mistty-alacritty-modulename)
                                       mistty-install-dir)))
           (condition-case err
               (mistty--interactive-download-source)
             (error (message "OUT<<EOF\n%sEOF"
                             (with-current-buffer mistty-install-buffer
                               (mistty-test-content)))
                    (signal err)))
           (should (file-exists-p dest))
           (let ((output (with-current-buffer mistty-install-buffer
                           (mistty-test-content))))
             (should (string-match "Compiling module\.\.\.$" output))
             (should (string-match "Finished" output)))))))))

(ert-deftest mistty-install-download-source-failed ()
  :tags '(:slow)
  (skip-unless (executable-find "curl"))
  (mistty-test-running)
  (ert-with-temp-directory dest-dir
    (let ((mistty-alacritty-version "2.0.0")
          (mistty-alacritty-release "v2.0.0")
          (mistty-install-dir dest-dir)
          (mistty-install-keep-output t)
          (system-type 'darwin)
          (mistty-alacritty-arch "aarch64"))
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (ws-send-404 process "Not Found")))
       (lambda (address)
         (let ((mistty-source-url (concat "http://" address "/archive/refs/tags/%v.tar.gz"))
               (dest (expand-file-name (mistty-alacritty-modulename)
                                       mistty-install-dir)))
           (should-error (mistty--interactive-download-source))
           (should-not (file-exists-p dest))))))))

(ert-deftest mistty-install-terminfo-from-local-file ()
  (skip-unless (>= emacs-major-version 31))
  (mistty-test-running)
  (ert-with-temp-directory tempdir
    (let ((mistty-install-keep-output t)
          (process-environment (cons (concat "HOME=" tempdir) process-environment)))
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (condition-case err
          (mistty--install-terminfo)
        (error (message "OUT<<EOF\n%sEOF"
                        (with-current-buffer mistty-install-buffer
                          (mistty-test-content)))
               (signal err)))
      (let ((default-directory tempdir))
        (should (file-exists-p ".terminfo/"))))))

(ert-deftest mistty-install-terminfo-from-remote-file ()
  (skip-unless (>= emacs-major-version 31))
  (mistty-test-running)
  (ert-with-temp-directory tempdir
    (let ((mistty-install-keep-output t)
          (process-environment (cons (concat "HOME=" tempdir) process-environment))
          (data (with-temp-buffer
                  (insert-file-contents (expand-file-name
                                         "extras/alacritty.info"
                                         mistty-install-src-dir))
                  (buffer-string))))
      (ignore-error error
        (kill-buffer mistty-install-buffer))
      (mistty-run-test-server
       (lambda (request)
         (with-slots (process headers) request
           (setq requested-url (cdr (assoc :GET headers)))
           (if (string= "/alacritty.info" requested-url)
               (progn
                 (ws-response-header process 200
                                     '("Content-Type" . "application/octet-stream"))
                 (process-send-string process data))
             (ws-send-404 process "Not Found"))))
       (lambda (address)
         (let ((mistty-install-terminfo-url (concat "http://" address "/alacritty.info"))
               (mistty-install-src-dir "/notfound"))
           (condition-case err
               (mistty--install-terminfo)
             (error (message "OUT<<EOF\n%sEOF"
                             (with-current-buffer mistty-install-buffer
                               (mistty-test-content)))
                    (signal err)))
      (let ((default-directory tempdir)
            (output (with-current-buffer mistty-install-buffer
                      (mistty-test-content))))
        (should (file-exists-p ".terminfo"))
        (should (string-match "curl" output))
        (should (string-match "tic -x" output)))))))))
