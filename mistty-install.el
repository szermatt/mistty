;;; mistty-install.el --- Download and install MisTTY module -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; This file provides helper for downloading or building and
;; installing the appropriate module.

(require 'mistty-alacritty)
(require 'cl-lib)

(defvar mistty-install-src-dir (file-name-directory load-file-name)
  "Directory where mistty is installed.

This is the directory where the .el or .elc files are installed.")

(defvar mistty-install-url
  "https://github.com/szermatt/mistty/releases/download/%r/mistty-alacritty-vt-%v-%a%e"
  "URL to download the module from.

%r is the release name from `mistty-alacritty-release'
%v is the version name from `mistty-alacritty-version'
%a is the machine architecture `mistty-alacritty-arch'
%e is `module-file-suffix'")

(defvar mistty-source-url
  "https://github.com/szermatt/mistty/archive/refs/tags/%v.tar.gz"
  "URL to download the module from.

%r is the release name from `mistty-alacritty-release'
%v is the version name from `mistty-alacritty-version'
%a is the machine architecture `mistty-alacritty-arch'
%e is `module-file-suffix'")

(defvar mistty-install-available
  '((gnu/linux . "x86_64")
    (darwin . "x86_64")
    (darwin . "aarch64")
    (windows-nt . "x86_64"))
  "Systems and architectures for there may exist a pre-built module.")

(defun mistty--download-module-issues ()
  "Check whether it's worth trying to download the module.

Return either:
 - nil if there are no issues
 - \\='development-version
 - \\='unsupported-system
 - \\='curl-not-installed"
  (cond
   ((not (member (cons system-type mistty-alacritty-arch)
                 mistty-install-available))
    'unsupported-system)
   ((null (executable-find "curl"))
    'curl-not-installed)
   ((or (null mistty-alacritty-version)
        (equal "dev" mistty-alacritty-version))
    'development-version)
   (t nil)))

(defun mistty--download-module (install-dir output-buffer)
  "Download the correct version of the module and store it into INSTALL-DIR.

This download the module from `mistty-install-url' using curl and puts
the result into `mistty-install-dir'.

OUTPUT-BUFFER should be a buffer where the actions and shell output are
to be directed.

If download succeeded, return the module path, otherwise return nil."
  (let* ((dest (expand-file-name
                (mistty-alacritty-modulename)
                install-dir))
         (url (format-spec mistty-install-url (mistty--install-url-spec)))
         (cmd (format "curl --no-progress-meter --fail-with-body -o %s %s"
                      (shell-quote-argument dest)
                      (shell-quote-argument url))))
    (with-current-buffer output-buffer
      (mistty--install-message
       'progress "Downloading module version " mistty-alacritty-version "...")
      (if (and (zerop (mistty--install-execute cmd)) (file-exists-p dest))
          dest
        (mistty--install-message 'error "downloading failed")
        nil))))

(defun mistty--download-source-issues ()
  "Check whether it's worth trying to download the source.

Return either:
 - nil if there are no issues
 - \\='development-version
 - \\='curl-not-installed"
  (cond
   ((null (executable-find "curl"))
    'curl-not-installed)
   ((or (null mistty-alacritty-version)
        (equal "dev" mistty-alacritty-version))
    'development-version)
   (t nil)))

(defun mistty--download-source (src-dir output-buffer)
  "Download the correct version of the module and store it into SRC-DIR.

This download the module from `mistty-install-url' using curl and puts
the result into `mistty-src-dir'.

OUTPUT-BUFFER should be a buffer where the actions and shell output are
to be directed.

Return non-nil if the download succeeded."
  (with-current-buffer output-buffer
    (let* ((url (format-spec mistty-source-url (mistty--install-url-spec)))
           (cmd (format
                 "curl --no-progress-meter --fail-with-body %s | tar xzf - "
                 (shell-quote-argument url)))
           (default-directory src-dir))
      (mistty--install-message
       'progress  "Downloading source version " mistty-alacritty-version "...")
      (if (and (zerop (mistty--install-execute cmd))
               (file-exists-p "Cargo.toml"))
          t
        (mistty--install-message 'error "downloading failed")
        nil))))

(defun mistty--compile-module-issues (src-dir)
  "Check whether it's worth trying to compile the module in SRC-DIR.

Return either:
 - nil if there are no issues
 - \\='cargo-not-installed
 - \\='no-sources"
  (cond
   ((null (executable-find "curl"))
    'cargo-not-installed)
   ((not (and (file-exists-p (expand-file-name "Cargo.toml" src-dir))
              (file-exists-p (expand-file-name "src/lib.rs" src-dir))))
    'no-sources)
   (t nil)))

(cl-defun mistty--compile-module (src-dir target-dir install-dir output-buffer)
  "Compile the module in SRC-DIR.

TARGET-DIR is used as the target directory. It can be relative to
SRC-DIR. It doesn't need to be inside SRC-DIR, but it must be writable.
It can be a temporary directory made with `make-temp-file' if the source
directory is read-only.

The module is installed into INSTALL-DIR.

OUTPUT-BUFFER should be a buffer where the actions and shell output are
to be directed.

If compile succeeded, return the path to the module that was built, otherwise
return nil."
  (with-current-buffer output-buffer
    (let* ((cmd (format "cargo build --release --target-dir %s"
                        (shell-quote-argument target-dir)))
           (dest (expand-file-name
                  (mistty-alacritty-modulename) install-dir))
           (default-directory src-dir)
           (build-target (expand-file-name
                          (concat (unless (eq 'windows-nt system-type) "lib")
                                  "mistty_alacritty_vt"
                                  module-file-suffix)
                          (expand-file-name "release/" target-dir))))
      (mistty--install-message 'progress "Compiling module...")
      (if (zerop (mistty--install-execute cmd))
          (if (condition-case _err
                  (prog1 t
                    (rename-file build-target dest 'ok-if-already-exists))
                (error nil))
              dest
            (mistty--install-message
             'error "failed to copy module to " install-dir " from " build-target)
            nil)
        (mistty--install-message
         'error "compilation failed")
        nil))))

(defun mistty--install-url-spec ()
  "Return a spec to use for `format-spec' for formatting URLs."
  `((?r . ,(or mistty-alacritty-release mistty-alacritty-version))
    (?v . ,mistty-alacritty-version)
    (?a . ,mistty-alacritty-arch)
    (?e . ,module-file-suffix)))

(defun mistty--install-message (type &rest parts)
  "Output a message of type TYPE.

The PARTS are concatenated together before displaying."
  (let ((msg (apply #'concat parts)))
    (insert (pcase type
              ('error "ERROR:")
              ('success "OK ")
              (_ ""))
            msg
            "\n")))

(defun mistty--install-execute (shell-cmd)
  "Execute SHELL-CMD and return its status.

The command and its output are appended to the current buffer."
  (goto-char (point-max))
  (insert "> " shell-cmd "\n\n")
  (let* ((proc (make-process
                :name "*mistty-install*"
                :buffer (current-buffer)
                :command (list "/bin/sh" "-c" shell-cmd)
                :sentinel (lambda (proc _msg)
                            (unless (eq 'run (process-status proc))
                              (exit-recursive-edit))))))
    (recursive-edit)
    (goto-char (point-max))
    (insert "\n")

    (process-exit-status proc)))

(provide 'mistty-install)
