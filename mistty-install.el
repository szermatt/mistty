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
(require 'url)

(defvar mistty-install-src-dir (file-name-directory load-file-name)
  "Directory where mistty is installed.

This is the directory where the .el or .elc files are installed.")

(defcustom mistty-install-dir (if (file-writable-p mistty-install-src-dir)
                                  mistty-install-src-dir
                                user-emacs-directory)
  "Directory where the module should be installed.

`mistty-install' will attempt to store the module into this directory.
It should be a writable directory that's on `load-path'.

It defaults to the directory where `mistty-install.el' is stored, if it
is writable, otherwise `user-emacs-directory'."
  :group 'mistty
  :type 'directory)

(defcustom mistty-install-keep-output nil
  "Keep around the *mistty-install* output buffer.

When this option is nil, which is the default, the buffer that displays
installation details and progress created by `mistty-install' and
`mistty-install-dwim' is displayed only after a delay and killed at the
end of a successful command.

With this option set, the buffer is displayed right away and kept around
for the user to kill."
  :group 'mistty
  :type 'boolean)

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

(defconst mistty-download-rust-url "https://rust-lang.org/tools/install/")

(defun mistty-install ()
  "Let user choose a way of installing the Mistty Alacritty module.

This command lets the user know what possibly ways there are to install
the module and highlight issues and choose what to do. It is meant to
be called interactively."
  (interactive)
  (pcase-let ((`(,options . ,option-alist) (mistty--install-setup)))
    (if (length= options 1)
        (funcall (alist-get 'handler
                            (alist-get (car options) option-alist)))
      (when-let* ((titlemap (let ((table (make-hash-table :test #'equal)))
                              (dolist (e option-alist)
                                (puthash (alist-get 'title (cdr e))
                                         (car e)
                                         table))
                              table))
                  (choice (completing-read
                           "Choose a way to install the MisTTY Alacritty module. "
                           (completion-table-with-metadata
                            (mapcar (lambda (id)
                                      (alist-get 'title (alist-get id option-alist)))
                                    options)
                            `((annotation-function
                               . ,(lambda (title)
                                    (concat "  "
                                            (alist-get
                                             'doc (alist-get
                                                   (gethash title titlemap)
                                                   option-alist)))))
                              (display-sort-function
                               . ,(lambda (collection) collection))))
                           nil 'require-match))
                  (id (gethash choice titlemap))
                  (option-def (alist-get id option-alist))
                  (handler (alist-get 'handler option-def)))
        (funcall handler)))))

(defun mistty--install-setup ()
  "Build the set of options available for `mistty-install'.

The return value is a CONS containing:
- the ID of the enabled options in the alist
- the option alist, defining the title, doc string and handler for the
  option."
  (let* ((option-alist
          `((download
             . ((title . "Download")
                (doc . ,(format
                         "Download released module from %s"
                         (url-host (url-generic-parse-url
                                    mistty-install-url))))
                (handler . ,#'mistty--interactive-download)))
            (download-source
             . ((title . "Download source")
                (doc . ,(format
                         "Download released module source from %s and compile them"
                         (url-host (url-generic-parse-url
                                    mistty-source-url))))
                (handler . ,#'mistty--interactive-download-source)))
            (compile
             . ((title . "Compile")
                (doc . "Compile module from source")
                (handler . ,#'mistty--interactive-compile)))
            (rust
             . ((title . "Install Rust/cargo")
                (doc . ,(format "Install Rust from %s"
                                (url-host (url-generic-parse-url
                                           mistty-download-rust-url))))
                (handler . ,#'mistty--interactive-rust)))))
         (download-issue (mistty--download-module-issues))
         (download-source-issue (mistty--download-source-issues))
         (compile-issue (mistty--compile-module-issues mistty-install-src-dir))
         (options (list)))

    ;; Define the set of visible options in order.
    ;;
    ;; The option most likely to be useful must appear first for
    ;; mistty-install-dwim.
    (unless (eq 'development-version download-issue)
      (push 'download options))
    (when (eq 'cargo-not-installed compile-issue)
      (push 'rust options))
    (unless (eq 'development-version download-source-issue)
      (push 'download-source options))
    (push 'compile options)
    (setq options (nreverse options))

    ;; Report issues in the relevant option's doc
    (when (eq 'unsupported-system download-issue)
      (dolist (e 'download 'download-source)
        (setf (alist-get 'doc (alist-get e option-alist))
              "System or architecture not supported")))
    (when (eq 'curl-not-installed download-issue)
      (dolist (e 'download 'download-source)
        (setf (alist-get 'doc (alist-get e option-alist))
              "curl must be on the $PATH")))
    (when (eq 'cargo-not-installed compile-issue)
      (dolist (e 'download-source 'compile)
        (setf (alist-get 'doc (alist-get e option-alist))
              "cargo must be on the $PATH; Install Rust first")))

    (cons options option-alist)))

(defun mistty--run-with-output-buffer (func)
  "Setup an output buffer and run FUNC.

The buffer is shown if enough time passes or if FUNC fails. Once FUNC
succeeds, the buffer is deleted."
  (let* ((buf (get-buffer-create "*mistty-install*")))
    (if mistty-install-keep-output
        (progn
          (mistty--setup-output-buffer buf)
          (pop-to-buffer buf)
          (funcall func buf))
      (let* ((buffer-revealed nil)
             (reveal-buffer (lambda ()
                              (when (and (buffer-live-p buf)
                                         (not buffer-revealed))
                                (pop-to-buffer buf)
                                (setq buffer-revealed t))))
             (timer (run-with-timer 0.75 nil reveal-buffer)))
        (unwind-protect
            (progn
              (mistty--setup-output-buffer buf)
              (funcall func buf)
              (cancel-timer timer)
              (let ((kill-buffer-query-functions nil))
                (kill-buffer buf)))
          (when (buffer-live-p buf)
            (cancel-timer timer)
            (funcall reveal-buffer)))))))

(defun mistty--setup-output-buffer (buf)
  "Prepare a newly-created or reused output BUF."
  (with-current-buffer buf
    (delete-region (point-min) (point-max))))

(defun mistty--run-with-temp-dir (name func)
  "Pass a temporary dir NAME to FUNC, then delete it."
  (let ((dir (make-temp-file (concat "mistty-" name) 'dir)))
    (unwind-protect
        (funcall func dir)
      (delete-directory dir t nil))))

(defun mistty--interactive-download ()
  "Download the module (interactive version)."
  (mistty--run-with-output-buffer
   (lambda (output-buffer)
     (unless
         (mistty--download-module
          mistty-install-dir output-buffer)
       (error "Module download failed."))
     (mistty--interactive-check-installed
      output-buffer))))

(defun mistty--interactive-download-source ()
  "Download the module source (interactive version)."
  (mistty--run-with-output-buffer
   (lambda (output-buffer)
     (mistty--run-with-temp-dir
      "src"
      (lambda (src-dir)
        (unless (mistty--download-source src-dir output-buffer)
          (error "Source download failed"))
        (unless (mistty--compile-module
                 src-dir "target" mistty-install-dir output-buffer)
          (error "Module compilation failed"))
        (mistty--interactive-check-installed
         output-buffer))))))

(defun mistty--interactive-compile ()
  "Compile the module source (interactive version)."
  (mistty--run-with-output-buffer
   (lambda (output-buffer)
     (if (eq 'no-source (mistty--compile-module-issues
                         mistty-install-dir))
         ;; No source in mistty-install-dir
         (when-let* ((src-dir (read-directory-name "Source dir ")))
           (unless (mistty--compile-module
                    src-dir "target" mistty-install-dir output-buffer)
             (error "Module compilation failed"))
           (mistty--interactive-check-installed
            output-buffer))

       ;; Rust source found in mistty-install-dir
       (mistty--run-with-temp-dir
        "target"
        (lambda (target-dir)
          (unless (mistty--compile-module
                   mistty-install-src-dir
                   target-dir
                   mistty-install-dir
                   output-buffer)
            (error "Module compilation failed"))
          (mistty--interactive-check-installed
           output-buffer)))))))

(defun mistty--interactive-rust ()
  "Direct the user to rust's install instructions."
  (message "Follow the instructions on %s to install Rust."
           mistty-download-rust-url)
  (browse-url mistty-download-rust-url))

(defun mistty--interactive-check-installed (output-buffer)
  "Report a successful install and try to load the module.

Signals an error if loading fails."
  (with-current-buffer output-buffer
    (mistty--install-message
     'success "Module installed into " mistty-install-dir)
    (cond
     ((mistty-alacritty-available-p)
      (mistty--install-message
       'success "Module installed. Restart Emacs to reload module"))
     ((progn
        (mistty-alacritty-load)
        (mistty-alacritty-available-p))
      (mistty--install-message
       'success "Module installed and loaded successfully"))
     (t
      (error "Installed module could not be loaded. Is %s on the load-path ? "
             mistty-install-dir)))))

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
  (with-current-buffer output-buffer
    (let* ((default-directory install-dir)
           (dest (mistty-alacritty-modulename))
           (url (format-spec mistty-install-url (mistty--install-url-spec)))
           (cmd (format-spec
                 "curl --no-progress-meter --fail-with-body -o %d.part %u && mv %d.part %d"
                 `((?u . ,(shell-quote-argument url))
                   (?d . ,(shell-quote-argument dest))))))
      (mistty--install-message
       'progress "Downloading module version " mistty-alacritty-version "...")
      (if (and (zerop (mistty--install-execute cmd)) (file-exists-p dest))
          (expand-file-name dest install-dir)
        (mistty--install-message 'error "download failed")
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
    (message msg)
    (goto-char (point-max))
    (insert (if (eq 'error type)
                "ERROR: "
              "")
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
                            (unless (process-live-p proc)
                              (exit-recursive-edit))))))
    (save-excursion
      (while (process-live-p proc)
        (recursive-edit)))
    (message "COMPILATION DONE current-buffer: %s" (current-buffer))
    (goto-char (point-max))
    (insert "\n")
    (process-exit-status proc)))

(provide 'mistty-install)
