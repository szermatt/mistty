;;; mistty-osc7.el --- Add support for OSC 7 to term-mode  -*- lexical-binding: t -*-


;;; Commentary:
;;
;; This file defines a handler for OSC 7 code sequences that can be
;; registered in `mistty-osc-handlers' to allow shells to report
;; current directories to Emacs (dirtrack).
;;

(require 'url-util)

;;; Code:

(defvar mistty-allow-tramp-paths nil
  "If true, allow TRAMP paths as shell-specified directory.

Make sure tramps works before enabling.

This affects directories set using OSC 7, which can then build
TRAMP remote paths based on the hostname specified in the file://
URL, using the default method.")

(defun mistty-osc7 (_ osc-seq)
  "Store OSC-SEQ as path to the current directory.

OSC-SEQ must be a file:// URL pointing to a directory to be used
as the current directory for the current shell.

By default, only local paths are taken into account. To allow
remote paths, configure `mistty-allow-tramp-paths.

Such sequences are typically printed out by shells just before
printing a prompt by a command such as:

  printf \"\\e]7;file://%s%s\\e\\\\\" \"$HOSTNAME\" \"$PWD\"

This can be used as a drop-in replacement for
`ansi-osc-directory-tracker', with the following tweaks:

 - it decodes percent-encoded non-ASCII characters in the paths,
 so such paths look better

 - it has optional supports for remote paths."
  (cond
   ((string-match "file://\\([^/]*\\)\\(/.*\\)" osc-seq)
    (let ((hostname (url-unhex-string (match-string 1 osc-seq)))
          (path (url-unhex-string (match-string 2 osc-seq))))
      (setq path
            (file-name-as-directory
             (decode-coding-string
              path (or file-name-coding-system
                       default-file-name-coding-system)
              'nocopy)))
      (cond
       ;; A local paths.
       ((and (string= hostname (system-name))
             (not (file-remote-p path)))
        (ignore-errors
          (cd-absolute path)))

       ;; Build TRAMP remote paths from the path and hostname.
       ((and mistty-allow-tramp-paths
             (not (string= hostname (system-name)))
             (not (file-remote-p path)))
        (setq default-directory (concat "/" hostname ":" path))))))

   ;; A TRAMP remote path.
   ((and mistty-allow-tramp-paths
         (file-remote-p osc-seq))
    (setq default-directory (file-name-as-directory osc-seq)))))

(provide 'mistty-osc7)

;;; mistty-osc7.el ends here
