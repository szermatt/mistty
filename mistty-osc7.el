;;; mistty-osc7.el --- Add support for OSC 7 to term-mode  -*- lexical-binding: t -*-

(require 'url-util)

(defvar mistty-allow-tramp-paths t
  "If true, allow TRAMP paths as shell-specified directory.

Make sure tramps works before enabling.

This affects directories set using OSC 7, which can then:

 - build TRAMP remote paths based on the hostname specified in
   the file:// URL, using the default method.

 - accept TRAMP remote paths embedded OSC 7 text (nonstandard)")

(defun mistty-osc7 (_ osc-seq)
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
             (string= hostname "")
             (file-remote-p path))
        (setq default-directory path)))))

   ;; A TRAMP remote path.
   ((and mistty-allow-tramp-paths
         (file-remote-p osc-seq))
    (setq default-directory (file-name-as-directory osc-seq)))))

(provide 'mistty-osc7)
