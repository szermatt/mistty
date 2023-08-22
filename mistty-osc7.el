;;; mistty-osc7.el --- Add support for OSC 7 to term-mode  -*- lexical-binding: t -*-

(require 'url-util)

(defun mistty-osc7 (_ osc-seq)
  (when (string-match "file://\\([^/]*\\)\\(/.*\\)" osc-seq)
    (let ((hostname (url-unhex-string (match-string 1 osc-seq)))
          (path (url-unhex-string (match-string 2 osc-seq))))
      (when (and (string= hostname (system-name))
                 (file-directory-p path))
        (setq
         path (decode-coding-string
               path (or file-name-coding-system
                        default-file-name-coding-system) 'nocopy))
        (setq default-directory path)))))


(provide 'mistty-osc7)
