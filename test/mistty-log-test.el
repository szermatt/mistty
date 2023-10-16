;;; Tests mistty-log.el -*- lexical-binding: t -*-

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

(require 'mistty-log)
(require 'ert)
(require 'ert-x)

(ert-deftest mistty-log-test-start-stop-log-in-buffer ()
  (ert-with-test-buffer ()
    (let ((noninteractive nil)
          (mistty-log nil)
          (mistty-backlog-size 0)
          log-buffer)
      (should (not mistty-log))
      (mistty-log "too early; will not be logged")
      (setq log-buffer (save-excursion (mistty-start-log)))
      (unwind-protect
          (progn
            (should (equal log-buffer mistty-log-buffer))
            (should mistty-log)
            (mistty-log "will be logged (1)")
            (mistty-log "will be logged (2)")
            (mistty-stop-log)
            (should (not mistty-log))
            (mistty-log "too late; will not be logged")
            (with-current-buffer log-buffer
              (goto-char (point-min))
              (should (search-forward "log enabled"))
              (should (search-forward "will be logged (1)"))
              (should (search-forward "will be logged (2)"))
              (goto-char (point-min))
              (should (not (search-forward "will not be logged" nil 'noerror)))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer log-buffer))))))

(ert-deftest mistty-log-test-disable-log-when-buffer-is-killed ()
  (ert-with-test-buffer ()
    (let ((noninteractive nil)
          (mistty-log nil)
          (mistty-backlog-size 0)
          log-buffer)
      (setq log-buffer (save-excursion (mistty-start-log)))
      (kill-buffer log-buffer)
      (should-not mistty-log))))

(ert-deftest mistty-log-test-start-stop-log-in-batch-mode ()
  (ert-with-test-buffer ()
    (let ((noninteractive t)
          (mistty-log nil)
          (mistty-backlog-size 0)
          log-buffer)
      (ert-with-message-capture messages
        (should (not mistty-log))
        (mistty-log "too early; will not be logged")
        (setq log-buffer (save-excursion (mistty-start-log)))
        (should (equal log-buffer mistty-log-buffer))
        (should mistty-log)
        (mistty-log "will be logged (1)")
        (mistty-log "will be logged (2)")
        (mistty-stop-log)
        (should (not mistty-log))
        (mistty-log "too late; will not be logged")

        (with-temp-buffer
          (insert messages)
          (goto-char (point-min))
          (should (search-forward "log enabled"))
          (should (search-forward "will be logged (1)"))
          (should (search-forward "will be logged (2)"))
          (goto-char (point-min))
          (should (not (search-forward
                        "will not be logged" nil 'noerror))))))))

(ert-deftest mistty-log-test-drop-log ()
  (ert-with-test-buffer ()
    (let ((noninteractive nil)
          (mistty-log nil)
          (log-buffer (save-excursion (mistty-start-log))))
      (unwind-protect
          (progn
            (mistty-log "dummy entry")
            (mistty-drop-log)
            (should (not (buffer-live-p log-buffer)))
            (should (null mistty-log-buffer)))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer log-buffer))))))

(ert-deftest mistty-log-test-backlog ()
  (ert-with-test-buffer ()
    (let ((noninteractive nil)
          (mistty-log nil)
          (mistty-backlog-size 3)
          log-buffer)
      (dotimes (i 10)
        (mistty-log "log entry %s" i))
      (setq log-buffer (save-excursion (mistty-start-log)))
      (unwind-protect
          (progn
            (mistty-stop-log)
            (with-current-buffer log-buffer
              (goto-char (point-min))
              (should (search-forward "log entry 7"))
              (should (search-forward "log entry 8"))
              (should (search-forward "log entry 9"))
              (should (not (search-forward "log entry [0-6]" nil 'noerror)))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer log-buffer))))))
