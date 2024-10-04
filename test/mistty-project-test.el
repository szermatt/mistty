;;; Tests mistty-project.el -*- lexical-binding: t -*-

(require 'mistty)
(require 'mistty-project)
(require 'mistty-testing)

(require 'project)

;; defines a project type 'fake
(cl-defmethod project-name ((project (head fake)))
  "fake")

(cl-defmethod project-root ((project (head fake)))
  (nth 1 project))

(cl-defmethod project-buffers ((project (head fake)))
  (cdr (cdr project)))

(ert-deftest mistty-project-test-mistty-in-project ()
  (ert-with-temp-directory project-dir
    (let* ((mistty-shell-command mistty-test-bash-exe)
           (default-directory project-dir)
           (project-current-directory-override project-dir)
           (fake-project (list 'fake project-dir))
           (project-find-functions (list (lambda (dir)
                                           (when (string= dir project-dir)
                                             fake-project))))
           buf buf-not-in-project)
      (unwind-protect
          (progn
            (should (equal fake-project (project-current)))
            ;; mistty-in-project should ignore this buffer
            (setq buf-not-in-project (mistty-create))

            ;; create new with a specific name
            (setq buf (mistty-in-project))
            (should (equal (project-prefixed-buffer-name "mistty") (buffer-name buf)))
            (setcdr (cdr fake-project) (list buf))
            (should (equal (list buf) (project-buffers fake-project)))

            ;; choose existing
            (switch-to-buffer "*scratch*")
            (should (equal buf (mistty-in-project))))

        ;; kill projects
        (let ((kill-buffer-query-functions nil))
          (when buf
            (kill-buffer buf))
          (when buf-not-in-project
            (kill-buffer buf-not-in-project)))))))

(ert-deftest mistty-project-test-kill-buffer-condition ()
  (let ((project-kill-buffer-conditions
         '(buffer-file-name
           (and
            (major-mode . fundamental-mode)
            "\\`[^ ]")
           (and (derived-mode . special-mode)
                (not (major-mode . help-mode))
                (not (derived-mode . gnus-mode)))
           (derived-mode . compilation-mode)
           (derived-mode . dired-mode)
           (derived-mode . diff-mode)
           (derived-mode . comint-mode)
           (derived-mode . eshell-mode)
           (derived-mode . change-log-mode))))
    (mistty-project-init-kill-buffer)
    (should (equal '(buffer-file-name
                     (and
                      (major-mode . fundamental-mode)
                      "\\`[^ ]")
                     (and (derived-mode . special-mode)
                          (not (major-mode . help-mode))
                          (not (derived-mode . gnus-mode)))
                     (derived-mode . compilation-mode)
                     (derived-mode . dired-mode)
                     (derived-mode . diff-mode)
                     (derived-mode . comint-mode)
                     ;; added:
                     (derived-mode . mistty-mode)
                     (derived-mode . eshell-mode)
                     (derived-mode . change-log-mode))
                   project-kill-buffer-conditions))))
