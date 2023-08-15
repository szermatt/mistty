;;; mistty-changeset.el --- Accumulate and replays changes -*- lexical-binding: t -*-

(require 'generator)
(eval-when-compile
  (require 'cl-lib))

(defvar-local mistty--changesets nil
  "Set of active changes.")

(cl-defstruct (mistty--changeset
               (:constructor mistty--make-changeset)
               (:conc-name mistty--changeset-)
               (:copier nil))
  collected
  applied)

(defun mistty--activate-changeset ()
  "Create a new active changeset or reuse an existing one.

Returns the changeset."
  (let ((changeset (mistty--active-changeset)))
    (unless changeset
      (setq changeset (mistty--make-changeset))
      (push changeset mistty--changesets))
    changeset))

(defun mistty--active-changeset ()
  "Return the active changeset or nil.

The active changeset is the first changeset on
`mistty--changesets' and hasn't been collected or applied yet."
  (let ((head (car mistty--changesets)))
    (when
        (and head
             (not (mistty--changeset-collected head)))
      head)))

(provide 'mistty-changeset)
