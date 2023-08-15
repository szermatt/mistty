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
  ;; beginning of the changed region in the work buffer.
  beg
  ;; end of the changed region in the work buffer.
  end
  
  collected
  ;; if t, the change has been applied; such a change should
  ;; be removed from mistty--changesets
  applied)

(defun mistty--activate-changeset (beg end)
  "Create a new active changeset for the region BEG-END or reuse one.

Returns the changeset."
  (let ((cs (mistty--active-changeset)))
    (if cs
        (progn
          (setf (mistty--changeset-beg cs)
                (max (point-min)
                     (min (mistty--changeset-beg cs) beg)))
          (setf (mistty--changeset-end cs)
                (min (point-max)
                     (max (mistty--changeset-end cs) end))))
              
      (setq cs (mistty--make-changeset :beg beg :end end))
      (push cs mistty--changesets))
    cs))

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
