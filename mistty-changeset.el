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
  ;; True if the end of work buffer was truncated.
  deleted-point-max

  intervals

  ;; if t, the change has been applied; such a change should
  ;; be removed from mistty--changesets
  applied)

(defsubst mistty--changeset-collected (cs)
  (mistty--changeset-intervals cs))

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

(defun mistty--changeset-collect (cs)
  (unless (mistty--changeset-intervals cs)
    (save-excursion
      (save-restriction
        (narrow-to-region (mistty--changeset-beg cs) (point-max))
        (let ((last-point (point-min))
              intervals last-at-point )
          (goto-char last-point)
          (while (let ((at-point (get-text-property (point) 'mistty-change)))
                   (when last-at-point
                     (push `(,last-point . ,last-at-point) intervals))
                   (setq last-at-point at-point)
                   (setq last-point (point))
                   (goto-char (next-single-property-change (point) 'mistty-change (current-buffer) (point-max)))
                   (< (point) (point-max))))
          (when last-at-point
            (push `(,last-point . ,last-at-point) intervals))
          (when (mistty--changeset-deleted-point-max cs)
            (push `(,(point-max) deleted-to-end) intervals))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (remove-text-properties (point-min) (point-max) '(mistty-change t)))
          (setf (mistty--changeset-intervals cs) (nreverse intervals))))))
  (mistty--changeset-intervals cs))

(defun mistty--changeset-restrict (cs min-pos)
  (when-let ((shift-and-intervals (mistty--restrict-modification-intervals
                                   (mistty--changeset-collect cs) min-pos)))
    (setf (mistty--changeset-intervals cs) (cdr shift-and-intervals))
    (car shift-and-intervals)))

(defun mistty--restrict-modification-intervals (intervals min-pos)
    (if (and (caar intervals) (>= (caar intervals) min-pos))
        (cons 0 intervals)
      
      ;; apply restrictions
      (while (pcase intervals
               ((and `((,_ . ,_ ) (,pos2 . ,_) . ,_) (guard (<= pos2 min-pos)))
                (setq intervals (cdr intervals))
                t)))
      
      ;; intervals now points to the first relevant section, which likely
      ;; starts before min-pos. 
      (let ((base-shift
             (pcase intervals
               (`((,_ shift ,shift) . ,_) shift)
               (`((,_ inserted) (,pos2 shift ,shift) . ,_)
                (+ shift (- pos2 min-pos)))
               (_ ;; other interval restrictions aren't supported
                nil))))
        (when (and base-shift intervals)
          (setcar (car intervals) min-pos)
          
          ;; shifts must be relative to base-shift
          (setq intervals
                (mapcar
                 (lambda (cur)
                   (pcase cur
                     (`(,pos shift ,shift) `(,pos shift ,(- shift base-shift)))
                     (_ cur)))
                 intervals))
          (cons base-shift intervals)))))

(provide 'mistty-changeset)