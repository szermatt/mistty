;;; mistty-changeset.el --- Accumulate and replays changes -*- lexical-binding: t -*-

(require 'generator)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-util)

(defvar-local mistty--changesets nil
  "Set of active changes, of type mistty--changeset.

The first change is the most recent one, which might be active.
Use `mistty--active-changeset' to access it.")

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

  ;; Modification intervals collected from the buffer. As long as this
  ;; is nil, details about the modifications are stored in the buffer,
  ;; as text properties.
  intervals

  ;; if t, the change has been applied; such a change should
  ;; be removed from mistty--changesets
  applied)

(defsubst mistty--changeset-collected (cs)
  "Evaluate to a true value if changeset intervals exist.

Calling `mistty-changeset-collect' guarantees this to be true."
  (mistty--changeset-intervals cs))

(defun mistty--activate-changeset ()
  "Create a new active changeset.

Returns the changeset."
  (let ((cs (mistty--active-changeset)))
    (unless cs
      (setq cs (mistty--make-changeset))
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

(defun mistty--changeset-mark-region (cs beg end old-end)
  "Store insertion and deletion for CS into buffer properties.

Creates and activate a changeset as necessary.

BEG to END reports a newly inserted string, BEG to OLD-END a
recently deleted string."
  (setf (mistty--changeset-beg cs)
        (max (point-min)
             (if (mistty--changeset-beg cs)
                 (min (mistty--changeset-beg cs) beg)
               beg)))
  (setf (mistty--changeset-end cs)
        (min (point-max)
             (if (mistty--changeset-end cs)
                 (max (mistty--changeset-end cs) end)
               end)))
  
  ;; Mark the text that was inserted
  (put-text-property beg end 'mistty-change '(inserted))
  
  ;; Update the shift value of everything that comes after.
  (let ((shift (- old-end end))
        (pos end))
    (while (< pos (point-max))
      (let ((next-pos (next-single-property-change pos 'mistty-change (current-buffer) (point-max))))
        (pcase (get-text-property pos 'mistty-change)
          (`(shift ,old-shift)
           (put-text-property pos next-pos 'mistty-change `(shift ,(+ old-shift shift))))
          ('() (put-text-property pos next-pos 'mistty-change `(shift ,shift))))
        (setq pos next-pos)))
    (when (and (> old-end beg) (= end (point-max)))
      (setf (mistty--changeset-deleted-point-max cs) t))))

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
  "Restrict the changes in changeset CS to the range [MIN-POS, (point-max)].

The function returns the difference between the work buffer and
term buffer at MIN-POS (shift), or nil if a restriction isn't
possible after MIN-POS."
  (let ((intervals (mistty--changeset-intervals cs)))
    (if (and (caar intervals) (>= (caar intervals) min-pos))
        0 ;; shift is 0, intervals don't change.
      
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
          (setf (mistty--changeset-intervals cs) intervals)
          base-shift)))))

(defun mistty--changeset-modifications (cs)
  "Returns modifications to re-apply for changeset CS.

Returns a list of (beg content old-length), with beg the
beginning position, content text that's inserted at beg and
old-length the length of text deleted from beg. old-length might
be -1 to mean 'delete everything from pos to the end of the buffer'."
  (let ((intervals (mistty--changeset-collect cs))
        (changes nil)
        (last-shift 0))
    (while intervals
      (pcase intervals
        ;; insert in the middle, possibly replacing a section of text
        (`((,start inserted) (,end shift ,end-shift) . ,_)
         (push (list (+ start last-shift)
                     (mistty--safe-bufstring start end)
                     (- (+ end end-shift) (+ start last-shift)))
               changes)
         ;; processed 2 entries this loop, instead of just 1
         (setq intervals (cdr intervals)))

        ;; insert at end, delete everything after
        (`((,start inserted) (,end deleted-to-end))
         (push (list (+ start last-shift)
                     (mistty--safe-bufstring start end)
                     -1)
               changes)
         ;; processed 2 entries this loop, instead of just 1
         (setq intervals (cdr intervals)))

        ;; insert at end
        (`((,start inserted))
         (push (list (+ start last-shift)
                     (mistty--safe-bufstring start (point-max))
                     0)
               changes))

        ;; delete a section of original text
        ((and `((,pos shift ,shift) . ,_)
              (guard (> shift last-shift)))
         (push (list (+ pos last-shift)
                     ""
                     (- shift last-shift))
               changes))

        ;; delete to the end of the original text
        (`((,pos deleted-to-end))
         (push (list (+ pos last-shift) "" -1)
               changes)))
      
      ;; prepare for next loop
      (pcase (car intervals)
        (`(,_ shift ,shift) (setq last-shift shift)))
      (setq intervals (cdr intervals)))
    changes))

(provide 'mistty-changeset)
