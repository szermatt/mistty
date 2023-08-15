;;; mistty-changeset.el --- Accumulate and replays changes -*- lexical-binding: t -*-

(require 'generator)
(eval-when-compile
  (require 'cl-lib))

(require 'mistty-util)

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

(defun mistty--changeset-modifications (cs)
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
