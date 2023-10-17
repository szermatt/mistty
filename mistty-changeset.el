;;; mistty-changeset.el --- Accumulate and replays changes -*- lexical-binding: t -*-

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

;;; Commentary:
;;
;; This file contains helper utilities for mistty.el for collecting
;; buffer modifications and forward them to the terminal.
;;
;; It defines `mistty--changesets' a list of `mistty--changeset'
;; struct instances.
;;
;; Each `mistty--changeset' tracks of a set of buffer modifications to
;; be replayed, some of which might be kept in the buffer as text with
;; specific text properties. Such changes kept in the buffer are
;; linked to the active changeset, returned by
;; `mistty--active-changeset'.
;;
;; New buffer modifications are reported and linked to the active
;; changeset by `mistty-changeset-mark-region'.  They are then kept in
;; the buffer until they're collected and eventually transformed into
;; a set of modifications to be replayed by
;; `mistty--changeset-modifications'.

(require 'generator)

;;; Code:

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
  intervals)

(defsubst mistty--changeset-collected (changeset)
  "Evaluate to a true value if CHANGESET intervals exist.

Calling `mistty-changeset-collect' guarantees this to be true."
  (mistty--changeset-intervals changeset))

(defun mistty--activate-changeset ()
  "Create a new active changeset.

Returns the changeset."
  (let ((changeset (mistty--active-changeset)))
    (unless changeset
      (setq changeset (mistty--make-changeset))
      (push changeset mistty--changesets))
    changeset))

(defun mistty--release-changeset (changeset)
  "Remove CHANGESET from `mistty-changeset'.

This function also cleans up any change information left in the
 work buffer."
  (when (and (mistty--changeset-beg changeset)
             (not (mistty--changeset-collected changeset)))
    (remove-text-properties (mistty--changeset-beg changeset)
                            (point-max) '(mistty-change t)))
  (setq mistty--changesets (delq changeset mistty--changesets)))

(defun mistty--active-changeset ()
  "Return the active changeset or nil.

The active changeset is the first changeset on
`mistty--changesets' and hasn't been collected yet."
  (let ((head (car mistty--changesets)))
    (when
        (and head
             (not (mistty--changeset-collected head)))
      head)))

(defun mistty--changeset-mark-region (changeset beg end old-end)
  "Store insertion and deletion for CHANGESET into buffer properties.

Creates and activate a changeset as necessary.

BEG to END reports a newly inserted string, BEG to OLD-END a
recently deleted string."
  (let ((beg (mistty--safe-pos beg))
        (end (mistty--safe-pos end)))
    (setf (mistty--changeset-beg changeset)
          (if-let ((changeset-beg (mistty--changeset-beg changeset)))
              (min (mistty--safe-pos changeset-beg) beg)
            beg))
    (setf (mistty--changeset-end changeset)
          (if-let ((changeset-end (mistty--changeset-end changeset)))
              (max (mistty--safe-pos changeset-end) end)
            end))

    ;; Mark the text that was inserted
    (when (> end beg)
      (put-text-property beg end 'mistty-change '(inserted)))

    ;; Update the shift value of everything that comes after.
    (let ((shift (- old-end end))
          (pos end))
      (while (< pos (point-max))
        (let ((next-pos (next-single-property-change pos 'mistty-change (current-buffer) (point-max))))
          (when (> next-pos pos)
            (pcase (get-text-property pos 'mistty-change)
              (`(shift ,old-shift)
               (put-text-property pos next-pos 'mistty-change `(shift ,(+ old-shift shift))))
              ('() (put-text-property pos next-pos 'mistty-change `(shift ,shift)))))
          (setq pos next-pos)))
      (when (and (> old-end beg) (= end (point-max)))
        (setf (mistty--changeset-deleted-point-max changeset) t)))))

(defun mistty--changeset-single-insert (cs)
  "If CS is just a single insertion, return it.

Otherwise return nil."
  (pcase (mistty--changeset-collect cs)
    (`((,pos1 inserted) (,pos2 shift ,_))
     (mistty--safe-bufstring pos1 pos2))
    (`((,pos1 inserted))
     (mistty--safe-bufstring pos1 (point-max)))))

(defun mistty--changeset-collect (changeset)
  "Extract CHANGESET data stored into text properties.

The second time this is called, this just returns
`mistty--changeset-intervals'."
  (unless (mistty--changeset-intervals changeset)
    (save-excursion
      (save-restriction
        (narrow-to-region (mistty--changeset-beg changeset) (point-max))
        (let ((last-point (point-min))
              intervals last-at-point )
          (goto-char last-point)
          (while (let ((at-point (get-text-property (point) 'mistty-change)))
                   (when last-at-point
                     (push `(,last-point . ,last-at-point) intervals))
                   (setq last-at-point at-point)
                   (setq last-point (point))
                   (goto-char (next-single-property-change (point) 'mistty-change (current-buffer) (point-max)))
                   (not (eobp))))
          (when last-at-point
            (push `(,last-point . ,last-at-point) intervals))
          (when (mistty--changeset-deleted-point-max changeset)
            (push `(,(point-max) deleted-to-end) intervals))
          (let ((inhibit-read-only t)
                (inhibit-modification-hooks t))
            (remove-text-properties (point-min) (point-max) '(mistty-change t)))
          (setf (mistty--changeset-intervals changeset) (nreverse intervals))))))
  (mistty--changeset-intervals changeset))

(defun mistty--changeset-restrict (changeset min-pos)
  "Restrict the content of CHANGESET to the range [MIN-POS,].

The function returns the difference between the work buffer and
term buffer at MIN-POS (shift), or nil if a restriction isn't
possible after MIN-POS."
  (let ((intervals (mistty--changeset-collect changeset)))
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
          (setf (mistty--changeset-intervals changeset) intervals)
          base-shift)))))

(defun mistty--changeset-modifications (changeset)
  "Return modifications to re-apply for CHANGESET.

This function returns a list of (beg content old-length), with beg
the beginning position, content text that\'s inserted at beg and
old-length the length of text deleted from beg. old-length might
be -1 to mean delete everything from pos to the end of the
buffer."
  (let ((intervals (mistty--changeset-collect changeset))
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

;;; mistty-changeset.el ends here
