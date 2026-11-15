;;; mistty-sync.el --- Buffer synchronization for MisTTY -*- lexical-binding: t -*-

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
;; The functions in this file are meant to copy content from one
;; buffer to another over and over while keeping markers in the
;; destination buffers as much as possible. This is based on
;; replace-region-contents.

(require 'mistty-util)

;;; Code:

(defvar-local mistty--point-marker nil
  "Marker (re)used by `mistty--sync-buffer' on Emacs 31 and later.")

(if (eval-when-compile (>= emacs-major-version 31))
    ;; Emacs 31 and later, using replace-region-contents
    (defun mistty--sync-buffers
        (source-buffer source-pos dest-buffer dest-pos)
      "Copy SOURCE-BUFFER from SOURCE-POS to eob to DEST-BUFFER.

The text and text properties of the region from DEST-POS to eob is
overwritten. This function attempt to keep
markers point and overlays as it rewrites the text."
      (mistty--with-live-buffer source-buffer
        (let ((properties (mistty--save-properties source-pos))
              (source-def (vector source-buffer source-pos (point-max))))
          (with-current-buffer dest-buffer
            (let ((point-marker (or mistty--point-marker
                                    (setq mistty--point-marker (make-marker)))))
              (set-marker point-marker (point))
              (set-marker-insertion-type point-marker nil)
              (replace-region-contents dest-pos (point-max) source-def 0.2)
              (goto-char point-marker)
              (move-marker point-marker nil))
            (mistty--restore-properties properties dest-pos)))))

  ;; Emacs 29 and 30, using replace-buffer-contents and narrowing.
  (defun mistty--sync-buffers
      (source-buffer source-pos dest-buffer dest-pos)
    "Copy SOURCE-BUFFER from SOURCE-POS to eob to DEST-BUFFER.

The text and text properties of the region from DEST-POS to eob is
overwritten. This function attempt to keep
markers point and overlays as it rewrites the text."
    (mistty--with-live-buffer source-buffer
      (save-restriction
        (narrow-to-region source-pos (point-max))
        (let ((properties (mistty--save-properties (point-min))))
          (with-current-buffer dest-buffer
            (let ((old-point (and (< (point) dest-pos) (point))))
              (save-restriction
                (narrow-to-region dest-pos (point-max))
                (replace-buffer-contents source-buffer 0.2)
                (mistty--restore-properties properties (point-min)))

              ;; If the point was outside the sync region, restore it,
              ;; as it has been moved by narrow-to-region . Otherwise,
              ;; trust replace-buffer-contents to do something
              ;; reasonable with it.
              (when old-point
                (goto-char old-point)))))))))

(defun mistty--save-properties (start)
  "Extracts the properties from START in the current buffer.

Returns a list of (BEG END PROPERTIES), ordered, with positions
relative to START."
  (let ((pos start) intervals)
    (while (< pos (point-max))
      (let ((props (text-properties-at pos))
            (last-pos pos))
        (setq pos (next-property-change pos nil (point-max)))
        (push `(,(- last-pos start) ,(- pos start) ,props)
              intervals)))

    intervals))

(defun mistty--restore-properties (intervals start)
  "Apply saved properties INTERVALS to the buffer at START.

This is the reverse operation of `mistty--save-properties'."
  (pcase-dolist (`(,beg ,end ,props) intervals)
    (set-text-properties (+ beg start) (+ end start) props)))

(provide 'mistty-sync)

;;; mistty-sync.el ends here
