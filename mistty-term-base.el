;;; mistty-term-base.el --- Generic methods for terminal access -*- lexical-binding: t -*-

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
;; This file defines generic methods called by mistty to communicate
;; with the selected terminal type (builtin or mod).

(require 'cl-lib)

;;; Code:

(cl-defgeneric mistty--create-term
    (type name command
          &key width height
          enter-fullscreen
          leave-fullscreen
          active-prompt
          after-clear-screen
          sync-scrolline)
  "Create a new term buffer of the given TYPE with name NAME.

The buffer runs COMMAND, a list containing the program to run and its
arguments.

LOCAL-MAP specifies a local map to be used as the char-mode map.

WIDTH and HEIGHT are the initial dimension of the terminal
reported to the remote process.

This function returns an instance of the generic terminal type, which
allows getting hold of the buffer and process.

ENTER-FULLSCREEN is to be called when entering fullscreen mode. It takes
a single boolean argument which specifies whether this is split-buffer
fullscreen mode or normal fullscreen mode.

LEAVE-FULLSCREEN is a function that takes the TERM instance and
leaves fullscreen mode.

ACTIVE-PROMPT should return the active `mistty--prompt'.

AFTER-CLEAR-SCREEN is to be called right after the screen has been cleared.

SYNC-SCROLLINE-FUNC is a function that return the current sync scrolline.")

(cl-defgeneric mistty--term-buf (term)
  "Return the TERM's terminal or process buffer.")

(cl-defgeneric mistty--term-proc (term)
  "Return the TERM's process.")

(cl-defgeneric mistty--term-screen-top-pos (term)
  "Return the marker for the start of TERM's terminal.

The marker is only valid in the terminal buffer.")

(cl-defgeneric mistty--term-screen-top-scrolline (term)
  "Return the scrolline for he start of TERM's terminal.")

(cl-defgeneric mistty--term-alt-screen-p (term)
  "Return non-nil when TERM is displaying the alternate screen buffer.")

(cl-defgeneric mistty--term-detect-prompt-p (term)
  "Return non-nil prompt detection should be enabled in TERM.")

(cl-defgeneric mistty--term-lines (term)
  "Return the height of TERM's terminal, in lines.")

(cl-defgeneric mistty--term-columns (term)
  "Return the width of TERM's terminal, in columns.")

(cl-defgeneric mistty--term-cursor-linecol (term)
  "Return the position of TERM's cursor as (LINE . COL).

The position of the cursor in terms of characters is available as the
process marker. This is different, especially the column number as in
general, in unicode, there's no direct link between character count and
column number.")

(cl-defgeneric mistty--term-sentinel-func (term)
  "Return the hardcoded sentinel function of TERM's terminal.")

(cl-defgeneric mistty--term-filter-func (term)
  "Return the hardcoded filter function of TERM's terminal.")

(defun mistty--term-sentinel (proc msg)
  "Call the hardcoded sentinel function.

PROC and MSG are as passed by a process to a sentinel function.

This might be different from the sentinel set on PROC."
  (funcall (mistty--term-sentinel-func (process-get proc 'mistty-term)) proc msg))

(cl-defgeneric mistty--term-resize (term width height)
  "Set the terminal size for TERM to WIDTH x HEIGHT.")

(cl-defgeneric mistty--term-autoresize (term enable)
  "Enable or disable auto-resizing of TERM based on the buffer windows.

A non-nil value for ENABLE enables autoresize, a nil value disables it.")

(defun mistty--term-is-term-buffer (buffer)
  "Return non-nil if BUFFER is a term buffer."
  (when-let* ((proc (get-buffer-process buffer)))
    (process-get proc 'mistty-term)))

(cl-defgeneric mistty--term-setup-buffer (term fullscreen)
  "Prepare TERM's terminal/process buffer for use.

If FULLSCREEN is non-nil, prepare the buffer for fullscreen mode")

(cl-defgeneric mistty--term-sync
    (term dest-buffer sync-pos sync-scrolline keep-markers cursor-marker)
  "Update the content of DEST-BUFFER to match the terminal.

The buffer from SYNC-POS to EOB is rewritten to match the content of the
terminal below SYNC-SCROLLINE.

If KEEP-MARKERS is nil, the function may complete rewrite and discard
the content of the buffer within that range. If KEEP-MARKERS is non-nil,
the function needs to do its best to keep markers and overlays and move
them as appropriate.

The function return a (cons SYNC-POS SYNC-SCROLLINE) containing new
proposed values for future calls. It may propose to increase the sync
scrolline, in cases where SYNC-SCROLLINE is above the top of the
terminal screen, or to decrease it, in cases where changes were detected
above SYNC-SCROLLINE.

Note that in the latter case, the function may modify DEST-BUFFER above
SYNC-POS and the new proposed value may be above the initial value.

DEST-BUFFER can change from one call to the next. If DEST-BUFFER is the
same as in the last call, the function may assume that changes
previously made are still in place to optimize its operations.

CURSOR-MARKER must be a marker. It will be updated with the position of
the cursor on DEST-BUFFER.")

(cl-defgeneric mistty--term-clear-to-eol (term pos)
  "Mark spaces in TERM from POS to end-of-line as unmodified.")

(cl-defgeneric mistty--term-cleanup-prompt-sp (term pos)
  "Cleanup after the shell using the prompt-sp hack in TERM at POS.

This command cleans up the terminal after a trick used to detect output
that doesn't end in a newline is called prompt sp. That trick consists
of outputing an optional end-of-line marker, then columns-1 spaces and a
CR. If we end up still on the same line, the output ended with a NL and
the whole line, and the marker, is then overwritten. If we end up on
another line due to line wrap, the previous line and the marker stay in
the previous line.

This results in a continuation line that shouldn't be continued and a
large number of newlines, both of which will look bad when they enter
scrollback.

To work around it, this call transform the fake newline into a real one
and marks the spaces at the end of the previous line as blank.

POS should be the position where the CR is called in the prompt-sp
sequence.")

(cl-defgeneric mistty--term-changed (term beg end)
  "Mark the region of TERM between BEG and END as requiring post-processing.")

(cl-defgeneric mistty--term-truncate-buffer (term scrolline-limit)
  "Truncate TERM's buffer, if necessary.

Always keep SCROLLINE-LIMIT and below.")

(provide 'mistty-term-base)

;;; mistty-term-base.el ends here

