;;; mistty.el --- Queue of terminal actions for mistty.el. -*- lexical-binding: t -*-

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

;;; Code:

(eval-when-compile
  (require 'cl-lib))

;;; Commentary:
;;
;; This file defines the struct `mistty--queue' to be used in mistty.el.
;;
;; `mistty--queue' sends strings to the terminal process and reacts to their
;; effect asynchronously, using `mistty--interact` instance.

(require 'mistty-log)
(require 'mistty-util)

(defvar mistty-timeout-s 0.5)
(defvar mistty-stable-delay-s 0.1)

(defvar mistty-max-try-count 1
  "Maximum number of tries in `mistty--interact-wrap-accept'.")

(defvar mistty--report-issue-function nil
  "A function that is told about non-fatal issues (debugging).

This function is called with a single argument describing the
possible problem:

 - ==\'hard-timeout ==\'soft-timeout A timeout is reported when
  some key sequence sent to the terminal hasn't had the expected
  effect after a certain time.

  A hard timeout is reported when nothing was sent back, a soft
  timeout when something was sent back, but it didn't satisfy the
  condition.

  This is to be expected, there are cases where terminal
  sequences aren't meant to have an effect, such as sequence that
  attempts to move left through the prompt.

- ==\'already-matching A condition used to detect the effect of
  a key sequence matches even before the key sequence is sent.
  This is a sign that the condition is not appropriate.")

;; A queue of strings to send to the terminal process.
;;
;; The queue contains mistty--interact, which generates strings to
;; send to the terminal.
(cl-defstruct (mistty--queue
               (:constructor mistty--make-queue (proc))
               (:conc-name mistty--queue-)
               (:copier nil))
  ;; The process the queue is communicating with.
  proc

  ;; A `mistty--interact' that yields strings to send to the terminal
  ;; or nil.
  interact

  ;; A queue of `mistty--interact' to use after INTERACT.
  more-interacts

  ;; Timer used by mistty--dequeue-with-timer.
  timer

  ;; Timer called if the process doesn't not answer after a certain
  ;; time.
  timeout)

;; Asynchronous terminal interaction, to add into the queue.
(cl-defstruct (mistty--interact
               (:constructor mistty--make-interact
                             (type
                              &aux
                              (initial-buf (current-buffer))
                              (buf initial-buf)))
               (:conc-name mistty--interact-)
               (:copier nil))
  ;; Symbol that identifies the type of interaction.
  type

  ;; Callback function that will handle the next call to
  ;; mistty--interact-next. It takes a single argument.
  ;;
  ;; Such callbacks must never return a value. They must instead call
  ;; mistty--interact-return, mistty--interact-keep-waiting or
  ;; mistty--interact-done, which both throw 'mistty--interact-return.
  ;;
  ;; CB initially runs within the buffer that was current when
  ;; `mistty--make-interact' was called. If CB modifies its current
  ;; buffer, it is stored by `mistty--interact-next' and set for the
  ;; next call to CB. The buffer that's current when
  ;; `mistty--interact-next' doesn't matter, though it is guaranteed
  ;; to be restored when it returns.
  ;;
  ;; As such, it is safe to use `set-buffer' within CB, as a buffer
  ;; set that way sticks around between calls to CB and doesn't
  ;; interfere with other pieces of the code.
  ;;
  ;; CB gets one argument, which can be one of:
  ;; - nil, for the first call
  ;; - 'intermediate every time the process sends something, which might
  ;;   not be complete
  ;; - 'stable once the process stops sending data for a certain time
  ;; - 'timeout if the process did not send anything for a long time
  ;; - 'fire-and-forget if the last value returned was a fire-and-forget
  ;;
  ;; CB then raises 'mistty--interact-return with one of the following
  ;; values:
  ;; - a non-empty string to send to the terminal
  ;; - 'keep-waiting to just wait for more output
  ;; - 'done once the interaction is done. It'll be closed and discarded.
  ;; (This is normally done by calling the function mistty--interact-return,
  ;; mistty--interact-done or mistty--interact-keep-waiting.)
  cb

  ;; A function that releases any resource held during the
  ;; interaction. It is called once It might be called even if the
  ;; interaction is not ended or never started.
  cleanup

  ;; Run interaction-specific behavior.
  call

  ;; The buffer that is current for the interaction. It'll be
  ;; set before the next call to CB.
  buf

  ;; The buffer that was current when `mistty--make-interact' was
  ;; called. It is set before calling CLEANUP.
  initial-buf)

(cl-defmacro mistty--interact (type (var) &rest body)
  "Create and return an interaction.

The returned interaction is of type TYPE.

VAR, usually \"interact\", is bound to the interaction, available
in BODY.

BODY defines the initial value of the interact callback.

Returns the newly-created interaction."
  (declare (indent 2))
  `(let ((,var (mistty--make-interact (quote ,type))))
     (setf (mistty--interact-cb ,var)
           (lambda (&optional _) ,@body))
     ,var))

(defsubst mistty--interact-callable-p (interact)
  "Return non-nil if INTERACT is callable."
  (functionp (mistty--interact-call interact)))

(defsubst mistty--interact-done (&optional _)
  "Return \\='done from an interact.

This is equivalent to calling:
  (mistty--interact-return interact \\='done)

This function accepts an optional argument so that it can safely
be pass to a :then argument in `mistty--interact-return'."
  (throw 'mistty--interact-return 'done))

(defsubst mistty--interact-keep-waiting ()
  "Return \\='keep-waiting from an interact.

This is equivalent to calling:
  (mistty--interact-return interact \\='keep-waiting)"
  (throw 'mistty--interact-return 'keep-waiting))

(defsubst mistty--queue-empty-p (queue)
  "Return t if QUEUE generator hasn't finished yet."
  (not (mistty--queue-interact queue)))

(defun mistty--queue-interact-type (queue)
  "Return the type of the QUEUE current interact or nil."
  (when-let ((interact (mistty--queue-interact queue)))
    (mistty--interact-type interact)))

(defun mistty--queue-last-interact (queue)
  "Return the last interact of QUEUE or nil.

The last interact often is the one that's currently running."
  (if-let ((more (mistty--queue-more-interacts queue)))
      (car (last more))
    (mistty--queue-interact queue)))

(defun mistty--send-string (proc str)
  "Send STR to PROC, if it is still live."
  (when (and (mistty--nonempty-str-p str)
             (process-live-p proc))
    (mistty-log "SEND[%s]" str)
    (process-send-string proc str)))

(defun mistty--enqueue-str (queue str &optional fire-and-forget)
  "Enqueue sending STR to the terminal into QUEUE.

Does nothing is STR is nil or empty."
  (when (mistty--nonempty-str-p str)
    (mistty--enqueue
     queue
     (mistty--interact send-str (interact)
       (mistty--interact-return
        interact
        (if fire-and-forget
            `(fire-and-forget ,str)
          str)
        :then #'mistty--interact-done)))))

(defun mistty--enqueue (queue interact &optional prepend)
  "Add INTERACT to QUEUE.

INTERACT is a `mistty--interact' instance that generates strings
to send to the process and then reacts to the process output and
its state, through a callback called by this function.

If the queue is empty, this function also kicks things off by
sending the first string generated by INTERACT to the process.

If the queue is not empty, INTERACT is put at the end of the
queue, to be executed afterwards. If PREPEND is non-nil, it is
added at the beginning of the queue. PREPEND is useful when
called from within an interaction, to trigger another interaction
right away.

Does nothing if INTERACT is nil."
  (cl-assert (mistty--queue-p queue))
  (when interact
    (cond
     ;; This is the first mistty-interact; kick things off.
     ((mistty--queue-empty-p queue)
      (mistty-log "START %s" (mistty--interact-type interact))
      (setf (mistty--queue-interact queue) interact)
      (mistty--dequeue queue))
     ;; Execute this interact next
     (prepend
      (push interact (mistty--queue-more-interacts queue)))
     ;; Execute this interact in order
     (t
      (setf (mistty--queue-more-interacts queue)
            (append (mistty--queue-more-interacts queue)
                    (list interact)))))))

(defun mistty--dequeue (queue &optional value)
  "Send the next string from QUEUE to the terminal.

If VALUE is set, send that value to the first call to
`mistty--interact-next'."
  (cl-assert (mistty--queue-p queue))
  (mistty--dequeue-1 queue value)
  (unless (mistty--queue-empty-p queue)
    (setf (mistty--queue-timeout queue)
          (run-with-timer
           mistty-timeout-s nil #'mistty--timeout-handler
           (current-buffer) queue))))

(cl-defun mistty--dequeue-1 (queue value)
  "Internal helper for `mistty--dequeue'.

This function does all the work of `mistty-dequeue'. See its
description for the meaning of QUEUE and VALUE."
  (let ((proc (mistty--queue-proc queue)))
    (mistty--cancel-timeout queue)
    (while (mistty--queue-interact queue)
      (pcase (condition-case-unless-debug err
                 (mistty--interact-next (mistty--queue-interact queue) value)
               (error
                (mistty-log "Interaction failed; giving up: %s" err)
                (message "mistty: Interaction failed; giving up: %s" err)
                'done))

        ('done
         (setq value nil)
         (mistty--interact-close (mistty--queue-interact queue))
         (setf (mistty--queue-interact queue)
               (pop (mistty--queue-more-interacts queue)))
         (when (mistty--queue-interact queue)
           (mistty-log "NEXT %s" (mistty--queue-interact-type queue))))

        ;; Keep waiting
        ('keep-waiting
         (cl-return-from mistty--dequeue-1))

        ;; Fire-and-forget; no need to wait for a response
        ((and `(fire-and-forget ,str)
              (guard (mistty--nonempty-str-p str)))
         (mistty--send-string proc str)
         (setq value 'fire-and-forget))

        ;; Normal sequences
        ((and (pred mistty--nonempty-str-p) str)
         (mistty--send-string proc str)
         (cl-return-from mistty--dequeue-1))

        (invalid (error "Yielded invalid value: '%s'"
                        invalid))))))

(defun mistty--dequeue-with-timer (queue &optional value)
  "Call `mistty--dequeue' on QUEUE with VALUE on a timer.

The idea is to accumulate updates that arrive at the same time
from the process, waiting for it to pause.

This function restarts the timer if a dequeue is already
scheduled."
  (cl-assert (mistty--queue-p queue))
  (mistty--cancel-timeout queue)
  (mistty--cancel-timer queue)
  (unless (mistty--queue-empty-p queue)
    (setf (mistty--queue-timer queue)
          (run-with-timer
           mistty-stable-delay-s nil #'mistty--queue-timer-handler
           (current-buffer) queue value))))

(defun mistty--cancel-queue (queue)
  "Clear QUEUE and cancel all pending actions.

The queue remains usable, but empty."
  (when (mistty--queue-interact queue)
    (mistty-log "CLOSE %s" (mistty--queue-interact-type queue))
    (mistty--interact-close (mistty--queue-interact queue))
    (setf (mistty--queue-interact queue) nil))
  (while (mistty--queue-more-interacts queue)
    (mistty--interact-close (pop (mistty--queue-more-interacts queue))))
  (mistty--cancel-timeout queue)
  (mistty--cancel-timer queue))

(defun mistty--cancel-timeout (queue)
  "Cancel the timeout timer in QUEUE."
  (when (and (mistty--queue-p queue) (timerp (mistty--queue-timeout queue)))
    (cancel-timer (mistty--queue-timeout queue))
    (setf (mistty--queue-timeout queue) nil)))

(defun mistty--cancel-timer (queue)
  "Cancel the timer in QUEUE."
  (when (and (mistty--queue-p queue) (timerp (mistty--queue-timer queue)))
    (cancel-timer (mistty--queue-timer queue))
    (setf (mistty--queue-timer queue) nil)))

(defun mistty--timeout-handler (buf queue)
  "Handle timeout in QUEUE.

The code is executed inside BUF.

This function is meant to be use as timer handler."
  (cl-assert (mistty--queue-p queue))
  (mistty--with-live-buffer buf
    (let ((proc (mistty--queue-proc queue)))
      (when (and (mistty--queue-timeout queue)
                 ;; last chance, in case some scheduling kerfuffle meant
                 ;; process output ended up buffered.
                 (not (and (process-live-p proc)
                           (accept-process-output proc 0 nil t))))
        (setf (mistty--queue-timeout queue) nil)
        (mistty-log "TIMEOUT")
        (mistty--dequeue queue 'timeout)))))

(defun mistty--queue-timer-handler (buf queue value)
  "Call `mistty--dequeue' on QUEUE in an idle timer.

VALUE is passed to `mistty--dequeue'.

The code is executed inside BUF.

This function is meant to be use as timer handler."
  (cl-assert (mistty--queue-p queue))
  (mistty--with-live-buffer buf
    (setf (mistty--queue-timer queue) nil)
    (mistty--dequeue queue value)))

(defun mistty--interact-next (interact &optional val)
  "Return the next value from INTERACT.

This passes VAL to `mistty-interact-cb'."
  (with-current-buffer (mistty--interact-buf interact)
    (prog1
        (catch 'mistty--interact-return
          (let ((ret (funcall (mistty--interact-cb interact) val)))
            (error "Unexpected return value from %s: %s"
                   (mistty--interact-type interact) ret)))
      (setf (mistty--interact-buf interact) (current-buffer)))))

(defun mistty--interact-close (interact)
  "Close INTERACT, releasing any resource it helds.

After this call, `mistty--interact-next' fails and
`mistty--interact-close' is a no-op."
  (setf (mistty--interact-cb interact)
        (lambda (&optional _)
          (error "Interaction was closed")))
  (when-let ((func (mistty--interact-cleanup interact)))
    (setf (mistty--interact-cleanup interact) nil)
    (with-current-buffer (mistty--interact-initial-buf interact)
      (funcall func))))

(defun mistty--interact-wrap-accept (accept-f)
  "Decorates a condition for accepting a change.

This is a helper for building `mistty--interact' instances. It wraps
around ACCEPT-F, which should be a function that returns non-nil once
the state of the output is acceptable.

The wrapped function takes into account timeouts, intermediate
states and stable states; it knows when to give up and return
even though ACCEPT-F still doesn't return non-nil.

It returns non-nil once interaction should continue:
 - ==\'accept if the state is correct
 - ==\'give-up if the state is incorrect but too much time has
   passed
 - nil if the caller should keep waiting."
  (let ((try-count 0))
    (when (and mistty--report-issue-function (funcall accept-f))
      (funcall mistty--report-issue-function 'already-matching))
    (lambda (res)
      (cond
       ((funcall accept-f) 'accept)

       ((and (eq res 'stable) (< try-count mistty-max-try-count))
        (cl-incf try-count)
        ;; keep waiting
        nil)

       ((eq res 'timeout)
        (when mistty--report-issue-function
          (funcall mistty--report-issue-function 'hard-timeout))
        'give-up)

       ((eq res 'stable)
        (when mistty--report-issue-function
          (funcall mistty--report-issue-function 'soft-timeout))
        'give-up)))))

(cl-defun mistty--interact-return
    (interact value &key (wait-until nil) (then nil) (else nil))
  "Convenience function for returning a value from INTERACT.

This function optionally sets up the callback with THEN, a
condition to wait on with WAIT-UNTIL, then returns VALUE, a
string to be sent to the process.

WAIT-UNTIL must be a function that returns non-nil once the
condition is met. It is wrapped with
`mistty--interact-wrap-accept' before it is used.

after sending that value. Note that there is no guarantee that
the CB is a function to call once the terminal has been updated,
terminal contains the effect of sending that value.

If ELSE is set, and the interact times out waiting for WAIT-UNTIL to
become non-nil, call ELSE instead of THEN. ELSE is ignored if WAIT-UNTIL
is unset.

Note that it makes no sense to return \\='done as a VALUE using
this function, as CB would never be executed; Just return
\\='done directly."
  (cond
   (wait-until
    (let ((accept-f (mistty--interact-wrap-accept wait-until))
          (then (or then (mistty--interact-cb interact))))
      (setf (mistty--interact-cb interact)
            (lambda (value)
              (pcase (funcall accept-f value)
                ('accept (funcall then))
                ('give-up (funcall (or else then)))
                (_ (mistty--interact-keep-waiting)))))))
   (then (setf (mistty--interact-cb interact) then)))
  (throw 'mistty--interact-return value))

(defun mistty--call-interact (interact type &rest args)
  "Call INTERACT and pass it ARGS.

Fails if INTERACT is not of of type TYPE.

This function checks the type of INTERACT, checks that it is
 callable then call it with the arguments ARGS."
  (if (and (eq type (mistty--interact-type interact))
           (mistty--interact-callable-p interact))
      (apply (mistty--interact-call interact) args)
    (error "Interact %s is not callable as a %s"
           (mistty--interact-type interact) type)))

(provide 'mistty-queue)

;;; mistty-queue.el ends here
