;;; mistty-term.el --- Extensions for term.el for MisTTY -*- lexical-binding: t -*-

(require 'term)

(require 'mistty-util)

(defconst mistty-left-str "\eOD"
  "Sequence to send to the process when the left arrow is pressed.")

(defconst mistty-right-str "\eOC"
  "Sequence to send to the process when the rightarrow is pressed.")

(defvar-local mistty-bracketed-paste nil
  "Whether bracketed paste is enabled in the terminal.

This variable evaluates to true when bracketed paste is turned on
by the command that controls, to false otherwise.

This variable is available in the work buffer.")


(defvar-local mistty--term-properties-to-add-alist nil
  "An alist of id to text properties to add to the term buffer.

This variable associates arbitrary symbols to property lists. It
is set by `mistty-register-text-properties' and read whenever
text is written to the terminal.

This variable is available in the work buffer.")

(defun mistty-emulate-terminal (proc str work-buffer)
  "Handle special terminal codes, then call `term-emulate-terminal'.

This functions intercepts some extented sequences term.el. This
all should rightly be part of term.el."
  (cl-letf ((inhibit-modification-hooks nil)
            (start 0)
            ;; Using term-buffer-vertical-motion causes strange
            ;; issues; avoid it. Using mistty's window to compute
            ;; vertical motion is correct since the window dimension
            ;; are kept in sync with the terminal size. Falling back
            ;; to using the selected window, on the other hand, is
            ;; questionable.
            ((symbol-function 'term-buffer-vertical-motion)
             (lambda (count)
               (vertical-motion count (or (get-buffer-window work-buffer)
                                          (selected-window))))))
    (while (string-match "\e\\(\\[\\?\\(2004\\|25\\)[hl]\\|\\]\\(.*?\\)\\(\e\\\\\\|\a\\)\\)" str start)
      (let ((ext (match-string 1 str))
            (osc (match-string 3 str))
            (seq-start (match-beginning 0))
            (seq-end (match-end 0))
            (term-buffer (process-buffer proc)))
        (cond
         ((equal ext "[?2004h") ; enable bracketed paste
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer term-buffer
            (let ((props `(mistty-prompt-id ,(mistty--next-id))))
              ;; zsh enables bracketed paste only after having printed
              ;; the prompt.
              (unless (eq ?\n (char-before (point)))
                (add-text-properties (mistty--bol-pos-from (point)) (point) props))
              (mistty-register-text-properties 'mistty-bracketed-paste props)))
          (mistty--with-live-buffer work-buffer
            (setq mistty-bracketed-paste t)))
         ((equal ext "[?2004l") ; disable bracketed paste
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer term-buffer
            (mistty-unregister-text-properties 'mistty-bracketed-paste))
          (mistty--with-live-buffer work-buffer
            (setq mistty-bracketed-paste nil)))
         ((equal ext "[?25h") ; make cursor visible
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer work-buffer
            (setq cursor-type t)))
         ((equal ext "[?25l") ; make cursor invisible
          (term-emulate-terminal proc (substring str start seq-end))
          (mistty--with-live-buffer work-buffer
            (setq cursor-type nil)))
         (osc
          (term-emulate-terminal proc (substring str start seq-start))
          (mistty--with-live-buffer term-buffer
            (let ((inhibit-read-only t))
              (run-hook-with-args 'mistty-osc-hook osc)))))
          (setq start seq-end)))
    (let ((final-str (substring str start)))
      (unless (zerop (length final-str))
        (term-emulate-terminal proc final-str)))))

(defun mistty-register-text-properties (id props)
  (unless (eq 'term-mode major-mode) (error "requires a term-mode buffer"))
  (if-let ((cell (assq id mistty--term-properties-to-add-alist)))
      (setcdr cell props)
    (push (cons id props) mistty--term-properties-to-add-alist)))

(defun mistty-unregister-text-properties (id)
  (unless (eq 'term-mode major-mode) (error "requires a term-mode buffer"))
  (when-let ((cell (assq id mistty--term-properties-to-add-alist)))
    (setq mistty--term-properties-to-add-alist 
          (delq cell
                mistty--term-properties-to-add-alist))))

(defun mistty--create-term (name program args)
  (let ((term-buffer (generate-new-buffer name 'inhibit-buffer-hooks)))
    (with-current-buffer term-buffer
      (term-mode)
      (setq-local term-char-mode-buffer-read-only t
                  term-char-mode-point-at-process-mark t
                  term-buffer-maximum-size 0
                  term-height (or (floor (window-screen-lines)) 24)
                  term-width (or (window-max-chars-per-line) 80))
      (term--reset-scroll-region)
      (term-exec term-buffer (buffer-name term-buffer) program nil args)
      (term-char-mode)
      (add-hook 'after-change-functions #'mistty--after-change-on-term nil t))
    term-buffer))

(defun mistty--after-change-on-term (beg end _old-length)
  (when (and mistty--term-properties-to-add-alist (> end beg))
    (when-let ((props (apply #'append
                       (mapcar #'cdr mistty--term-properties-to-add-alist))))
      (add-text-properties beg end props))))


(defun mistty--maybe-bracketed-str (str)
  (let ((str (string-replace "\t" (make-string tab-width ? ) str)))
    (cond
     ((not mistty-bracketed-paste) str)
     ((not (string-match "[[:cntrl:]]" str)) str)
     (t (concat "\e[200~"
                str
                "\e[201~"
                mistty-left-str
                mistty-right-str)))))

(provide 'mistty-term)
