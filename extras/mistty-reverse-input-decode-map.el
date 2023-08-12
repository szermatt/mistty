(require 'seq)
(require 'help-fns)

(defun mistty-reverse-input-decode-map (map)
  "Generate elisp code for `mistty-term-key-map' given a MAP.

This command reverses input-decode maps, such as the ones defined
in elisp/term/ and outputs a definition that's appropriate to use
as `mistty-term-key-map' into a buffer.

You might find it useful if you'd like MisTTY to generate a set
of keys from a different terminal than xterm."
  (interactive
   (let ((def (variable-at-point))
	 (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
	 accept choice)
     (setq accept
           (lambda (sym)
             (let ((resolved (and (symbolp sym)
                                  (boundp sym)
                                  (or (buffer-local-value sym orig-buffer)
                                      (symbol-value sym)))))
               (if (keymapp resolved) resolved))))
     (setq choice
           (completing-read 
            (format-prompt "Reverse map " (funcall accept def))
            #'help--symbol-completion-table
            accept
            t nil nil
            (if (funcall accept def) (symbol-name def))))
     (list (if (equal choice "")
	       choice (funcall accept (intern choice))))))
  (with-current-buffer (get-buffer-create "*mistty-reverse-map*")
    (delete-region (point-min) (point-max))
    (emacs-lisp-mode)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (insert "(let ((map (make-sparse-keymap)))\n")
    (let ((start (point)))
      (map-keymap
       (lambda (e b)
         (mistty--reverse-input-decode-map-1 e b [] (make-sparse-keymap)))
       map)
      (sort-lines nil start (point)))
    (insert "\n    map)\n")
    (goto-char (point-min))))

(defun mistty--reverse-input-decode-map-1 (event-type binding prefix exists-map)
  (let ((full-event (vconcat prefix (vector event-type))))
    (cond
     ((keymapp binding)
      (map-keymap
       (lambda (e b)
         (mistty--reverse-input-decode-map-1 e b full-event exists-map))
       binding))
     ((functionp binding))
     ((sequencep binding) ; a key seq
      (unless (eq 'exists (lookup-key exists-map binding))
        (define-key exists-map binding 'exists)
        (insert (format
                "    (define-key map (kbd %S) %S)\n"
                (key-description binding)
                (seq-mapcat 'text-char-description full-event 'string))))))))

(provide 'mistty-reverse-map)
