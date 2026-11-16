;;; Tests mistty-nav.el -*- lexical-binding: t -*-

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

(require 'ert)
(require 'ert-x)

(require 'mistty-nav)
(require 'mistty-prompt)
(require 'test/mistty-testing)

(mistty-deftest mistty-test-fish-right-prompt-command-for-output
    (:shell ((fish mistty-test-fish-right-prompt)) :type all)
  (dolist (text '("one" "two" "three" "four"))
    (mistty-send-text (concat "echo " text))
    (mistty-send-and-wait-for-prompt))

  (should (equal "echo four" (mistty--command-for-output
                              (mistty--prompt-ranges-for-current-or-previous-output 1))))
  (should (equal "echo three" (mistty--command-for-output
                               (mistty--prompt-ranges-for-current-or-previous-output 2))))
  (should (equal "echo two" (mistty--command-for-output
                             (mistty--prompt-ranges-for-current-or-previous-output 3))))
  (should (equal "echo one" (mistty--command-for-output
                             (mistty--prompt-ranges-for-current-or-previous-output 4)))))

(mistty-deftest mistty-test-zsh-right-prompt-command-for-output
    (:shell ((zsh mistty-test-zsh-right-prompt)) :type all)
  (dolist (text '("one" "two" "three" "four"))
    (mistty-send-text (concat "echo " text))
    (mistty-send-and-wait-for-prompt))

  (should (equal "echo four" (mistty--command-for-output
                              (mistty--prompt-ranges-for-current-or-previous-output 1))))
  (should (equal "echo three" (mistty--command-for-output
                               (mistty--prompt-ranges-for-current-or-previous-output 2))))
  (should (equal "echo two" (mistty--command-for-output
                             (mistty--prompt-ranges-for-current-or-previous-output 3))))
  (should (equal "echo one" (mistty--command-for-output
                             (mistty--prompt-ranges-for-current-or-previous-output 4)))))

(mistty-deftest mistty-osc133-command-for-output
    (:type all :shell ((zsh mistty-test-zsh-osc133
                            "function osc133_prompt { PS1='prompt \033]133;B\007'; };")))
  (mistty-send-text "osc133_prompt")
  (setq mistty-test-prompt-re "^prompt ")
  (mistty-send-and-wait-for-prompt)

  ;; The prompt really doesn't look like a prompt, but that doesn't
  ;; matter since it's marked with OSC133.
  (mistty-send-text "echo one")
  (mistty-send-and-wait-for-prompt)

  (mistty-send-text "echo two")
  (mistty-send-and-wait-for-prompt)

  (mistty-send-text "for i in a b c; do")
  (mistty-newline)
  (mistty-send-text "echo $i")
  (mistty-newline)
  (mistty-send-text "done")
  (mistty-send-and-wait-for-prompt)

  (should (equal "for i in a b c; do; echo $i; done"
                 (mistty--command-for-output
                  (mistty--prompt-ranges-for-current-or-previous-output 1))))
  (should (equal "echo two" (mistty--command-for-output
                             (mistty--prompt-ranges-for-current-or-previous-output 2))))
  (should (equal "echo one" (mistty--command-for-output
                             (mistty--prompt-ranges-for-current-or-previous-output 3)))))

(defconst mistty-test-zsh-osc133-cmdline "
function osc133_precmd() {
      printf '\\033]133;A\\007'
}
precmd_functions+=(osc133_precmd)

osc133_cmdline_url=''
function osc133_preexec() {
      if [ -n \"$osc133_cmdline_url\" ]; then
            printf '\\033]133;C;cmdline_url=%s\\007' \"$osc133_cmdline_url\"
      else
            printf '\\033]133;C\\007'
      fi
}
preexec_functions+=(osc133_preexec)
")

(mistty-deftest mistty-osc133-cmdline-url-command-for-output
    (:type alacritty :shell ((zsh mistty-test-zsh-osc133-cmdline)))
  ;; Send a percent-encoded cmdline_url that differs from the typed text
  ;; to verify it is decoded and preferred over `mistty--extract-cmdline'.
  (mistty-send-text "osc133_cmdline_url='reported%20cmd%20%22%E4%B8%96%E7%95%8C%22'")
  (mistty-send-and-wait-for-prompt)

  (mistty-send-text "echo one")
  (mistty-send-and-wait-for-prompt)

  ;; Without cmdline_url, it falls back to `mistty--extract-cmdline'.
  (mistty-send-text "osc133_cmdline_url=''")
  (mistty-send-and-wait-for-prompt)

  (mistty-send-text "echo two")
  (mistty-send-and-wait-for-prompt)

  (should (equal "echo two"
                 (mistty--command-for-output
                  (mistty--prompt-ranges-for-current-or-previous-output 1))))
  (should (equal "reported cmd \"世界\""
                 (mistty--command-for-output
                  (mistty--prompt-ranges-for-current-or-previous-output 2)))))


(mistty-deftest mistty-osc133-fish-cmdline (:type alacritty :shell fish)
  (mistty-send-text "for i in a b c")
  (mistty-send-command)
  (mistty-send-text "echo $i")
  (mistty-send-command)
  (mistty-send-text "end")
  (mistty-send-and-wait-for-prompt)

  (should (equal
           "for i in a b c; echo $i; end"
           (mistty--command-for-output
            (mistty--prompt-ranges-for-current-or-previous-output 1)))))


(mistty-deftest mistty-user-input-start (:type all)
  (let ((user-input-start
         (lambda ()
           (let ((p (mistty--prompt-ranges-around
                     (point)
                     (mistty--active-or-potential-prompt-ranges))))
             (mistty--user-input-start (mistty--prompt-ranges-prompt-start p)
                                       (mistty--prompt-ranges-output-start p))))))
    (mistty-send-text "echo one")
    (mistty-send-and-wait-for-prompt)
    (mistty-send-and-wait-for-prompt)
    (mistty-send-text "echo two")
    (mistty-send-and-wait-for-prompt)

    (should
     (equal
      "$ <>"
      (mistty-test-content
       :start (pos-bol)
       :end (pos-eol)
       :show (funcall user-input-start))))

    (mistty-test-goto "echo two")
    (should
     (equal
      "$ <>echo two"
      (mistty-test-content
       :start (pos-bol)
       :end (pos-eol)
       :show (funcall user-input-start))))

    (forward-line -1)
    (should
     (equal
      "$ <>"
      (mistty-test-content
       :start (pos-bol)
       :end (pos-eol)
       :show (funcall user-input-start))))

    (mistty-test-goto "echo one")
    (should
     (equal
      "$ <>echo one"
      (mistty-test-content
       :start (pos-bol)
       :end (pos-eol)
       :show (funcall user-input-start))))))

(mistty-deftest mistty-imenu-create-index (:type all)
  (let ((prompt-pos (make-vector 4 nil)))
    (cl-loop for index from 0
             for text across ["one" "two" "three"]
             do (progn
                  (aset prompt-pos index (point))
                  (mistty-send-text (format "echo %s" text))
                  (mistty-send-and-wait-for-prompt)))
    (aset prompt-pos 3 (point))
    (mistty-send-text "current")

    (let ((imenu-use-markers nil))
      (should (equal `(("Outputs"
                        ("echo three" . ,(mistty--bol (aref prompt-pos 2) 2))
                        ("echo two" . ,(mistty--bol (aref prompt-pos 1) 2))
                        ("echo one" . ,(mistty--bol (aref prompt-pos 0) 2)))
                       ("Commands"
                        ("CURRENT" . ,(aref prompt-pos 3))
                        ("echo three" . ,(aref prompt-pos 2))
                        ("echo two" . ,(aref prompt-pos 1))
                        ("echo one" . ,(aref prompt-pos 0))))
                     (mistty-imenu-create-index))))))

(mistty-deftest mistty-imenu-create-index-use-markers ( :type all)
  (let ((prompt-pos (make-vector 4 nil)))
    (cl-loop for index from 0
             for text across ["one" "two" "three"]
             do (progn
                  (aset prompt-pos index (point))
                  (mistty-send-text (format "echo %s" text))
                  (mistty-send-and-wait-for-prompt)))
    (aset prompt-pos 3 (point))
    (mistty-send-text "current")

    (let* ((imenu-use-markers t)
           (index (mistty-imenu-create-index)))

      ;; All positions are markers
      (dolist (group-cell index)
        (dolist (entry-cell (cdr group-cell))
          (should (markerp (cdr entry-cell)))))

      ;; Check marker positions
      (dolist (group-cell index)
        (mapc (lambda (cell)
                (setcdr cell (marker-position (cdr cell))))
              (cdr group-cell)))

      (should (equal `(("Outputs"
                        ("echo three" . ,(mistty--bol (aref prompt-pos 2) 2))
                        ("echo two" . ,(mistty--bol (aref prompt-pos 1) 2))
                        ("echo one" . ,(mistty--bol (aref prompt-pos 0) 2)))
                       ("Commands"
                        ("CURRENT" . ,(aref prompt-pos 3))
                        ("echo three" . ,(aref prompt-pos 2))
                        ("echo two" . ,(aref prompt-pos 1))
                        ("echo one" . ,(aref prompt-pos 0))))
                     index)))))

(mistty-deftest mistty-imenu-create-index-no-output ( :type all)
  (mistty-send-text "echo one")
  (mistty-send-and-wait-for-prompt)
  (mistty-send-text "echo -n")
  (mistty-send-and-wait-for-prompt)

  (let ((imenu-use-markers nil)
        (index (mistty-imenu-create-index)))
    (should (equal '("CURRENT" "echo -n" "echo one")
                   (mapcar #'car
                           (alist-get "Commands" index nil nil #'string=))))

    ;; Neither CURRENT nor echo -n has any outputs.
    (should (equal '("echo one")
                   (mapcar #'car
                           (alist-get "Outputs" index nil nil #'string=))))))

(mistty-deftest mistty-imenu-create-index-multiline ( :type all)
  (mistty-send-text "for i in a b c; do")
  (mistty-newline)
  (mistty-send-text " echo $i")
  (mistty-newline)
  (mistty-send-text "done")
  (mistty-send-and-wait-for-prompt)

  (let ((imenu-use-markers nil)
        (index (mistty-imenu-create-index)))
    (should (equal '("CURRENT" "for i in a b c; do; echo $i; done")
                   (mapcar #'car
                           (alist-get "Commands" index nil nil #'string=))))))

(mistty-deftest mistty-test-select-output (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo two"))
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo three"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (mistty-select-output)
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$\n"
                         "$ echo three\n"
                         "<>three\n<1>"
                         "$ echo current")
                 (mistty-test-content :show (list (point) (mark)))))

  (mistty-test-goto "echo two")
  (forward-line 1)
  (mistty-select-output)
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "<>two\n<1>"
                         "$\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo current")
                 (mistty-test-content :show (list (point) (mark)))))

  (goto-char (1+ (point)))
  (mistty-select-output)
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "<>two\n<1>"
                         "$\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo current")
                 (mistty-test-content :show (list (point) (mark)))))

  (mistty-select-output 1)
  (should (equal (concat "$ echo one\n"
                         "<>one\n<1>"
                         "$ echo two\n"
                         "two\n"
                         "$\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo current")
                 (mistty-test-content :show (list (point) (mark)))))

  (mistty-test-goto "current")
  (mistty-select-output 2)
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "<>two\n<1>"
                         "$\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo current")
                 (mistty-test-content :show (list (point) (mark)))))

  (mistty-test-goto "echo one")
  (mistty-select-output)
  (should (equal (concat "$ echo one\n"
                         "<>one\n<1>"
                         "$ echo two\n"
                         "two\n"
                         "$\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo current")
                 (mistty-test-content :show (list (point) (mark))))))

(mistty-deftest mistty-test-select-output-eob (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (goto-char (point-max))

  (mistty-select-output)
  (equal (concat "$ echo one\n"
                 "<>one\n<1>"
                 "$ echo current")
         (mistty-test-content :show (list (point) (mark)))))

(mistty-deftest mistty-test-select-output-bob (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (mistty-test-goto "echo one")
  (forward-line)
  (delete-region (point-min) (point))

  (should-error (mistty-select-output))

  (mistty-test-goto "echo current")
  (should-error (mistty-select-output)))

(mistty-deftest mistty-test-next-output-bob (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo two"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo three"))

  (mistty-test-goto "echo one")
  (forward-line)
  (delete-region (point-min) (point))

  (goto-char (point-min))
  (mistty-next-output 1)
  (should (equal "two" (mistty-test-content :start (pos-bol) :end (pos-eol)))))

(mistty-deftest mistty-test-next-input (:shell (bash zsh fish))
  (mistty-with-test-buffer ()
    (mistty-run-command
     (insert "echo one"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo two"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo three"))
    (mistty-send-and-wait-for-prompt)
    (mistty-run-command
     (insert "echo current"))

    (goto-char (point-min))
    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ <>echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ <>echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-next-input 1)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ <>echo current")
            (mistty-test-content :show (point))))

    (should-error (mistty-next-input 1))
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ <>echo current")
            (mistty-test-content :show (point))))

    (goto-char (point-min))
    (mistty-next-input 2)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ <>echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (goto-char (point-min))
    (mistty-next-input 3)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$ echo three\n"
                    "three\n"
                    "$ <>echo current")
            (mistty-test-content :show (point))))))

(mistty-deftest mistty-test-previous-input (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo two"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo three"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (mistty-test-goto "current")

  (mistty-previous-input 1)
  (should
   (equal (concat "$ echo one\n"
                  "one\n"
                  "$ echo two\n"
                  "two\n"
                  "$ <>echo three\n"
                  "three\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-previous-input 1)
  (should
   (equal (concat "$ echo one\n"
                  "one\n"
                  "$ <>echo two\n"
                  "two\n"
                  "$ echo three\n"
                  "three\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-previous-input 1)
  (should
   (equal (concat "$ <>echo one\n"
                  "one\n"
                  "$ echo two\n"
                  "two\n"
                  "$ echo three\n"
                  "three\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (should-error (mistty-previous-input 1))
  (should
   (equal (concat "$ <>echo one\n"
                  "one\n"
                  "$ echo two\n"
                  "two\n"
                  "$ echo three\n"
                  "three\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-test-goto "current")
  (mistty-previous-input 2)
  (should
   (equal (concat "$ echo one\n"
                  "one\n"
                  "$ <>echo two\n"
                  "two\n"
                  "$ echo three\n"
                  "three\n"
                  "$ echo current")
          (mistty-test-content :show (point)))))

(mistty-deftest mistty-test-next-input-empty-prompt (:type all)
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (goto-char (point-min))
  (mistty-next-input 1)
  (should
   (equal (concat "$\n"
                  "$ <>\n"
                  "$\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-next-input 1)
  (should
   (equal (concat "$\n"
                  "$\n"
                  "$ <>\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-next-input 1)
  (should
   (equal (concat "$\n"
                  "$\n"
                  "$\n"
                  "$ <>echo current")
          (mistty-test-content :show (point))))

  (should-error (mistty-next-input 1))

  (goto-char (point-min))
  (mistty-next-input 2)
  (should
   (equal (concat "$\n"
                  "$\n"
                  "$ <>\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (goto-char (point-min))
  (mistty-next-input 3)
  (should
   (equal (concat "$\n"
                  "$\n"
                  "$\n"
                  "$ <>echo current")
          (mistty-test-content :show (point)))))

(mistty-deftest mistty-test-previous-input-empty-prompt (:type all)
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (mistty-test-goto "current")
  (mistty-previous-input 1)
  (should
   (equal (concat "$\n"
                  "$\n"
                  "$ <>\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-previous-input 1)
  (should
   (equal (concat "$\n"
                  "$ <>\n"
                  "$\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (mistty-previous-input 1)
  (should
   (equal (concat "$ <>\n"
                  "$\n"
                  "$\n"
                  "$ echo current")
          (mistty-test-content :show (point))))

  (should-error (mistty-previous-input 1))

  (mistty-test-goto "current")
  (mistty-previous-input 2)
  (should
   (equal (concat "$\n"
                  "$ <>\n"
                  "$\n"
                  "$ echo current")
          (mistty-test-content :show (point)))))

(mistty-deftest mistty-test-next-input-nobracketed-paste
    (:shell ((bash mistty-test-nobracketed-paste-setup)) :type all)
  (mistty-run-command (insert "echo $((1 + 1))"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command (insert "echo $((2 + 2))"))
  (goto-char (point-min))
  (mistty-next-input 1)
  (should (equal (concat "$ echo $((1 + 1))\n"
                         "2\n"
                         "$ <>echo $((2 + 2))")
                 (mistty-test-content :show (point))))
  (mistty-previous-input 1)
  (should (equal (concat "$ <>echo $((1 + 1))\n"
                         "2\n"
                         "$ echo $((2 + 2))")
                 (mistty-test-content :show (point)))))

(mistty-deftest mistty-test-next-output (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo two"))
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo three"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (let ((range))
    (goto-char (point-min))
    (setq range (mistty-next-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "<>one\n<1>"
                    "$ echo two\n"
                    "two\n"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (list (point) (cdr range)))))
    (should (equal (point) (car range)))

    (setq range (mistty-next-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "<>two\n<1>"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (list (point) (cdr range)))))
    (should (equal (point) (car range)))

    (setq range (mistty-next-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$\n"
                    "$ echo three\n"
                    "<>three\n<1>"
                    "$ echo current")
            (mistty-test-content :show (list (point) (cdr range)))))
    (should (equal (point) (car range)))

    (should-error (mistty-next-output 1))

    (goto-char (point-min))
    (mistty-next-output 2)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "<>two\n"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (goto-char (point-min))
    (mistty-next-output 3)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$\n"
                    "$ echo three\n"
                    "<>three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))))

(mistty-deftest mistty-test-previous-output (:type all)
  (mistty-run-command
   (insert "echo one"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo two"))
  (mistty-send-and-wait-for-prompt)
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo three"))
  (mistty-send-and-wait-for-prompt)
  (mistty-run-command
   (insert "echo current"))

  (mistty-test-goto "current")

  (let (range)

    (setq range (mistty-previous-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "two\n"
                    "$\n"
                    "$ echo three\n"
                    "<>three\n<1>"
                    "$ echo current")
            (mistty-test-content :show (list (point) (cdr range)))))
    (should (equal (point) (car range)))

    (setq range (mistty-previous-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "<>two\n<1>"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (list (point) (cdr range)))))
    (should (equal (point) (car range)))

    (setq range (mistty-previous-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "<>one\n<1>"
                    "$ echo two\n"
                    "two\n"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (list (point) (cdr range)))))
    (should (equal (point) (car range)))

    (should-error (mistty-previous-output 1))
    (should
     (equal (concat "$ echo one\n"
                    "<>one\n"
                    "$ echo two\n"
                    "two\n"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))

    (mistty-test-goto "current")
    (mistty-previous-output 2)
    (should
     (equal (concat "$ echo one\n"
                    "one\n"
                    "$ echo two\n"
                    "<>two\n"
                    "$\n"
                    "$ echo three\n"
                    "three\n"
                    "$ echo current")
            (mistty-test-content :show (point))))))

;; https://github.com/szermatt/mistty/issues/33
(mistty-deftest mistty-test-previous-output-zsh (:shell (bash zsh fish))
  (ert-with-temp-file tempfile
    (with-temp-file tempfile
      (dotimes (n 200)
        (insert (format "line %d\n" n))))
    (mistty--send-string mistty-proc (format "cat '%s'" tempfile))
    (mistty-send-and-wait-for-prompt
     :send (lambda ()
             (mistty-run-command
              (mistty-send-command))))
    (mistty-previous-output 1)
    (should (equal
             "<>line 0\nline 1\nline 2"
             (mistty-test-content :show (point)
                                  :start (pos-bol)
                                  :end (pos-eol 3))))))


(mistty-deftest mistty-test-create-buffer-with-prev-output ( :type all)
  (mistty-send-text "printf '#!/bin/bash\\necho ok\\n'")
  (mistty-send-and-wait-for-prompt)
  (let ((newbuf (ert-simulate-keys
                    (kbd "RET")
                  (call-interactively 'mistty-create-buffer-with-output))))
    (unwind-protect
        (with-current-buffer newbuf
          (should (equal "#!/bin/bash\necho ok\n"
                         (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal "printf '#!/bin/bash\\necho ok\\n..."
                         (buffer-name)))
          ;; mode should be recognized thanks to the shebang
          (should (equal 'sh-mode major-mode)))
      (kill-buffer newbuf))))

(mistty-deftest mistty-test-create-buffer-with-prev-n-output ( :type all)
  (mistty-send-text "echo hello")
  (mistty-send-and-wait-for-prompt)
  (mistty-send-text "echo world")
  (mistty-send-and-wait-for-prompt)
  (let* ((current-prefix-arg 2)
         (newbuf (ert-simulate-keys
                     (kbd "RET")
                   (call-interactively 'mistty-create-buffer-with-output))))
    (unwind-protect
        (with-current-buffer newbuf
          (should (equal "hello\n"
                         (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal "echo hello"
                         (buffer-name))))
      (kill-buffer newbuf))))

(mistty-deftest mistty-test-create-buffer-with-prev-output-multiline ( :type all)
  (mistty-run-command
   (insert "for i in hello world; do\n echo $i\n done"))
  (mistty-send-and-wait-for-prompt)
  (let ((newbuf (ert-simulate-keys
                    (kbd "RET")
                  (call-interactively 'mistty-create-buffer-with-output))))
    (unwind-protect
        (with-current-buffer newbuf
          (should (equal "hello\nworld\n"
                         (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal "for i in hello world; do; echo..." (buffer-name))))
      (kill-buffer newbuf))))

(mistty-deftest mistty-test-create-buffer-with-prev-output-single-line ( :type all)
  (mistty-run-command
   (insert "for i in hello world; do echo $i; done"))
  (mistty-send-and-wait-for-prompt)
  (let ((newbuf (ert-simulate-keys
                    (kbd "RET")
                  (call-interactively 'mistty-create-buffer-with-output))))
    (unwind-protect
        (with-current-buffer newbuf
          (should (equal "hello\nworld\n"
                         (buffer-substring-no-properties (point-min) (point-max))))
          (should (equal "for i in hello world; do echo ..." (buffer-name))))
      (kill-buffer newbuf))))

(mistty-deftest mistty-test-create-buffer-with-current-output ( :type all)
  (mistty-send-text "echo content")
  (mistty-send-and-wait-for-prompt)
  (mistty-test-goto "echo content")
  (forward-line)
  (let ((newbuf (ert-simulate-keys
                    (kbd "RET")
                  (call-interactively 'mistty-create-buffer-with-output))))
    (unwind-protect
        (with-current-buffer newbuf
          (should (equal "content" (mistty-test-content)))
          (should (equal "echo content" (buffer-name))))
      (kill-buffer newbuf))))

(mistty-deftest mistty-test-zsh-multiline-prompt-next-previous (:shell ((zsh mistty-test-zsh-fancy-prompt)) :type all)
  (dolist (text '("one" "two" "three"))
    (mistty-send-text (concat "echo " text))
    (mistty-send-and-wait-for-prompt))

  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "one\n"
            "left...............................right\n"
            "$ echo two\n"
            "two\n"
            "left...............................right\n"
            "$ echo three\n"
            "three\n"
            "left...............................right\n"
            "$ <>")
    (mistty-test-content :show (point))))

  (mistty-previous-output 1)
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "one\n"
            "left...............................right\n"
            "$ echo two\n"
            "two\n"
            "left...............................right\n"
            "$ echo three\n"
            "<>three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point))))

  (mistty-previous-output 1)
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "one\n"
            "left...............................right\n"
            "$ echo two\n"
            "<>two\n"
            "left...............................right\n"
            "$ echo three\n"
            "three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point))))

  (mistty-previous-output 1)
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "<>one\n"
            "left...............................right\n"
            "$ echo two\n"
            "two\n"
            "left...............................right\n"
            "$ echo three\n"
            "three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point))))

  (should-error (mistty-previous-output 1))
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "<>one\n"
            "left...............................right\n"
            "$ echo two\n"
            "two\n"
            "left...............................right\n"
            "$ echo three\n"
            "three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point))))

  (mistty-next-output 1)
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "one\n"
            "left...............................right\n"
            "$ echo two\n"
            "<>two\n"
            "left...............................right\n"
            "$ echo three\n"
            "three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point))))


  (mistty-next-output 1)
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "one\n"
            "left...............................right\n"
            "$ echo two\n"
            "two\n"
            "left...............................right\n"
            "$ echo three\n"
            "<>three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point))))

  (should-error (mistty-next-output 1))
  (should
   (equal
    (concat "left...............................right\n"
            "$ echo one\n"
            "one\n"
            "left...............................right\n"
            "$ echo two\n"
            "two\n"
            "left...............................right\n"
            "$ echo three\n"
            "<>three\n"
            "left...............................right\n"
            "$")
    (mistty-test-content :show (point)))))

(mistty-deftest mistty-test-zsh-multiline-prompt-extract-output (:shell ((zsh mistty-test-zsh-fancy-prompt)) :type all)
  (dolist (text '("one" "two" "three" "four"))
    (mistty-send-text (concat "echo " text))
    (mistty-send-and-wait-for-prompt))

  (let ((buf (mistty-create-buffer-with-output
              (generate-new-buffer-name "extracted") 2)))
    (unwind-protect
        (should (equal "three\n"
                       (with-current-buffer buf
                         (buffer-string))))
      (kill-buffer buf))))

(mistty-deftest mistty-prev-next-prompts-as-defuns ( :type all)
  (dolist (text '("one" "two" "three" "four" "five"))
    (mistty-send-text (format "echo %s" text))
    (mistty-send-and-wait-for-prompt))
  (mistty-send-text "current")
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "$ current<>")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(beginning-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "<>$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(beginning-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "<>$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(beginning-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "<>$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(beginning-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "<>$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(end-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "<>$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(end-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "<>$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(end-of-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "<>$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(beginning-of-defun 4))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "<>$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point))))

  (ert-simulate-command '(beginning-of-defun -1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "$ echo two\n"
                         "two\n"
                         "$ echo three\n"
                         "three\n"
                         "<>$ echo four\n"
                         "four\n"
                         "$ echo five\n"
                         "five\n"
                         "$ current")
                 (mistty-test-content :show (point)))))

(mistty-deftest mistty-mark-prompt-as-defun ( :type all)
  (dolist (text '("one" "two" "three"))
    (mistty-send-text (format "echo %s" text))
    (mistty-send-and-wait-for-prompt))
  (mistty-send-text "current")

  (goto-char (point-min))
  (mistty-test-goto "echo two")

  (ert-simulate-command '(mark-defun 1))
  (should (equal (concat "$ echo one\n"
                         "one\n"
                         "<>$ echo two\n"
                         "two\n<1>"
                         "$ echo three\n"
                         "three\n"
                         "$ current")
                 (mistty-test-content :show (list (point) (mark))))))

(mistty-deftest mistty-narrow-to-prompt-as-defun ( :type all)
  (dolist (text '("one" "two" "three"))
    (mistty-send-text (format "echo %s" text))
    (mistty-send-and-wait-for-prompt))
  (mistty-send-text "current")

  (goto-char (point-min))
  (mistty-test-goto "echo two")

  (narrow-to-defun)

  (ert-simulate-command '(mark-defun 1))
  (should (equal "$ echo two\ntwo"
                 (mistty-test-content))))
