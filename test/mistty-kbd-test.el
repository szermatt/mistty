;;; Tests mistty-kbd.el -*- lexical-binding: t -*-

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

(require 'mistty-kbd)
(require 'mistty-term)
(require 'ert)
(require 'ert-x)

(defvar mistty-test-key-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<backspace>") "\e[3~")
    (define-key map (kbd "<delete>") "\C-d")
    (define-key map (kbd "<down>") "\eOB")
    (define-key map (kbd "<end>") "\eOF")
    (define-key map (kbd "<escape>") "\e")
    (define-key map (kbd "<left>") "\eOD")
    (define-key map (kbd "<menu>") "\e[29~")
    (define-key map (kbd "<next>") "\e[6~")
    (define-key map (kbd "<prior>") "\e[5~")
    (define-key map (kbd "<return>") "\C-m")
    (define-key map (kbd "<right>") "\eOC")
    (define-key map (kbd "<tab>") "\t")
    (define-key map (kbd "<up>") "\eOA")
    (define-key map (kbd "S-<tab>") "\e[9;2u")
    map)
  "A limited keymap used in this file for testing.")

(defun mistty-test-translate-key (key n)
  (mistty--translate-key-default key n mistty-test-key-map))

(ert-deftest mistty-kbd-translate-key ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal "a" (mistty-translate-key (kbd "a") 1)))
    (should (equal "aaa" (mistty-translate-key (kbd "a") 3)))

    (should (equal "\C-a" (mistty-translate-key (kbd "C-a") 1)))

    (should (equal "\ea" (mistty-translate-key (kbd "M-a") 1)))
    (should (equal "\ea\ea\ea" (mistty-translate-key (kbd "M-a") 3)))
    (should (equal "\ea" (mistty-translate-key (kbd "\ea") 1)))
    (should (equal "\ea\ea" (mistty-translate-key (kbd "\ea") 2)))

    (should (equal "\eOD" (mistty-translate-key (kbd "<left>") 1)))
    (should (equal "\eOC" (mistty-translate-key (kbd "<right>") 1)))
    (should (equal "\eOA" (mistty-translate-key (kbd "<up>") 1)))
    (should (equal "\eOB" (mistty-translate-key (kbd "<down>") 1)))))

(ert-deftest mistty-kbd-translate-key-escape ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal "\e" (mistty-translate-key (kbd "<escape>"))))
    (should (equal "\e" (mistty-translate-key (kbd "ESC"))))))

(ert-deftest mistty-kbd-translate-key-return ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal "\C-m" (mistty-translate-key (kbd "<return>"))))
    (should (equal "\C-m" (mistty-translate-key (kbd "RET"))))))

(ert-deftest mistty-kbd-translate-key-tab ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal "\t" (mistty-translate-key (kbd "<tab>"))))
    (should (equal "\t" (mistty-translate-key (kbd "TAB"))))))

(ert-deftest mistty-kbd-translate-key-backspace ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal "\e[3~" (mistty-translate-key (kbd "<backspace>"))))
    (should (equal "\e[3~" (mistty-translate-key (kbd "DEL"))))))

(ert-deftest mistty-kbd-translate-key-delete ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal (mistty-translate-key (kbd "C-d"))
                   (mistty-translate-key (kbd "<delete>"))))))

(ert-deftest mistty-kbd-translate-key-backtab ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (should (equal (mistty-translate-key (kbd "S-<tab>"))
                   (mistty-translate-key (kbd "<backtab>"))))))

(ert-deftest mistty-kbd-key-override ()
  (let ((mistty--translate-key-function #'mistty-test-translate-key))
    (let* ((map (copy-keymap mistty-term-key-map))
           (mistty-term-key-map map))
      (should (equal "\ea" (mistty-translate-key (kbd "M-a") 1)))

      ;; override the default translation of M-a to \ea by adding it to the map
      (define-key map (kbd "M-a") "foobar")
      (should (equal "foobar" (mistty-translate-key (kbd "M-a") 1))))))

(ert-deftest mistty-kbd-capture-keyboard-hooks  ()
  (ert-with-test-buffer ()
    (let ((buf (current-buffer))
          (events nil)
          (mistty--translate-key-function #'mistty-test-translate-key)
          (mistty-start-capture-keyboard-hook nil)
          (mistty-end-capture-keyboard-hook nil))
      (add-hook 'mistty-start-capture-keyboard-hook
                (lambda ()
                  (push `(start ,mistty--capture-keyboard-active) events)))
      (add-hook 'mistty-end-capture-keyboard-hook
                (lambda ()
                  (push `(end ,mistty--capture-keyboard-active) events)))
      (setq mistty--send-function (lambda (str key &rest _)
                                    (push `(key ,key) events)
                                    (with-current-buffer buf
                                      (insert str))))
      (ert-simulate-keys '(?f ?o ?o ?\C-g)
        (mistty-capture-keyboard))
      (should (equal "foo" (buffer-string)))
      (should (equal '((start t)
                       (key [?f])
                       (key [?o])
                       (key [?o])
		       (end nil))
                     (nreverse events))))))

(ert-deftest mistty-kbd-capture-keyboard-mouse-event  ()
  (ert-with-test-buffer ()
    (let ((buf (current-buffer)))
      (setq mistty--send-function (lambda (str _key &rest _)
                                    (with-current-buffer buf
                                      (insert str))))
      ;; down-mouse-1 must end the sequence and bar should never
      ;; actually be typed
      (ert-simulate-keys '(?f ?o ?o down-mouse-1 ?b ?a ?r ?\C-g)
        (mistty-capture-keyboard))
      (should (equal "foo" (buffer-string))))))

(ert-deftest mistty-kbd-capture-keyboard-exit-key  ()
  (ert-with-test-buffer ()
    (let ((buf (current-buffer))
          (mistty-exit-capture-keyboard-key "<f7>"))
      (setq mistty--send-function (lambda (str _key &rest _)
                                    (with-current-buffer buf
                                      (insert str))))
      ;; C-g doesn't end the sequence, but f7 does
      (ert-simulate-keys (kbd "f o o C-g b a r <f7> q u x")
        (mistty-capture-keyboard))
      (should (equal "foo\7bar" (buffer-string))))))

(ert-deftest mistty-kbd-capture-keyboard-exit-key-invalid  ()
  (let ((mistty-exit-capture-keyboard-key "C-c t")
        (mistty--send-function #'ignore))
    (should-error (mistty-capture-keyboard) :type 'user-error)))
