;;; Tests the module mistty-alacritty-vt -*- lexical-binding: t -*-

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
(require 'mistty-alacritty) ; loads mistty-alacritty-vt
(require 'mistty-testing)
(require 'turtles)

(ert-deftest mistty-alacritty-vt-process-bytes ()
  (let ((term (mistty-alacritty-vt-make-vterm 80 10)))
    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))
    (should (equal "0\n1\n2\n3\n4\n5\n6\n7\n8\n9" (mistty-alacritty-vt-display-string term)))

    ;; scroll
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n10"))
    (should (equal "1\n2\n3\n4\n5\n6\n7\n8\n9\n10" (mistty-alacritty-vt-display-string term)))))

(ert-deftest mistty-alacritty-vt-render ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0 "))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))
    (ert-with-test-buffer ()
        (insert "---\n") ; before point; will not be overwritten
        (save-excursion
          (insert "===\n")) ; after point; will be overwritten
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "---\n"
           "0 \n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9<>\n")
          (mistty-test-content :trim nil :show cursor))))))

(ert-deftest mistty-alacritty-vt-render-to-last-nonblank ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 5)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        ;; render only up to the last written line
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5<>\n")
          (mistty-test-content :trim nil :show cursor)))

        ;; The cursor goes past the last written line, which should
        ;; now be rendered.
        (mistty-alacritty-vt-process-bytes term (vconcat "\e[3B"))
        (goto-char (point-min))
        (mistty-alacritty-vt-render-damaged term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "\n"
           "\n"
           " <>\n")
          (mistty-test-content :trim nil :show cursor)))

        ;; The cursor comes back up, so blank lines disappear.
        (mistty-alacritty-vt-process-bytes term (vconcat "\e[3A"))
        (goto-char (point-min))
        (mistty-alacritty-vt-render-damaged term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5<>\n")
          (mistty-test-content :trim nil :show cursor)))

        ;; The number of written lines shrink
        (mistty-alacritty-vt-process-bytes term (vconcat "\e[2A\e[0J"))
        (goto-char (point-min))
        (mistty-alacritty-vt-render-damaged term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3<>\n")
          (mistty-test-content :trim nil :show cursor)))

        ;; The number of written lines expands
        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n4\r\n5\r\n6"))
        (goto-char (point-min))
        (mistty-alacritty-vt-render-damaged term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6<>\n")
          (mistty-test-content :trim nil :show cursor)))))))

(ert-deftest mistty-alacritty-vt-set-cursor ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))
    (goto-char (point-min))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9<>\n")
          (mistty-test-content :trim nil :show cursor)))

        ;; move cursor 3 lines up, 2 columns right
        (mistty-alacritty-vt-process-bytes term (vconcat "\e[3A\e[2C"))
        (goto-char (point-min))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6  <>\n"
           "7\n"
           "8\n"
           "9\n")
          (mistty-test-content
           :trim nil
           :show cursor)))))))

(turtles-ert-deftest mistty-alacritty-vt-set-color (:instance 'mistty)
 (ert-with-test-buffer ()
   (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[31mred\e[0m, \e[37m\e[42mgreen\e[0m, \e[34mblue\e[0m."))
    (mistty-alacritty-vt-render term (make-marker))
    (turtles-with-grab-buffer ()
      (goto-char (point-min))
      (should
       (equal
        "red, green, blue."
        (buffer-substring-no-properties (pos-bol) (pos-eol))))

      (mistty-test-goto "red")
      (should
       (equal (mistty-colors-at-point)
              (mistty-face-colors 'ansi-color-red 'default)
              ))

      (mistty-test-goto ",")
      (should
       (equal (mistty-colors-at-point)
              (mistty-face-colors 'default)))

      (mistty-test-goto "green")
      (should
       (equal (mistty-colors-at-point)
              (mistty-face-colors 'ansi-color-white 'ansi-color-green)))

      (mistty-test-goto "blue")
      (should
       (equal (mistty-colors-at-point)
              (mistty-face-colors 'ansi-color-blue 'default)))))))

(turtles-ert-deftest mistty-alacritty-vt-set-24bit-color (:instance 'mistty)
 (ert-with-test-buffer ()
   (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes
     term (vconcat "\e[38;2;237;237;216m\e[48;2;97;35;196mcolorful\e[0m!"))
    (goto-char (point-min))
    (mistty-alacritty-vt-render term (make-marker))
    (turtles-with-grab-buffer ()
      (goto-char (point-min))
      (should
       (equal
        "colorful!"
        (buffer-substring-no-properties (pos-bol) (pos-eol))))

      (mistty-test-goto "colorful")
      (should
       (equal (mistty-colors-at-point)
              '("#ededd8" "#6123c4")))))))


(ert-deftest mistty-alacritty-vt-set-face ()
 (ert-with-test-buffer ()
   (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[1mbold, \e[3mitalic\e[0m,\r\n\e[4munderline\e[0m,\r\n\e[7minverse\e[0m."))
    (mistty-alacritty-vt-render term (make-marker))

    (goto-char (point-min))
    (should
     (equal
      "bold, italic,\nunderline,\ninverse."
      (mistty-test-content)))

    (mistty-test-goto "bold")
    (should (equal 'ansi-color-bold (get-text-property (point) 'face)))

    (mistty-test-goto "italic")
    (should (equal (sort '(ansi-color-bold ansi-color-italic) #'value<)
                   (sort (get-text-property (point) 'face) #'value<)))

    (mistty-test-goto "underline")
    (should (equal 'ansi-color-underline (get-text-property (point) 'face)))

    (mistty-test-goto "inverse")
    (should (equal 'ansi-color-inverse (get-text-property (point) 'face))))))

(ert-deftest mistty-alacritty-vt-render-damaged-move-cursor ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))
    (goto-char (point-min))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9<>")
          (mistty-test-content :show cursor)))

        ;; move cursor 3 lines up, 2 columns right
        (mistty-alacritty-vt-process-bytes term (vconcat "\r\e[3A\e[2Cmodified\r\e[2A\e[2C"))
        (goto-char (point-min))
        (mistty-alacritty-vt-render-damaged term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4 <>\n"        ; not modified, but the cursor moved there
           "5\n"
           "6 modified\n"  ; modified
           "7\n"
           "8\n"
           "9")            ; not modified, but the cursor moved from there
          (mistty-test-content :show cursor)))))))

(ert-deftest mistty-alacritty-vt-pty-write ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; \e[6n queries the cursor position. ]
    (should (equal nil (mistty-alacritty-vt-process-bytes term (vconcat "foo\r\n"))))
    (should (equal
             '((pty-write "\33[2;4R"))
             (mistty-alacritty-vt-process-bytes term (vconcat "bar\e[6n\r\n"))))))

(ert-deftest mistty-alacritty-vt-render-unicode-wide-characters ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[1ma\e[0m\xF0\x9F\x9F\xA7\e[4msquare\e[0m!\r\n"))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        ;; Alacritty puts fake columns around wide chars to keep the column aligned. Make
        ;; sure these don't appear in the Emacs text.
        (should
         (equal
           "a\U0001F7E7square!"
          (mistty-test-content)))
        (should (equal "a" (mistty-alacritty-vt-display-substring term 0 0 0 0)))
        (should (equal "a\U0001F7E7" (mistty-alacritty-vt-display-substring term 0 0 0 1)))
        (should (equal "a\U0001F7E7" (mistty-alacritty-vt-display-substring term 0 0 0 2)))
        (should (equal "a\U0001F7E7s" (mistty-alacritty-vt-display-substring term 0 0 0 3)))

        ;; The following makes sure that the text properties are
        ;; applied to the right portion of the text, despite the
        ;; calculations being possibly thrown off by the fake columns.
        (should
         (equal
           "[a]\U0001F7E7square!"
          (mistty-test-content :show-property '(face ansi-color-bold))))
        (should
         (equal
           "a\U0001F7E7[square]!"
          (mistty-test-content :show-property '(face ansi-color-underline))))))))


(ert-deftest mistty-alacritty-vt-render-unicode-combining-characters ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[1mc'e\xcc\x81tait\e[0m \e[4ml'e\xcc\x81te\xcc\x81\e[0m!\r\n"))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        ;; The following makes sure that the text properties are
        ;; applied to the right portion of the text, despite the
        ;; calculations being possibly thrown off by and é (e\u0301)
        ;; counting as two characters in the emacs buffer, even though
        ;; it's displayed in a single column.
        (should
         (equal
           "[c'e\u0301tait] l'e\u0301te\u0301!"
          (mistty-test-content :show-property '(face ansi-color-bold))))
        (should
         (equal
           "c'e\u0301tait [l'e\u0301te\u0301]!"
          (mistty-test-content :show-property '(face ansi-color-underline))))))))

(ert-deftest mistty-alacritty-vt-render-unicode-zerowidth-characters ()
  (let ((term (mistty-alacritty-vt-make-vterm 80 10)))
    (mistty-alacritty-vt-process-bytes
     term (vconcat "https://example.com/\xe2\x80\x8b\e[1mvery\e[0m/\xe2\x80\x8blong/\xe2\x80\x8b\e[1mpath\e[0m.\r\n"))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        ;; The zerowidth chars must be there. They must not have
        ;; thrown off the text property computations.
        (should
         (equal
           "https://example.com/\u200b[very]/\u200blong/\u200b[path]."
          (mistty-test-content :show-property '(face ansi-color-bold))))
        ))))

(ert-deftest mistty-alacritty-vt-render-unicode-joiner ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes
     term (vconcat
           ;; 👨 (Man) + [ZWJ] + 👩 (Woman) + [ZWJ] + 👧 (Girl)
           "\e[1m\xF0\x9F\x91\xA8\xE2\x80\x8D\xF0\x9F\x91\xA9\xE2\x80\x8D\xF0\x9F\x91\xA7\e[0m.\r\n"))
    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        ;; The joiner must not have thrown off the text property
        ;; computations (no matter how alacritty decided to render
        ;; it.)
        (should
         (equal
          "[👨\u200d👩\u200d👧]."
          (mistty-test-content :show-property '(face ansi-color-bold))))
        ))))

(ert-deftest mistty-alacritty-vt-scrollback-enabled ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-enable-scrollback term)

    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))

    (ert-with-test-buffer ()
      (let ((cursor (make-marker))
            (screen-top (make-marker)))
        (should (equal 0 (mistty-alacritty-vt-write-scrollback term)))
        (should (equal "" (buffer-string)))
        (goto-char (point-min))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9\n")
          (mistty-test-content :trim nil)))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n10"))
        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n11"))

        ;; the scrollback lines are written before the start
        ;; of the buffer
        (goto-char (point-min))
        (should (equal 2 (mistty-alacritty-vt-write-scrollback term)))
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "<>2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9\n"
           "10\n"
           "11\n")
          (mistty-test-content :show screen-top
                               :trim nil)))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n12"))
        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n13"))
        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n14"))

        ;; next time, only the additional scrollback lines
        ;; are written, so 3 lines, not 5.
        (goto-char screen-top)
        (should (equal 3 (mistty-alacritty-vt-write-scrollback term)))
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "<>5\n"
           "6\n"
           "7\n"
           "8\n"
           "9\n"
           "10\n"
           "11\n"
           "12\n"
           "13\n"
           "14\n")
          (mistty-test-content :show screen-top
                               :trim nil)))))))

(ert-deftest mistty-alacritty-vt-scrollback-trim-right ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-enable-scrollback term)

    ;; When writing scrollback data, spaces that were not actually
    ;; written at the end of the line should be skipped, but spaces
    ;; not written in the middle or in the beginning should be
    ;; written.
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[2Cfoo\e[2Cbar   "))

    ;; fill the screen
    (dotimes (i 10)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" i))))

    (ert-with-test-buffer ()
      (let ((cursor (make-marker))
            (screen-top (make-marker)))
        (mistty-alacritty-vt-write-scrollback term))
        (should (equal "  foo  bar   \n" (buffer-string))))))

(ert-deftest mistty-alacritty-vt-scrollback-disabled ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; unnecessary, as scrollback is disabled by default
    ;; (mistty-alacritty-vt-disable-scrollback term)

    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))

    (ert-with-test-buffer ()
      (let ((cursor (make-marker)))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9\n")
          (mistty-test-content :trim nil)))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n10"))
        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n11"))

        ;; There's no scrollback to write
        (goto-char (point-min))
        (should (equal 0 (mistty-alacritty-vt-write-scrollback term)))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8\n"
           "9\n"
           "10\n"
           "11\n")
          (mistty-test-content :trim nil)))

        ))))

(ert-deftest mistty-alacritty-vt-wrapped-lines ()
  (let ((term (mistty-alacritty-vt-make-vterm 10 20))
        (cursor (make-marker)))

    ;; The first line cannot fit into 10 columns, it'll be split by
    ;; the terminal.
    (mistty-alacritty-vt-process-bytes
     term (vconcat "\rBaa, baa, black sheep have you any wool?"))
    (mistty-alacritty-vt-process-bytes term (vconcat " Yes sir, yes, sir three bags full!"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nOne for the Master"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nand one for the Dame"))

    (ert-with-test-buffer ()
      (mistty-alacritty-vt-render term cursor)
      (should (equal
       (concat
        "Baa, baa, [\n]black shee[\n]p have you[\n] any wool?[\n] Yes sir, [\n]yes, sir t[\n]hree bags [\n]full!\n"
        "One for th[\n]e Master\n"
        "and one fo[\n]r the Dame")
       (mistty-test-content :show-property '(term-line-wrap t))))
      (should (equal
       (concat
        "Baa, baa, [\n]black shee[\n]p have you[\n] any wool?[\n] Yes sir, [\n]yes, sir t[\n]hree bags [\n]full!\n"
        "One for th[\n]e Master\n"
        "and one fo[\n]r the Dame")
       (mistty-test-content :show-property '(invisible term-line-wrap))))

      ;; an empty yank handler should be set for fake newlines but not
      ;; for real ones.
      (goto-char (point-min))
      (should (equal '(nil "" nil nil)
                     (get-text-property
                      (search-forward "shee") 'yank-handler)))
      (should (equal '(nil "" nil nil)
                     (get-text-property
                      (search-forward "have you") 'yank-handler)))
      (should (eq nil (get-text-property
                       (search-forward "full!") 'yank-handler))))))

(ert-deftest mistty-alacritty-vt-scrollback-not-wrapped ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-enable-scrollback term)

    ;; The first line cannot fit into 10 columns, it'll be split by
    ;; the terminal.
    (mistty-alacritty-vt-process-bytes
     term (vconcat "\rBaa, baa, black sheep have you any wool?"))
    (mistty-alacritty-vt-process-bytes term (vconcat " Yes sir, yes, sir three bags full!"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nOne for the Master"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nand one for the Dame"))

    ;; fill the screen, moving the wrapped line into scrollback
    (dotimes (i 10)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" i))))

    (ert-with-test-buffer ()
      (goto-char (point-min))
      (should (equal 3 (mistty-alacritty-vt-write-scrollback term)))
      (should (equal
               (concat
                "Baa, baa, black sheep have you any wool? Yes sir, yes, sir three bags full!\n"
                "One for the Master\n"
                "and one for the Dame")
               (mistty-test-content))))))

(ert-deftest mistty-alacritty-vt-clear-scrollback ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-enable-scrollback term)

    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))

    (should (equal 0 (mistty-alacritty-vt-topmost-line term)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n10"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n11"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n12"))
    (should (equal -3 (mistty-alacritty-vt-topmost-line term)))
    (should (equal 3 (mistty-alacritty-vt-clear-scrollback term)))
    (should (equal 0 (mistty-alacritty-vt-topmost-line term)))))

(ert-deftest mistty-alacritty-vt-top-bottom-lines ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-enable-scrollback term)

    (should (equal 0 (mistty-alacritty-vt-topmost-line term)))
    (should (equal 9 (mistty-alacritty-vt-bottommost-line term)))
    (should (equal 19 (mistty-alacritty-vt-last-column term)))

    ;; fill the screen
    (mistty-alacritty-vt-process-bytes term (vconcat "\r0"))
    (dotimes (i 9)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))

    (should (equal 0 (mistty-alacritty-vt-topmost-line term)))
    (should (equal 9 (mistty-alacritty-vt-bottommost-line term)))

    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n10"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n11"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n12"))

    (should (equal -3 (mistty-alacritty-vt-topmost-line term)))
    (should (equal 9 (mistty-alacritty-vt-bottommost-line term)))

    (ert-with-test-buffer ()
      (mistty-alacritty-vt-write-scrollback term)

    (should (equal 0 (mistty-alacritty-vt-topmost-line term)))
    (should (equal 9 (mistty-alacritty-vt-bottommost-line term)))

    (mistty-alacritty-vt-process-bytes term (vconcat "\r\n13"))
    (should (equal -1 (mistty-alacritty-vt-topmost-line term)))
    (should (equal 9 (mistty-alacritty-vt-bottommost-line term))))))

(ert-deftest mistty-alacritty-vt-cursor ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (should (equal '(0 . 0) (mistty-alacritty-vt-cursor term)))
    (mistty-alacritty-vt-process-bytes term (vconcat "test"))
    (should (equal '(0 . 4) (mistty-alacritty-vt-cursor term)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[2D"))
    (should (equal '(0 . 2) (mistty-alacritty-vt-cursor term)))
    (mistty-alacritty-vt-process-bytes term (vconcat "\e[3B\e[5C"))
    (should (equal '(3 . 7) (mistty-alacritty-vt-cursor term)))))

(ert-deftest mistty-alacritty-vt-count-chars ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; full empty line
    (should (equal 20 (mistty-alacritty-vt-count-chars term 0 0 0 20)))
    ;; full empty screen, 10 lines of 20 columns + newline
    (should (equal 210 (mistty-alacritty-vt-count-chars term 0 0 10 0)))

    ;; line 0: regular 1-byte chars in UTF-8
    (mistty-alacritty-vt-process-bytes term (vconcat "baa, baa\r\n"))

    ;; line 1: regular 1-byte chars in UTF-8
    (mistty-alacritty-vt-process-bytes term (vconcat "black sheep\r\n"))

    ;; line 2: wide character: character takes 2 columns
    (mistty-alacritty-vt-process-bytes term (vconcat ".\xF0\x9F\x9F\xA7....\r\n"))

    ;; line 3: combining characters: columns 2 and 4 display 2 chars
    (mistty-alacritty-vt-process-bytes term (vconcat "l'e\xcc\x81te\xcc\x81.\r\n"))

    ;; line 4: zerowidth character: column 1 and 3 display 2 chars
    (mistty-alacritty-vt-process-bytes term (vconcat "--\xe2\x80\x8b--\xe2\x80\x8b---\r\n"))

    ;; simple case
    (should (equal 5 (mistty-alacritty-vt-count-chars term 0 0 0 5)))
    ;; empty
    (should (equal 0 (mistty-alacritty-vt-count-chars term 0 0 0 0)))
    ;; full line, without newline
    (should (equal 20 (mistty-alacritty-vt-count-chars term 0 0 0 20)))
    ;; full line plus newline
    (should (equal 21 (mistty-alacritty-vt-count-chars term 0 0 1 0)))
    ;; partial line start
    (should (equal 16 (mistty-alacritty-vt-count-chars term 0 5 1 0)))
    ;; partial line end
    (should (equal 26 (mistty-alacritty-vt-count-chars term 0 0 1 5)))
    ;; multiple lines, with newlines (2 lines + 2 newlines)
    (should (equal 62 (mistty-alacritty-vt-count-chars term 0 0 3 0)))

    ;; column 1 on line 2, containing a wide character, count as one
    ;; character.
    (should (equal 1 (mistty-alacritty-vt-count-chars term 2 0 2 1)))
    (should (equal 2 (mistty-alacritty-vt-count-chars term 2 0 2 2)))
    (should (equal 2 (mistty-alacritty-vt-count-chars term 2 0 2 3)))
    (should (equal 3 (mistty-alacritty-vt-count-chars term 2 0 2 4)))
    (should (equal 20 (mistty-alacritty-vt-count-chars term 2 0 3 0)))

    ;; line 3 contains columns that display multiple characters
    (should (equal 1 (mistty-alacritty-vt-count-chars term 3 0 3 1)))
    (should (equal 2 (mistty-alacritty-vt-count-chars term 3 0 3 2)))
    (should (equal 4 (mistty-alacritty-vt-count-chars term 3 0 3 3)))
    (should (equal 5 (mistty-alacritty-vt-count-chars term 3 0 3 4)))
    (should (equal 7 (mistty-alacritty-vt-count-chars term 3 0 3 5)))
    (should (equal 23 (mistty-alacritty-vt-count-chars term 3 0 4 0)))

    ;; line 4 contains two zerowidth characters
    (should (equal 1 (mistty-alacritty-vt-count-chars term 4 0 4 1)))
    (should (equal 3 (mistty-alacritty-vt-count-chars term 4 0 4 2)))
    (should (equal 4 (mistty-alacritty-vt-count-chars term 4 0 4 3)))
    (should (equal 6 (mistty-alacritty-vt-count-chars term 4 0 4 4)))
    (should (equal 7 (mistty-alacritty-vt-count-chars term 4 0 4 5)))
    (should (equal 23 (mistty-alacritty-vt-count-chars term 4 0 5 0)))))

(ert-deftest mistty-alacritty-vt-count-chars-invalid ()
    (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
      ;; end < start
      (should-error (mistty-alacritty-vt-count-chars term 1 0 0 5))
      ;; invalid start line
      (should-error (mistty-alacritty-vt-count-chars term -1 0 0 1))
      ;; invalid start column
      (should-error (mistty-alacritty-vt-count-chars term 0 20 1 0))

      ;; invalid end line
      (should-error (mistty-alacritty-vt-count-chars term 0 0 10 1))
      (should-error (mistty-alacritty-vt-count-chars term 0 0 11 0))))

(ert-deftest mistty-alacritty-vt-count-chars-in-scrollback ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    ;; negative line are ok when there is scrollback data
    (mistty-alacritty-vt-enable-scrollback term)
    (mistty-alacritty-vt-process-bytes term (vconcat "0\r\n"))
    (dotimes (i 20)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" (1+ i)))))

    (should (equal 21 (mistty-alacritty-vt-count-chars term -2 0 -1 0)))))


(ert-deftest mistty-alacritty-vt-count-unwrapped-lines ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes
     term (vconcat "\rBaa, baa, black sheep have you any wool?"))
    (mistty-alacritty-vt-process-bytes term (vconcat " Yes sir, yes, sir three bags full!"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nOne for the Master"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nand one for the Dame"))

    ;; The first real line takes 4 terminal lines. The two lines after
    ;; that each take one terminal line.
    (should (equal 0 (mistty-alacritty-vt-count-unwrapped-lines term 0 1)))
    (should (equal 0 (mistty-alacritty-vt-count-unwrapped-lines term 0 2)))
    (should (equal 0 (mistty-alacritty-vt-count-unwrapped-lines term 0 3)))
    (should (equal 1 (mistty-alacritty-vt-count-unwrapped-lines term 0 4)))
    (should (equal 2 (mistty-alacritty-vt-count-unwrapped-lines term 0 5)))

    ;; The real newline is at the end of line 3
    (should (equal 0 (mistty-alacritty-vt-count-unwrapped-lines term 1 2)))
    (should (equal 1 (mistty-alacritty-vt-count-unwrapped-lines term 3 4)))
    (should (equal 2 (mistty-alacritty-vt-count-unwrapped-lines term 3 5)))

    ;; The last line is a real line
    (should (equal 3 (mistty-alacritty-vt-count-unwrapped-lines term 0 6)))

    ;; Empty lines are all real
    (should (equal 3 (mistty-alacritty-vt-count-unwrapped-lines term 6 9)))

    ;; The last line is real
    (should (equal 1 (mistty-alacritty-vt-count-unwrapped-lines term 9 10)))))

(ert-deftest mistty-alacritty-vt-count-unwrapped-lines-in-scrollback ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes
     term (vconcat "\rBaa, baa, black sheep have you any wool?"))
    (mistty-alacritty-vt-process-bytes term (vconcat " Yes sir, yes, sir three bags full!"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nOne for the Master"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nand one for the Dame"))

    ;; fill the screen and put everything into scrollback
    (mistty-alacritty-vt-enable-scrollback term)
    (dotimes (i 10)
      (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" i))))

    ;; count the lines in scrollback
    (should (equal 3 (mistty-alacritty-vt-count-unwrapped-lines term -6 0)))))

(ert-deftest mistty-alacritty-vt-count-unwrapped-lines-invalid ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (should (equal 10 (mistty-alacritty-vt-count-unwrapped-lines term 0 10)))
    (should-error (mistty-alacritty-vt-count-unwrapped-lines term -1 1))
    (should-error (mistty-alacritty-vt-count-unwrapped-lines term 0 11))
    (should-error (mistty-alacritty-vt-count-unwrapped-lines term 2 1))))

(ert-deftest mistty-alacritty-vt-scrollback-wrapped-lines ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
    (mistty-alacritty-vt-process-bytes
     term (vconcat "\rBaa, baa, black sheep have you any wool?"))
    (mistty-alacritty-vt-process-bytes term (vconcat " Yes sir, yes, sir three bags full!"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nOne for the Master"))
    (mistty-alacritty-vt-process-bytes term (vconcat "\r\nand one for the Dame"))

    (mistty-alacritty-vt-enable-scrollback term)
    (ert-with-test-buffer ()
      (let ((cursor (copy-marker (point-min)))
            (screen-top (copy-marker (point-min))))
        (goto-char screen-top)
        (mistty-alacritty-vt-write-scrollback term)
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (dotimes (i 4)
          (mistty-alacritty-vt-process-bytes term (vconcat (format "\r\n%d" i)))
          (goto-char screen-top)
          (mistty-alacritty-vt-write-scrollback term)
          (set-marker screen-top (point))
          (mistty-alacritty-vt-render term cursor))
        (should
         (equal
          (concat
           "<>Baa, baa, black shee[\n]"
           "p have you any wool?[\n]"
           " Yes sir, yes, sir t[\n]"
           "hree bags full!\n"
           "One for the Master\n"
           "and one for the Dame\n"
           "0\n"
           "1\n"
           "2\n"
           "3")
          (mistty-test-content
           :show screen-top :show-property '(term-line-wrap t))))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n4"))
        (goto-char screen-top)
        (mistty-alacritty-vt-write-scrollback term)
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "Baa, baa, black shee[\n]"
           ;; The scrollback line above must end with a newline, even
           ;; tough it's wrapped, because the next line is a terminal
           ;; line. The newline must be marked as 'term-line-wrap.
           "<>p have you any wool?[\n]"
           " Yes sir, yes, sir t[\n]"
           "hree bags full!\n"
           "One for the Master\n"
           "and one for the Dame\n"
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4")
          (mistty-test-content
           :show screen-top :show-property '(term-line-wrap t))))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n5"))
        (goto-char screen-top)
        (mistty-alacritty-vt-write-scrollback term)
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "Baa, baa, black sheep have you any wool?[\n]"
           ;; The newline within sheep, above, must have been removed
           ;; when writing the scrollback as now both terminal lines
           ;; are part of the scrollback portion of the buffer.
           "<> Yes sir, yes, sir t[\n]"
           "hree bags full!\n"
           "One for the Master\n"
           "and one for the Dame\n"
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5")
          (mistty-test-content
           :show screen-top :show-property '(term-line-wrap t))))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n6"))
        (goto-char screen-top)
        (mistty-alacritty-vt-write-scrollback term)
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "Baa, baa, black sheep have you any wool? Yes sir, yes, sir t[\n]"
           "<>hree bags full!\n"
           "One for the Master\n"
           "and one for the Dame\n"
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6")
          (mistty-test-content
           :show screen-top :show-property '(term-line-wrap t))))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n7"))
        (goto-char screen-top)
        (mistty-alacritty-vt-write-scrollback term)
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "Baa, baa, black sheep have you any wool? Yes sir, yes, sir three bags full!\n"
           "<>One for the Master\n"
           "and one for the Dame\n"
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7")
          (mistty-test-content
           :show screen-top :show-property '(term-line-wrap t))))

        (mistty-alacritty-vt-process-bytes term (vconcat "\r\n8"))
        (goto-char screen-top)
        (mistty-alacritty-vt-write-scrollback term)
        (set-marker screen-top (point))
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           "Baa, baa, black sheep have you any wool? Yes sir, yes, sir three bags full!\n"
           "One for the Master\n"
           "<>and one for the Dame\n"
           "0\n"
           "1\n"
           "2\n"
           "3\n"
           "4\n"
           "5\n"
           "6\n"
           "7\n"
           "8")
          (mistty-test-content
           :show screen-top :show-property '(term-line-wrap t))))))))

(ert-deftest mistty-alacritty-vt-render-mistty-clear ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-render term cursor)
      (should (equal
               (concat "<>\n")
               (mistty-test-content
                :trim nil
                :show cursor
                :show-property '(mistty-clear t))))

      ;; mistty-clear identifies cells that have been explicitly
      ;; written to. It allows telling cells that contain space from
      ;; cells that are just empty. It is only written for clear
      ;; spaces within the line, as clear, trailing spaces are just
      ;; removed, unless the cursor is on that point.
      (mistty-alacritty-vt-process-bytes term (vconcat "\e[2Chello,  \e[2Cworld. \r\n"))
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)
      (should (equal
               (concat "[  ]hello,  [  ]world. \n"
                       "<>\n")
               (mistty-test-content
                :trim nil
                :show cursor
                :show-property '(mistty-clear t))))

      ;; mistty-clear must be reset when cells are cleared
      (mistty-alacritty-vt-process-bytes term (vconcat "\e[H\e[2J"))
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)
      (should (equal
               (concat "<>\n")
               (mistty-test-content
                :trim nil
                :show cursor
                :show-property '(mistty-clear t)))))))

(ert-deftest mistty-alacritty-vt-render-mistty-clear-not-dim ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      ;; Since the DIM flag is used internally to track clear terminal
      ;; columns, DIM-related commands must be ignored. Notably \e[0m
      ;; and \e[22m must not clear DIM (but they must clear BOLD).
      (mistty-alacritty-vt-process-bytes
       term (vconcat "\e[1mf\e[0moo\r\n\e[1mb\e[22mar\r\n\e[2mnot dim\e[0m\r\n"))
      (mistty-alacritty-vt-render term cursor)

      (should (equal
               (concat "foo\n"
                       "bar\n"
                       "not dim\n"
                       "<>\n")
               (mistty-test-content
                :trim nil
                :show cursor
                :show-property '(mistty-clear t))))
      (should (equal
               (concat "[f]oo\n"
                       "[b]ar\n"
                       "not dim")
               (mistty-test-content :show-property '(face ansi-color-bold)))))))

(ert-deftest mistty-alacritty-vt-count-cells ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
      ;; mistty-clear identifies cells that have been explicitly
      ;; written to. It allows telling cells that contain space from
      ;; cells that are just empty.
      (mistty-alacritty-vt-process-bytes term (vconcat "\e[2Chello,  \e[2Cworld. \r\n"))
      (mistty-alacritty-vt-process-bytes term (vconcat "\e[2C foo \e[2C bar \r\n"))
      ;; "[  ]hello,  [  ]world. [ ]\n"
      ;; "[  ] foo [  ] bar [      ]\n"

      ;; 1st line: "hello, world. "
      (should (equal 15 (mistty-alacritty-vt-count-cells term 0 0 0 20)))

      ;; 2nd line: " foo bar "
      (should (equal 10 (mistty-alacritty-vt-count-cells term 1 0 1 20)))

      ;; line 1 and 2, with two newlines
      (should (equal 27 (mistty-alacritty-vt-count-cells term 0 0 2 0)))

      ;; "[ ]\n[  ] f"
      (should (equal 3 (mistty-alacritty-vt-count-cells term 0 18 1 3)))))

(ert-deftest mistty-alacritty-vt-count-cells-invalid ()
    (let ((term (mistty-alacritty-vt-make-vterm 20 10)))
      ;; end < start
      (should-error (mistty-alacritty-vt-count-cells term 1 0 0 5))
      ;; invalid start line
      (should-error (mistty-alacritty-vt-count-cells term -1 0 0 1))
      ;; invalid start column
      (should-error (mistty-alacritty-vt-count-cells term 0 20 1 0))

      ;; invalid end line
      (should-error (mistty-alacritty-vt-count-cells term 0 0 10 1))
      (should-error (mistty-alacritty-vt-count-cells term 0 0 11 0))))

(ert-deftest mistty-alacritty-vt-resize ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes term (vconcat "Baa, baa, black sheep have you any wool?\r\n"))
      (mistty-alacritty-vt-render term cursor)
      (should (equal
               (concat "Baa, baa, black shee\n"
                       "p have you any wool?\n"
                       "\n")
               (mistty-test-content :trim nil)))

      (mistty-alacritty-vt-resize term 30 8)
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)
      (should (equal
               (concat "Baa, baa, black sheep have you\n"
                       " any wool?\n"
                       "\n")
               (mistty-test-content :trim nil))))))


(ert-deftest mistty-alacritty-vt-clear-to-eol ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes term (vconcat "foo     \r\nbar          \r\n"))
      (mistty-alacritty-vt-render term cursor)
      (should (equal
               (concat "foo     \n"
                       "bar          \n"
                       "<>\n")
               (mistty-test-content
                :trim nil
                :show cursor
                :show-property '(mistty-clear t))))

      (mistty-alacritty-vt-clear-to-eol term 0 5)
      (mistty-alacritty-vt-clear-to-eol term 1 3)
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)

      (should (equal
               (concat "foo  \n"
                       "bar\n"
                       "<>\n")
               (mistty-test-content
                :trim nil
                :show cursor
                :show-property '(mistty-clear t)))))))

(ert-deftest mistty-alacritty-vt-clear-to-eol-unicode ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      ;; This makes sure the unicode characters don't mess up the
      ;; char-to-column computations.

      ;; wide char (1 char, 2 columns)
      (mistty-alacritty-vt-process-bytes term (vconcat "\xF0\x9F\x9F\xA7(1)    \r\n"))
      ;; combining chars (2 chars, 1 column)
      (mistty-alacritty-vt-process-bytes term (vconcat "e\xcc\x81te\xcc\x81(2)    \r\n"))
      (mistty-alacritty-vt-render term cursor)

      (let ((one (mistty-test-pos-after "(1)"))
            (two (mistty-test-pos-after "(2)")))
        (mistty-alacritty-vt-clear-to-eol term 0 (- one (mistty--bol one)))
        (mistty-alacritty-vt-clear-to-eol term 1 (- two (mistty--bol two)))
        (goto-char (point-min))
        (mistty-alacritty-vt-render term cursor)

        (should (equal
                 (concat
                  "\U0001F7E7(1)\n"
                  "e\u0301te\u0301(2)\n"
                  "\n")
                 (mistty-test-content :trim nil)))))))

(ert-deftest mistty-alacritty-vt-cleanup-sp-continued ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 5))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes term (vconcat "output 1\r\n"))
      (mistty-alacritty-vt-process-bytes term (vconcat "end"))
      (mistty-alacritty-vt-process-bytes term (vconcat "%" (make-string 19 ?\ ))) ;; prompt-sp
      (mistty-alacritty-vt-render term cursor)

      (should (equal
               (concat "output 1\n"
                       "end%                [\n]"
                       "   <>\n")
               (mistty-test-content
                :trim nil :show cursor :show-property '(term-line-wrap t))))

      (mistty-alacritty-vt-cleanup-prompt-sp term 2)
      (mistty-alacritty-vt-process-bytes term (vconcat "\r"))
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)

      (should (equal
               (concat "output 1\n"
                       "end%\n"
                       "<>\n")
               (mistty-test-content
                :trim nil :show cursor :show-property '(term-line-wrap t)))))))

(ert-deftest mistty-alacritty-vt-cleanup-sp-not-continued ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 5))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes term (vconcat "output 1\r\n"))
      (mistty-alacritty-vt-process-bytes term (vconcat "%" (make-string 19 ?\ ))) ;; prompt-sp
      (mistty-alacritty-vt-render term cursor)

      (should (equal
               (concat "output 1\n"
                       "%                  <> \n")
               (mistty-test-content
                :trim nil :show cursor :show-property '(term-line-wrap t))))

      (mistty-alacritty-vt-cleanup-prompt-sp term 1)
      (mistty-alacritty-vt-process-bytes term (vconcat "\r"))
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)

      (should (equal
               (concat "output 1\n"
                       "<>%\n")
               (mistty-test-content
                :trim nil :show cursor :show-property '(term-line-wrap t)))))))

(ert-deftest mistty-alacritty-vt-mark-indent ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes
       term
       (vconcat "$ for i in a b c\r\n\e[3Cecho\e[2C$i\r\n\e[3C"))
      (mistty-alacritty-vt-render term cursor)
      (should
       (equal
        (concat
         "$ for i in a b c\n"
         "[   ]echo  $i\n"
         "[   ]<>\n")
        (mistty-test-content
         :trim nil :show cursor :show-property '(mistty-skip indent))))

      (mistty-alacritty-vt-process-bytes
       term
       (vconcat "call\e[2C$i\r\ndone"))
      (goto-char (point-min))
      (mistty-alacritty-vt-render term cursor)
      (should
       (equal
        (concat
         "$ for i in a b c\n"
         "[   ]echo  $i\n"
         "[   ]call  $i\n"
         "done\n")
        (mistty-test-content :trim nil :show-property '(mistty-skip indent))))
      )))

(ert-deftest mistty-alacritty-vt-mark-right-prompt ()
  (let ((term (mistty-alacritty-vt-make-vterm 30 10))
        (cursor (make-marker)))
    (mistty-alacritty-vt-process-bytes
     term
     (vconcat "$ echo foo\e[10Cbar\r\n"
              "\e[24G> right\e[1G$ echo foo\r\n"
              "\e[23G> right\e[1G$ echo foo\r\n"
              "\e[22G> right\e[1G$ echo foo\r\n"
              "\e[21G> right\e[1G$ echo foo\r\n"
              "\e[24G> right\e[1G"
              ))
    (ert-with-test-buffer ()
        (mistty-alacritty-vt-render term cursor)
        (should
         (equal
          (concat
           ;; not a right prompt; bar is too much to the left
           "$ echo foo          bar\n"

           ;; a right prompt, > right is at the end
           "$ echo foo[             > right]\n"

           ;; a right prompt, > right is almost at the end (1col)
           "$ echo foo[            > right]\n"

           ;; a right prompt, > right is almost at the end (2col)
           "$ echo foo[           > right]\n"

           ;; not a right prompt; >right is too far from end
           "$ echo foo          > right\n"

           ;; a right prompt, even though there is nothing to the left
           "[                       > right]\n")
          (mistty-test-content
           :trim nil :show-property '(mistty-skip right-prompt))))

        ;; the last line shouldn't have an indent, because
        ;; right-prompt takes precedence.
        (should
         (equal
           "                       > right\n"
          (mistty-test-content
           :trim nil :start (mistty--bol (point-max) 0)
           :show-property '(mistty-skip indent)))))))

(ert-deftest mistty-alacritty-vt-link ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes
       term
       (vconcat "hello \e]8;;http://www.example.com/world\e\\world\e]8;;\e\\ !"))
      (mistty-alacritty-vt-render term cursor)
      (goto-char (point-min))
      (search-forward "world")
      (let* ((hello-beg (match-beginning 0))
             (hello-end (match-end 0))
             (button (button-at hello-beg)))
        (should-not (null button))
        (should (button-at (1- hello-end)))
        (should-not (button-at (1- hello-beg)))
        (should-not (button-at (1+ hello-end)))
        (should (eq 'ansi-osc-hyperlink (button-get button 'type)))
        (should (equal "http://www.example.com/world" (button-get button 'browse-url-data)))))))

(ert-deftest mistty-alacritty-vt-consecutive-links ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (ert-with-test-buffer ()
      (mistty-alacritty-vt-process-bytes
       term
       (vconcat "\e]8;;http://www.example.com/hello\e\\hello \e]8;;\e\\"
                "\e]8;;http://www.example.com/world\e\\world\e]8;;\e\\ !"))
      (mistty-alacritty-vt-render term cursor)
      (goto-char (point-min))
      (search-forward "hello")
      (should (equal "http://www.example.com/hello" (button-get (button-at (match-beginning 0)) 'browse-url-data)))
      (should (equal "http://www.example.com/hello" (button-get (button-at (1- (match-end 0))) 'browse-url-data)))
      (search-forward "world")
      (should (equal "http://www.example.com/world" (button-get (button-at (match-beginning 0)) 'browse-url-data)))
      (should (equal "http://www.example.com/world" (button-get (button-at (1- (match-end 0))) 'browse-url-data))))))

(ert-deftest mistty-alacritty-vt-set-title ()
  (let ((term (mistty-alacritty-vt-make-vterm 20 10))
        (cursor (make-marker)))
    (should (equal
             '((title "window-title"))
             (mistty-alacritty-vt-process-bytes
              term (vconcat "foo\e]0;window-title\e\\bar"))))
    (ert-with-test-buffer ()
       (mistty-alacritty-vt-render term cursor)
       (should (equal "foobar" (mistty-test-content))))))

(ert-deftest mistty-alacritty-vt-osc52-update-kill-ring ()
  (let* ((mistty-alacritty-osc52 'only-copy)
         (term (mistty-alacritty-vt-make-vterm 80 10))
         (cursor (make-marker)))
    (kill-new "initial")
    (mistty-alacritty-vt-process-bytes
     term (vconcat (format "\e]52;c;%s\a" (base64-encode-string "baa1"))))
    (mistty-alacritty-vt-process-bytes
     term (vconcat (format "\e]52;c;%s\a" (base64-encode-string "baa2"))))

    (should (equal "baa2" (nth 0 kill-ring)))
    (should (equal "baa1" (nth 1 kill-ring)))
    (should (equal "initial" (nth 2 kill-ring)))))

(ert-deftest mistty-alacritty-vt-osc52-clear ()
  (let* ((mistty-alacritty-osc52 'only-copy)
         (term (mistty-alacritty-vt-make-vterm 80 10))
         (cursor (make-marker)))
    (kill-new "initial")
    (mistty-alacritty-vt-process-bytes
     term (vconcat (format "\e]52;c;%s\a" (base64-encode-string "baa1"))))
    (should (equal "baa1" (nth 0 kill-ring)))

    ;; TODO: This is a "clear" operation. It doesn't fit well into
    ;; Emacs kill-ring concept. Should clear actually remove the
    ;; value? should it be ignored?
    (mistty-alacritty-vt-process-bytes term (vconcat "\e]52;c;\a"))
    (should (equal "" (nth 0 kill-ring)))
    (should (equal "baa1" (nth 1 kill-ring)))))

(ert-deftest mistty-alacritty-vt-osc52-disabled ()
  (let* ((mistty-alacritty-osc52 nil)
         (term (mistty-alacritty-vt-make-vterm 80 10)))
    (kill-new "initial")
    (should
     (equal nil
            (mistty-alacritty-vt-process-bytes
             term
             (vconcat (format "\e]52;c;%s\a"
                              (base64-encode-string "foo, bar"))))))

    ;; copy did not work
    (should (equal "initial" (current-kill 0)))

    ;; paste did not work
    (should (equal nil (mistty-alacritty-vt-process-bytes
                        term (vconcat "\e]52;c;?\a"))))))

(ert-deftest mistty-alacritty-vt-osc52-only-copy ()
  (let* ((mistty-alacritty-osc52 'only-copy)
         (term (mistty-alacritty-vt-make-vterm 80 10)))
    (kill-new "initial")
    (should
     (equal nil
            (mistty-alacritty-vt-process-bytes
             term
             (vconcat (format "\e]52;c;%s\a"
                              (base64-encode-string "foo, bar"))))))

    (should (equal "foo, bar" (current-kill 0)))

    ;; paste did not work
    (should (equal nil (mistty-alacritty-vt-process-bytes
                        term (vconcat "\e]52;c;?\a"))))))

(ert-deftest mistty-alacritty-vt-osc52-only-paste ()
  (let* ((mistty-alacritty-osc52 'only-paste)
         (term (mistty-alacritty-vt-make-vterm 80 10)))
    (kill-new "initial")
    (should
     (equal nil
            (mistty-alacritty-vt-process-bytes
             term (vconcat (format "value\e]52;c;%s\a : "
                                   (base64-encode-string "foo, bar"))))))
    ;; copying didn't work
    (should (equal "initial" (current-kill 0)))

    (should (equal `((pty-write
                     ,(format "\e]52;c;%s\a"
                              (base64-encode-string "initial"))))
                   (mistty-alacritty-vt-process-bytes
                    term (vconcat "\e]52;c;?\a."))))))

(ert-deftest mistty-alacritty-vt-osc52-copy-paste ()
  (let* ((mistty-alacritty-osc52 'copy-paste)
         (term (mistty-alacritty-vt-make-vterm 80 10)))
    (kill-new "initial")
    (should
     (equal nil
            (mistty-alacritty-vt-process-bytes
             term (vconcat (format "value\e]52;c;%s\a : "
                                   (base64-encode-string "foo, bar"))))))
    (should (equal "foo, bar" (current-kill 0)))

    (kill-new "new value")
    (should (equal `((pty-write
                     ,(format "\e]52;c;%s\a"
                              (base64-encode-string "new value"))))
                   (mistty-alacritty-vt-process-bytes
                    term (vconcat "\e]52;c;?\a."))))))
