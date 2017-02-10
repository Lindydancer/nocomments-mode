;;; nocomments-mode-test-misc.el --- Misc test for nocomments-mode.

;;; Commentary:

;; Author: Anders Lindgren
;; Keywords: faces languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:


(defface nocomments-mode-test-misc-foreground
  '((t :foreground "green"))
  "Test face with foreground.")


(defface nocomments-mode-test-misc-background
  '((t :background "red"))
  "Test face with background.")

(defface nocomments-mode-test-misc-background2
  '((t :background "yellow"))
  "Test face with background.")


(defface nocomments-mode-test-misc-mix
  '((t :foreground "green" :background "red"))
  "Test face with foreground and background.")



(defvar nocomments-mode-test-misc-dummy-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; C and C++-style comments.
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    ;; Lisp-style comments.
    (modify-syntax-entry ?\; "< b" table)
    table)
  "Syntax table for `nocomments-mode-test-misc-dummy-mode'.")



(defvar nocomments-mode-test-misc-font-lock-keywords
  '(("F>\\(\\(.\\|\n\\)*?\\)<F"
     (1 'nocomments-mode-test-misc-foreground t))
    ("B>\\(\\(.\\|\n\\)*?\\)<B"
     (1 'nocomments-mode-test-misc-background t))
    ("M>\\(\\(.\\|\n\\)*?\\)<M"
     (1 'nocomments-mode-test-misc-mix t)))
  "Highlight rules for `nocomments-mode-test-misc-dummy-mode'.")


(define-derived-mode nocomments-mode-test-misc-dummy-mode nil
  "TestNocommentsDummy"
  (setq font-lock-multiline t)
  (setq font-lock-defaults
        '(nocomments-mode-test-misc-font-lock-keywords nil))
  ;; TODO: More variants.
  (setq comment-start-skip "/\\*"))


(defun nocomments-mode-test-misc-dummy-mode-example ()
  (interactive)
  (with-current-buffer (get-buffer-create "*TestNocommentsDummy*")
    (erase-buffer)
    (nocomments-mode-test-misc-dummy-mode)
    (let (midpoint)
      (dotimes (i 2)
        (setq midpoint (point))         ; Only last is used.
        (insert "/* allan */        Here is\n")
        (insert "/* F>allan<F */    some text\n")
        (insert "/* B>allan<B */    which isn't\n")
        (insert "/* M>allan<M */    a comment.\n")
        (insert "\n")
        (insert "/* sp  ace */        Here is\n")
        (insert "/* F>sp  ace<F */    some text\n")
        (insert "/* B>sp  ace<B */    which isn't\n")
        (insert "/* M>sp  ace<M */    a comment.\n")
        (insert "\n")
        (insert "/* TAB\tTAB */        Here is\n")
        (insert "/* F>TAB\tTAB<F */    some text\n")
        (insert "/* B>TAB\tTAB<B */    which isn't\n")
        (insert "/* M>TAB\tTAB<M */    a comment.\n")
        (insert "\n")
        (insert "/* multi\nline */        Here is\n")
        (insert "/* F>multi\nline<F */    some text\n")
        (insert "/* B>multi\nline<B */    which isn't\n")
        (insert "/* M>multi\nline<M */    a comment.\n")
        (insert "\n"))
      (let ((o (make-overlay midpoint (point) (current-buffer))))
        (overlay-put o 'face 'nocomments-mode-test-misc-background2)))

    (display-buffer (current-buffer))))


(provide 'nocomments-mode-test-misc)

;;; nocomments-mode-test-misc.el ends here
