;;; nocomments-mode-test-files.el --- `nocomments-mode' file tests.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Anders Lindgren

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

;; Regression test of `nocomments-mode', a package that makes commets
;; invisible. This module verifies fontification of a number of
;; files. This is done by keeing a text representation of the
;; fontification using `faceup' markup, in addition to the original
;; files.
;;
;; The actual check is performed using `ert', with font-lock test
;; function provided by `faceup'.

;;; Code:

;; Note: Due to a problem in `faceup', only a single `display' entry
;; is emitted for each line, instead of one per character.
;;
;; Despite this, the test is retained (as it would find problems).
;; However, the reference file must be regenerated when faceup has
;; been corrected.

(require 'faceup)

(defvar nocomments-mode-test-dir (faceup-this-file-directory))

(defun nocomments-mode-test-file (file)
  "Test that FILE is fontified as the .faceup file describes.

FILE is interpreted as relative to this source directory."
  (let ((faceup-properties '(face display)))
    (faceup-test-font-lock-file '(emacs-lisp-mode
				  nocomments-mode)
				(concat
				 nocomments-mode-test-dir
				 file))))
(faceup-defexplainer nocomments-mode-test-file)


(ert-deftest nocomments-mode-test-files ()
  (should (nocomments-mode-test-file "files/basics.el")))

(defun nocomments-mode-test-facup-write-file ()
  "Like `faceup-write-file' but include the `display' property."
  (interactive)
  (let ((faceup-properties '(face display)))
    (call-interactively #'faceup-write-file)))

(provide 'nocomments-mode-test-files)

;; nocomments-mode-test-files.el ends here.
