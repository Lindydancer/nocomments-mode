;;; nocomments-mode-test-setup.el --- Setup and execute all tests.  -*- lexical-binding: t; -*-

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

;; This package sets up a suitable enviroment for testing
;; `nocomments-mode', and executes the tests.
;;
;; Usage:
;;
;;   emacs -Q -l nocomments-mode-test-setup.el
;;
;; Note that this package assumes that some packages are located in
;; specific locations.

;; Additional tests:
;;
;; - `nocomments-mode-test-misc' generates an example containing
;;   comments with extra highlighting that use a background color, and
;;   background overlays.  This is useful to check that
;;   `nocomments-mode' work properly in that environment.
;;   Unfortuantely, this test is not automatic.

;;; Code:

(setq inhibit-startup-screen t)
(prefer-coding-system 'utf-8)

(defvar nocomments-mode-test-setup-dir
  (if load-file-name
      (file-name-directory load-file-name)
    default-directory))

(dolist (dir '("." ".." "../../faceup"))
  (add-to-list 'load-path (concat nocomments-mode-test-setup-dir dir)))

(require 'nocomments-mode)
(require 'nocomments-mode-test-files)

(ert t)

;;; nocomments-mode-test-setup.el ends here
