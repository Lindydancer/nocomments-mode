;;; nocomments-mode.el --- Minor mode that makes comments invisible.

;; Copyright (C) 2016  Anders Lindgren

;; Author: Anders Lindgren
;; Version: 0.0.0
;; Created: 2016-05-23
;; URL: https://github.com/Lindydancer/nocomments-mode

;; This program is free software; you can redistribute it and/or modify
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

;; Minor mode that can make all comments temporarily invisible.
;;
;; In most situations, comments in a program are good. However,
;; exuberant use of comments may make it harder to follow the flow of
;; the actual program. By temporarily making comments invisble, the
;; program will stand out more clearly.
;;
;; "Invisible" in this context means that the comments will not be
;; visible but they will still take up the same space they did before,
;; so non-comment portions will not move.

;; Example:
;;
;; | Before                     | After                         |
;; | ------                     | -----                         |
;; | ![](doc/before.png)        | ![](doc/after.png)            |


;; Install:
;;
;; Install this using the build-in Emacs package manager, e.g. using
;; `M-x package-install-from-file RET nocomments-mode.el RET'.

;; Usage:
;;
;; This package provides two minor modes:
;;
;; - `nocomments-mode' - Local minor mode that makes all comments in
;;   current buffer invisible.
;;
;; - `nocomments-global-mode' - Global minor mode that makes all
;;   comments in all buffers invisible.

;; Configuration:
;;
;; For convenience, you can bind a key to toggle the visibility of
;; comment. For example, you can place the following in a suitable
;; init file to make F12 toggle comments:
;;
;;     (global-set-key (kbd "<f12>") #'nocomments-mode)



;;; Code:

;; Implementation:
;;
;; A naive approach, to reassign `font-lock-comment-face', doesn't
;; work when packages higlight things inside comment (like
;; `emacs-lisp-mode' mode does with quoted words). Instead, a
;; font-lock keyword is used to search for comment, the rule prepends
;; a new custom face in top of the the entire comment, hiding the
;; comment and any extra highlighting it may contain.

;; Problems:
;;
;; Technically, it's not possible to define a face that use the
;; "default background" as both background and foreground. Instead,
;; the actual background of the selected frame is used. This may lead
;; to problems if the buffer is visible in multiple frames, and that
;; the frames use different color schemes. (However, I doubt it will
;; ever be a problem in real life.)

;; -------------------------------------------------------------------
;; The modes.
;;


(defgroup nocomments-mode nil
  "Minor mode that makes all comments invisble."
  :group 'faces)


(defface nocomments-invisible-face
  '((t))
  "Face applied to comments when `nocomments-mode' is enabled.

Note: Foreground and background are set when the mode is enabled."
  :group 'nocomments-mode)


(defun nocomments-beginning-of-comment ()
  "If point is inside a comment, move point to start and return non-nil."
  (let ((state (syntax-ppss)))
    (and (nth 4 state)                  ; Comment
         (progn
           (goto-char (nth 8 state))
           (point)))))


(defun nocomments-end-of-comment ()
  "Move to of end of comment after point.

Also, include trailing newline and empty lines."
  (forward-comment 1)
  ;; Include a bit more, to ensure that doxygen comments works
  ;; properly, in case of custom highlighting.
  (while (progn (skip-syntax-forward " ")
                (and (eolp)
                     (not (eobp))))
    (forward-line)))


;; Don't warn for using these dynamically bound variables, see
;; `font-lock-extend-region-functions'.
(eval-when-compile
  (defvar font-lock-beg)
  (defvar font-lock-end))


(defun nocomments-extend-region-full-comment ()
  "Extend font-lock region to include full comments."
  (save-excursion
    (let ((res nil))
      (goto-char font-lock-beg)
      (when (nocomments-beginning-of-comment)
        (when (< (point) font-lock-beg)
          (setq font-lock-beg (point))
          (setq res t)))
      (goto-char font-lock-end)
      (when (nocomments-beginning-of-comment)
        (nocomments-end-of-comment)
        (when (< font-lock-end (point))
          (setq font-lock-end (point))
          (setq res t)))
      res)))


(defvar nocomments-font-lock-keywords
  '(((lambda (limit)
       (let ((start (comment-search-forward limit t)))
         (when start
           (set-match-data (list start
                                 (save-excursion
                                   (goto-char start)
                                   (nocomments-end-of-comment)
                                   (point)))))
         start))
     (0 'nocomments-invisible-face prepend))))


;;;###autoload
(define-minor-mode nocomments-mode
  "Minor mode makes comment invisible.

\"Invisible\" in this context means that comments will be painted
in the same color as the background."
  :group 'nocomment
  (if nocomments-mode
      (progn
        (add-to-list 'font-lock-extend-region-functions
                     #'nocomments-extend-region-full-comment)
        (font-lock-add-keywords nil nocomments-font-lock-keywords 'append)
        (setq font-lock-multiline t)
        ;; Update the face. Note that this will not work when a user
        ;; has multiple frames with different color settings.
        (let ((color (frame-parameter (selected-frame) 'background-color)))
          (set-face-foreground 'nocomments-invisible-face color)
          (set-face-background 'nocomments-invisible-face color)))
    ;; Note: `font-lock-multiline' is not restored. It may have gotten
    ;; set by some other minor mode. Besides, it doesn't hurt keeping
    ;; it set to t.
    (setq font-lock-extend-region-functions
          (delq 'nocomments-extend-region-full-comment
                font-lock-extend-region-functions))
    (font-lock-remove-keywords nil nocomments-font-lock-keywords))
  ;; As of Emacs 25, `font-lock-fontify-buffer' is not legal to
  ;; call, instead `font-lock-flush' should be used.
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings
        (font-lock-fontify-buffer)))))


;;;###autoload
(define-global-minor-mode nocomments-global-mode nocomments-mode
  (lambda ()
    (nocomments-mode 1))
  :group 'nocomment)


(provide 'nocomments-mode)

;;; nocomments-mode.el ends here
