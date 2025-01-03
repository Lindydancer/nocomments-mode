;; nocomments-mode-screenshot-setup.el --- Prepare for screenshot.  -*- lexical-binding: t; -*-

;; Usage:
;;
;;   emacs -q -l nocomments-mode-screenshot-setup.el
;;
;;   C-c n. Take screenshot. C-c n. Take second screenshot.
;;
;;   OS X: Cmd-Shift-4 SPC click on window.

(setq inhibit-startup-screen t)

(blink-cursor-mode -1)

(defvar nocomments-mode-screenshot-dir
  (or (and load-file-name
           (file-name-directory load-file-name))
      default-directory))

(load (concat nocomments-mode-screenshot-dir
              "../nocomments-mode.el"))
(nocomments-global-mode 1)

(global-set-key (kbd "C-c n") 'nocomments-mode)

(find-file (concat nocomments-mode-screenshot-dir "demo.el"))

(set-frame-size (selected-frame) 50 22)

(message "")

;; nocomments-mode-screenshot-setup.el ends here
