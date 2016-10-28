
;; Demonstration of `nocomments-mode'.
;;
;; https://github.com/Lindydancer/nocomments-mode

(defun demo-do-something ()
  "Demonstration of `nocomments-mode'."
  ;; This function bla. bla. bla. bla.
  (if (> value 0)
      ;; Bla bla, some some.
      ;;
      ;; Can you spot the error?
      (if (equal value 0)
          'yes
        'no)
    'maybe))

(provide 'demo)
