;;; simplifies.el --- my emacs simplifies
;;; Commentary:
;;; Code:

(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))

(provide 'simplifies)
;;; simplifies.el ends here
