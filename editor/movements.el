;;; movements.el --- effective cursor movements
;;
;; Filename: movements.el
;; Description: effective cursor movements
;; Author: Dmitry Akatov
;; Created: <2016-11-21 Mon 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/editor/movements.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun init-movements () "Initialize movements with my configuration." (interactive))

(use-package goto-last-change
  :ensure t)

(provide 'movements)
;;; movements.el ends here
