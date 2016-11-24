;;; init-bindings.el --- core emacs bindings
;;
;; Filename: init-bindings.el
;; Description: core emacs bindings
;; Author: Dmitry Akatov
;; Created: <2016-11-24 Thu 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/core/init-bindings.el
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

(defmacro define-context-key (keymap key dispatch)
  "Define in KEYMAP KEY to execute according to DISPATCH.

DISPATCH is a form that is evaluated and should return the
command to be executed.

If DISPATCH returns nil, then the command normally bound to KEY
will be executed."
  `(define-key ,keymap ,key
     `(menu-item "context-key" ignore
		 :filter ,(lambda (&optional ignored)
			    ,dispatch))))

(provide 'init-bindings)
;;; init-bindings.el ends here
