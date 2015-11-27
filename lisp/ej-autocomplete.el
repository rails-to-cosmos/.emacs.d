(use-package popup)
(use-package pos-tip)
(use-package popup-kill-ring)
;; (use-package auto-complete-config)

;; (add-to-list 'ac-dictionary-directories (concat libfiles-dir "/ac-dict"))
(ac-config-default)
(setq ac-use-menu-map t)
(define-key ac-menu-map (kbd "ESC") 'viper-exit-popup-menu)
(define-key ac-complete-mode-map  (kbd "ESC") 'viper-exit-popup-menu)

(global-set-key "\M-y" 'popup-kill-ring)


;;(setq popup-kill-ring-interactive-insert t)


;;; describe inline
;; (defun my-describe-function (function)
;;   "Display the full documentation of FUNCTION (a symbol) in tooltip."
;;   (interactive (list (function-called-at-point)))
;;   (if (null function)
;;       (pos-tip-show
;;        "** You didn't specify a function! **" '("red"))
;;     (pos-tip-show
;;      (with-temp-buffer
;;        (let ((standard-output (current-buffer))
;;              (help-xref-following t))
;;          (prin1 function)
;;          (princ " is ")
;;          (describe-function-1 function)
;;          (buffer-string)))
;;      nil nil nil 0)))
;; (define-key emacs-lisp-mode-map (kbd "C-;") 'my-describe-function)

;;; autocomplete advice
;; (require 'popup-pos-tip)
;; (defadvice popup-tip
;;   (around popup-pos-tip-wrapper (string &rest args) activate)
;;   (if (eq window-system 'ns)
;;       (apply 'popup-pos-tip string args)
;;     ad-do-it))

(setq default-frame-alist
      '((width             . 155)
        (height            . 44)
        (tool-bar-lines . 0)
        (menu-bar-lines . 1)
        (background-color . "#111111")
        (background-mode . dark)
        (border-color . "black")
        (cursor-color . "white")
        (foreground-color . "white")
        ))

;; tab expansion with hippie and yas
;; from https://gist.github.com/215930

(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-complete-file-name-partially
        try-expand-all-abbrevs
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(use-package yasnippet)
(setq yas/root-directory "~/.emacs.d/snippets")
(yas/load-directory yas/root-directory)

;; Helps when debugging which try-function expanded
(setq hippie-expand-verbose t)
(yas/global-mode 1)

(defvar smart-tab-using-hippie-expand t
  "turn this on if you want to use hippie-expand completion.")

(defun smart-tab (prefix)
  "Needs `transient-mark-mode' to be on. This smart tab is
  minibuffer compliant: it acts as usual in the minibuffer.

  In all other buffers: if PREFIX is \\[universal-argument], calls
  `smart-indent'. Else if point is at the end of a symbol,
  expands it. Else calls `smart-indent'."
  (interactive "P")
  (labels ((smart-tab-must-expand (&optional prefix)
                                  (unless (or (consp prefix)
                                              mark-active)
                                    (looking-at "\\_>"))))
    (cond ((minibufferp)
           (minibuffer-complete))
          ((smart-tab-must-expand prefix)
           (if smart-tab-using-hippie-expand
               (hippie-expand prefix)
             (dabbrev-expand prefix)))
          ((smart-indent)))))

(defun smart-indent ()
  "Indents region if mark is active, or current line otherwise."
  (interactive)
  (if mark-active
    (indent-region (region-beginning)
                   (region-end))
    (indent-for-tab-command)))

;; Bind tab everywhere
(global-set-key (kbd "TAB") 'smart-tab)

;; Enables tab completion in the `eval-expression` minibuffer
(define-key read-expression-map [(tab)] 'hippie-expand)
(define-key read-expression-map [(shift tab)] 'unexpand)

;; Replace yasnippets's TAB
(add-hook 'yas/minor-mode-hook
          (lambda () (define-key yas/minor-mode-map
                       (kbd "TAB") 'smart-tab))) ; was yas/expand


(provide 'ej-autocomplete)
