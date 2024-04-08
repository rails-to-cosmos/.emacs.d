;; (put 'upcase-region 'disabled nil)
;; (put 'downcase-region 'disabled nil)
;; (put 'eval-expression 'disabled nil)
;; (put 'narrow-to-region 'disabled nil)
;; (put 'narrow-to-page 'disabled nil)
;; (put 'list-threads 'disabled nil)

;; (define-key global-map (kbd "C-x f") #'projectile-find-file)

;; (define-key global-map (kbd "C-x <RET>") #'execute-extended-command)

;; (require 'reverse-im)
;; (reverse-im-activate "russian-computer")

(use-package which-key
    :config
  (which-key-mode t)
  (which-key-setup-side-window-bottom)
  :ensure t)
