(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'list-threads 'disabled nil)

(global-set-key (kbd "C-M-o") #'occur)

(use-package cask-mode
  :ensure t)

(use-package envrc
  :config (envrc-global-mode)
  :ensure t)

(use-package align
  :bind ("M-[" . align)
  :ensure t)

(use-package whitespace-cleanup-mode
  :config (progn
            (global-whitespace-cleanup-mode t)
            (add-hook 'before-save-hook 'delete-trailing-whitespace)

            (defun sanityinc/no-trailing-whitespace ()
              "Turn off display of trailing whitespace in this buffer."
              (setq show-trailing-whitespace nil))

            ;; But don't show trailing whitespace in SQLi, inf-ruby etc.
            (dolist (hook '(special-mode-hook
                            Info-mode-hook
                            eww-mode-hook
                            term-mode-hook
                            comint-mode-hook
                            compilation-mode-hook
                            twittering-mode-hook
                            minibuffer-setup-hook
                            eshell-mode-hook
                            prog-mode-hook))
              (add-hook hook #'sanityinc/no-trailing-whitespace)))
  :ensure t)

(use-package reverse-im
  :config (progn
            (reverse-im-activate "russian-computer"))
  :ensure t)

(use-package a
  :ensure t)

(use-package browse-kill-ring
  :config (progn
            (global-unset-key (kbd "M-y"))
            (global-set-key (kbd "M-y") #'browse-kill-ring)
            (define-key browse-kill-ring-mode-map (kbd "M-y") #'browse-kill-ring-forward)
            (define-key browse-kill-ring-mode-map (kbd "M-n") #'browse-kill-ring-forward)
            (define-key browse-kill-ring-mode-map (kbd "M-Y") #'browse-kill-ring-previous)
            (define-key browse-kill-ring-mode-map (kbd "M-p") #'browse-kill-ring-previous))
  :ensure t)

(use-package multiple-cursors
  :config (progn
            (define-key global-map (kbd "C-<") #'mc/mark-previous-like-this)
            (define-key global-map (kbd "C->") #'mc/mark-next-like-this)
            (define-key global-map (kbd "C-+") #'mc/mark-all-like-this))
  :ensure t)

(use-package expand-region
  :config (progn
            (global-set-key (kbd "M-2") #'er/expand-region))
  :ensure t)

(use-package undo-tree
  :config (progn
            (setq undo-tree-visualizer-timestamps t
                  undo-tree-visualizer-diff t
                  undo-tree-history-directory-alist (a-list ".*" (f-join user-emacs-directory "undo-tree")))
            (global-undo-tree-mode))
  :ensure t)

(use-package smartparens
  :config (progn
            (require 'smartparens)
            (require 'smartparens-config))
  :ensure t)

(use-package paredit
  :config (progn
            (add-hook 'lisp-mode-hook 'paredit-mode)
            (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
            (add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

            (defvar paredit-minibuffer-commands '(eval-expression
                                                  pp-eval-expression
                                                  eval-expression-with-eldoc
                                                  ibuffer-do-eval
                                                  ibuffer-do-view-and-eval)
              "Interactive commands for which paredit should be enabled in the minibuffer.")

            (defun conditionally-enable-paredit-mode ()
              "Enable paredit during lisp-related minibuffer commands."
              (if (memq this-command paredit-minibuffer-commands)
                  (enable-paredit-mode))))
  :ensure t)

(require 'haskell-mode)
(defun my-kill-line ()
  (interactive)
  (delete-horizontal-space)
  (cond ((bolp) (save-excursion
                  (call-interactively #'indent-for-tab-command)))
        ((looking-at ")") t)
        (t (insert " "))))

(advice-add #'kill-line :after #'my-kill-line)

(defun increment-number-at-point ()
  (interactive)
  (save-excursion
    (skip-chars-backward "\-0-9")
    (or (looking-at "\-?[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (1+ (string-to-number (match-string 0)))))))

(defun decrement-number-at-point ()
  (interactive)
  (skip-chars-backward "\-0-9")
  (or (looking-at "\-?[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

(global-set-key (kbd "C-+") #'increment-number-at-point)
(global-set-key (kbd "C--") #'decrement-number-at-point)
(global-set-key (kbd "C-c p") 'duplicate-dwim)
(define-key global-map (kbd "C-x k") #'kill-current-buffer)
(define-key global-map (kbd "C-x y d c") #'desktop-clear)
(define-key global-map (kbd "C-x y f f") #'toggle-frame-maximized)
(define-key global-map (kbd "C-x y d r") #'restart-emacs)

(defun my-rc ()
  (interactive)
  (find-file "~/.zshrc"))

(global-set-key (kbd "C-x y r c") #'my-rc)

(use-package yaml-mode
  :ensure t)

(setq create-lockfiles nil)

;; (use-package git-browse
;;   :bind ("C-c g" . git-browse-current-line)
;;   :quelpa (git-browse :fetcher github :repo "rails-to-cosmos/git-browse"))

(provide 'init-editor)
