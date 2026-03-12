
(show-paren-mode 1)

;; (use-package smartparens
;;     :config (require 'smartparens-config))

;; (use-package paredit
;;     :config (progn
;;               ;; (use-package paredit-everywhere
;;               ;;   :ensure t)
;;               ;; (add-hook 'prog-mode-hook 'paredit-everywhere-mode)
;;               ;; (add-hook 'css-mode-hook 'paredit-everywhere-mode)
;;               )
;;     :ensure t)

(require 'smartparens)
(require 'paredit)
(require 'smartparens-config)

(defun maybe-map-paredit-newline ()
  (unless (or (memq major-mode '(inferior-emacs-lisp-mode cider-repl-mode))
              (minibufferp))
    (local-set-key (kbd "RET") 'paredit-newline)))

(add-hook 'paredit-mode-hook 'maybe-map-paredit-newline)
(add-hook 'prog-mode-hook 'paredit-everywhere-mode)

;; (after-load 'paredit
;; 	    (diminish 'paredit-mode " Par")
;; 	    (dolist (binding (list (kbd "C-<left>") (kbd "C-<right>")
;; 				   (kbd "C-M-<left>") (kbd "C-M-<right>")))
;; 	      (define-key paredit-mode-map binding nil))
;; 	    (define-key paredit-mode-map [remap kill-sentence] nil)
;; 	    (define-key paredit-mode-map [remap backward-kill-sentence] nil)
;; 	    (define-key paredit-mode-map (kbd "M-?") nil))

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
      (enable-paredit-mode)))
