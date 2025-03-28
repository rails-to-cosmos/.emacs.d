;; -*- lexical-binding: t -*-

(require 'rainbow-delimiters)
(require 'paredit)

;; (use-package elsa
;;   :ensure elsa-flycheck
;;   :ensure t)

(use-package eask
  :config (progn
            (require 'eask-mode))
  :ensure t
  :ensure eask-mode)

(use-package buttercup
  :ensure t)

;; (require 'eval-sexp-fu)

(add-hook 'lisp-data-mode-hook 'smartparens-strict-mode)

(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'lisp-mode-hook 'enable-paredit-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
;; (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'yas-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'company-mode)

(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'my/expand-region)

;; (add-hook 'emacs-lisp-mode-hook 'emacs-lisp-completion-setup)
;; (cl-defun emacs-lisp-completion-setup ()
;;   (setq-local company-backends '(company-elisp company-files company-yasnippet)))

(require 'expand-region)

(defvar my/expand-region-last-bounds nil
  "Stores the previous region bounds for manual contraction.")

(defvar my/expand-region-last-point nil
  "Stores the previous cursor position.")

(cl-defun my/expand-region-outside-pairs-expand ()
  (interactive)

  (let* ((initial-region (cons (region-beginning) (region-end)))
         (expanded-region nil))

    (er/mark-outside-pairs)

    (setq expanded-region (cons (region-beginning) (region-end)))

    (if (equal initial-region expanded-region)
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "C-c") #'(lambda () (interactive)))
          (my/expand-region-outside-pairs-eval)
          (set-transient-map map t))
      (unless (equal initial-region (car my/expand-region-last-bounds))
        (push initial-region my/expand-region-last-bounds)))))

(cl-defun my/expand-region-outside-pairs-contract ()
  (interactive)
  (if my/expand-region-last-bounds
      (cl-destructuring-bind (beg . end) (pop my/expand-region-last-bounds)
        (set-mark end)
        (goto-char beg))
    (my/expand-region-outside-pairs-quit)))

(cl-defun my/expand-region-outside-pairs-eval ()
  (interactive)
  (unwind-protect
      (condition-case result
          (eval-region (region-beginning) (region-end) t)
        (error (message "%s" result)))
    (my/expand-region-outside-pairs-quit)))

(cl-defun my/expand-region-outside-pairs-quit ()
  (interactive)
  (deactivate-mark)
  (goto-char my/expand-region-last-point))

(cl-defun my/expand-region ()
  "Call `er/expand-region` and enable a temporary keymap with custom actions."
  (interactive)

  (setq my/expand-region-last-point (point)
        my/expand-region-last-bounds (list))

  (let ((map (make-sparse-keymap)))

    (er/mark-outside-pairs)

    ;; expand
    (define-key map (kbd "C-c") #'my/expand-region-outside-pairs-expand)
    (define-key map (kbd "c") #'my/expand-region-outside-pairs-expand)
    (define-key map (kbd "+") #'my/expand-region-outside-pairs-expand)
    (define-key map (kbd "=") #'my/expand-region-outside-pairs-expand)
    (define-key map (kbd "n") #'my/expand-region-outside-pairs-expand)

    ;; shrink
    (define-key map (kbd "C--") #'my/expand-region-outside-pairs-contract)
    (define-key map (kbd "-") #'my/expand-region-outside-pairs-contract)
    (define-key map (kbd "p") #'my/expand-region-outside-pairs-contract)

    ;; evaluate
    (define-key map (kbd "C-e") #'my/expand-region-outside-pairs-eval)
    (define-key map (kbd "C-j") #'my/expand-region-outside-pairs-eval)
    (define-key map (kbd "e") #'my/expand-region-outside-pairs-eval)
    (define-key map (kbd "j") #'my/expand-region-outside-pairs-eval)

    ;; quit
    (define-key map (kbd "C-q") #'my/expand-region-outside-pairs-quit)
    (define-key map (kbd "q") #'my/expand-region-outside-pairs-quit)
    (define-key map (kbd "ESC") #'my/expand-region-outside-pairs-quit)

    (set-transient-map map t)))

;; (use-package elsa
;;   :ensure flycheck-elsa
;;   :ensure t)

(provide 'init-lisp)
