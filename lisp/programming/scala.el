(require 'expal)

;; (defun scala-expal:indent ()
;;   (interactive)
;;   (save-excursion
;;     (let* ((bounds (expal:bounds))
;;            (beg (car bounds))
;;            (end (cdr bounds)))
;;       (goto-char beg)
;;       (while (< (point) end)
;;         (scala-indent:indent-line)
;;         (forward-line))))
;;   (goto-char (max (point) (scala-syntax:beginning-of-code-line)))
;;   (when (looking-at "^\\W*$")  ;; empty line
;;     (scala-indent:indent-line)))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\|c\\)$"
  :config
  (setq scala-indent:step 2
        scala-indent:align-parameters t
        scala-indent:align-forms t)
  (add-to-list 'org-structure-template-alist '("scala" . "src amm"))
  (add-to-list 'ob-languages '(scala . t))
  (org-babel-do-load-languages 'org-babel-load-languages ob-languages))

;; (use-package sbt-amm
;;     :after (scala-mode)
;;     ;; :hook ;; (scala-mode . run-sbt-amm)
;;     ;;       ;; (post-command . sbt-amm-highlight-hook)
;;     :commands run-sbt-amm
;;     :bind (:map scala-mode-map
;; 		("C-c C-c" . sbt-amm-send)
;;                 ;; ("<tab>" . scala-expal:indent)
;;                 )
;;     :load-path "~/.emacs.d/packages")

(use-package sbt-mode
    :commands sbt-start sbt-command
    :config (progn
              (substitute-key-definition
               'minibuffer-complete-word
               'self-insert-command
               minibuffer-local-completion-map)
              (setq sbt:program-options '("-Dsbt.supershell=false")))
    :ensure t)

(require 'flycheck)
(add-hook 'scala-mode-hook #'flycheck-mode)

;; (use-package dap-mode
;;     :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode)
;;   :ensure t)

;; (use-package lsp-ui
;;     :ensure t)

(use-package lsp-mode
    :hook (scala-mode . lsp)
          (lsp-mode . lsp-lens-mode)
    :config (setq lsp-prefer-flymake nil)
    :ensure t)

(use-package lsp-metals
    :config (progn
              (lsp-metals-toggle-show-implicit-arguments)
              ;; (lsp-metals-toggle-show-implicit-conversions)
              (lsp-metals-toggle-show-inferred-type)
              ;; (lsp-metals-toggle-show-super-method-lenses)
              )
    :ensure t)

(use-package lsp-ui
    :ensure t)

;; (use-package company-lsp
;;     :ensure t)

(use-package posframe
    :ensure t)

(use-package dap-mode
    :ensure t)
