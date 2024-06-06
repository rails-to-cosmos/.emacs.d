;;; init.el --- my emacs configuration
;;; Commentary:
;;; Code:

(require 'custom)
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure nil)

(use-package diminish
  :ensure t)

(use-package dash
  :ensure t)

(use-package f
  :ensure t)

(let ((paths '("lisp" "packages")))
  (--map (cl-pushnew (f-join user-emacs-directory it) load-path) paths))

;; lsp hack for svg support to not break sessions
(setq image-types (cons 'svg image-types))

(with-eval-after-load 'undo-tree
  (diminish 'undo-tree-mode))

(with-eval-after-load 'grab-and-drag
  (diminish 'grab-and-drag-mode))

(with-eval-after-load 'paredit
  (diminish 'paredit-mode))

(with-eval-after-load 'auto-revert-mode
  (diminish 'auto-revert-mode))

(with-eval-after-load 'org-indent
  (diminish 'org-indent-mode))

(require 'init-ui)
(require 'init-tramp)
(require 'init-scratch)

(require 'init-darwin)

(require 'init-completion)
(require 'init-org)
(require 'init-editor)

(require 'init-glance)
(require 'init-dired)
(require 'init-direnv)
(require 'init-shell)
(require 'init-search)

(require 'init-git)
(require 'init-lisp)
(require 'init-haskell)
(require 'init-python)
(require 'init-zig)
(require 'init-scala)
(require 'init-nim)
(require 'init-go)
(require 'init-c)
(require 'init-nix)
(require 'init-docker)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

(condition-case nil
    (load-file (f-join user-emacs-directory "init-local.el"))
  (file-missing nil))

;;; init.el ends here
(put 'set-goal-column 'disabled nil)
