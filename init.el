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

(cl-defun overwrite-mode (&optional arg)
  "Disable overwrite mode entirely."
  (interactive)
  (message "overwrite-mode is disabled"))

(remove-hook 'pre-command-hook 'overwrite-mode)

(let ((paths '("src" "packages")))
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

(require 'mijn-ui)
(require 'mijn-tramp)
(require 'mijn-scratch)
(require 'mijn-repos)

(require 'mijn-darwin)

(require 'mijn-completion)
(require 'mijn-org)
(require 'mijn-editor)

(require 'mijn-glance)
(require 'mijn-dired)
(require 'mijn-terminal)
(require 'mijn-search)

(require 'mijn-git)
(require 'mijn-lisp)
(require 'mijn-haskell)
(require 'mijn-python)
(require 'mijn-sql)
(require 'mijn-bash)
(require 'mijn-zig)
(require 'mijn-scala)
(require 'mijn-nim)
(require 'mijn-go)
(require 'mijn-java)
(require 'mijn-c)
(require 'mijn-nix)
(require 'mijn-docker)
(require 'mijn-os)
(require 'mijn-ab)
(require 'mijn-prog)
(require 'mijn-llm)
(require 'xrandr)
(require 'xmobarrc-mode)
(require 'network-manager)

(require 'make)

(global-set-key (kbd "C-x y m") #'make-completing-read)
(global-set-key (kbd "C-x C-o") #'other-frame)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)) t)

(condition-case nil
    (load-file (f-join user-emacs-directory "init-local.el"))
  (file-missing nil))

(put 'set-goal-column 'disabled nil)

;; (eval-after-load 'my-ui
;;   (progn
;;     (set-frame-font "-JB-JetBrains Mono NL-regular-normal-normal-*-11-*-*-*-m-0-iso10646-1" nil t)
;;     (set-frame-size (selected-frame) 310 82)
;;     (set-frame-position (selected-frame) 1182 24)))

;;; init.el ends here
