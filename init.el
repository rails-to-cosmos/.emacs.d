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

(require 'my-ui)
(require 'my-tramp)
(require 'my-scratch)

(require 'my-darwin)

(require 'my-completion)
(require 'my-org)
(require 'my-editor)

(require 'my-glance)
(require 'my-dired)
(require 'my-terminal)
(require 'my-search)

(require 'my-git)
(require 'my-lisp)
(require 'my-haskell)
(require 'my-python)
(require 'my-sql)
(require 'my-bash)
(require 'my-zig)
(require 'my-scala)
(require 'my-nim)
(require 'my-go)
(require 'my-java)
(require 'my-c)
(require 'my-nix)
(require 'my-docker)
(require 'my-os)
(require 'my-ab)
(require 'my-prog)
(require 'my-llm)
(require 'xrandr)
(require 'xmobarrc-mode)

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
