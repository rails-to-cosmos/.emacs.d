;;; init.el --- my emacs configuration
;;; Commentary:
;;; Code:

;; (add-to-list 'load-path (expand-file-name "src/parquet-mode" user-emacs-directory))

(require 'custom)
(require 'package)

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("rails-to-cosmos" . "https://rails-to-cosmos.github.io/elpa/")))

(package-initialize)

(setq package-selected-packages nil)
(advice-add 'package--save-selected-packages :override #'ignore)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(use-package diminish)
(use-package dash)
(use-package f)
(use-package vterm)
(use-package magit)
(use-package rainbow-delimiters)
(use-package table-view)

(use-package session-buffer-cycle
  :bind (("C-x C-x" . session-buffer-cycle))
  :custom (session-buffer-cycle-kinds '(("vterm" . (lambda (name _label _root)
                                                     (vterm name)))
                                        ("llm"   . (lambda (name _label _root)
                                                     (let ((vterm-shell "claude")) (vterm name)))))))

(cl-defun overwrite-mode (&optional arg)
  "Disable overwrite mode entirely."
  (interactive)
  (message "overwrite-mode is disabled"))

(remove-hook 'pre-command-hook 'overwrite-mode)

(let ((paths '("src" "src/repos" "src/network-manager" "src/parquet-mode" "packages")))
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
(require 'repos)

(require 'mijn-darwin)

(require 'mijn-completion)
(require 'mijn-org)
(require 'mijn-editor)

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
(require 'mijn-rust)
(require 'mijn-lisette)
(require 'mijn-nix)
(require 'mijn-docker)
(require 'mijn-os)
(require 'mijn-ab)
(require 'ray-cluster)              ; M-x table-view-ray-actors
(require 'mijn-prog)
(require 'parquet-mode)

(use-package agnostic-llm
  :bind (("C-x y e" . agnostic-llm-menu)
         ("C-S-j"   . agnostic-llm-next-buffer)
         ("C-S-k"   . agnostic-llm-previous-buffer))
  :config (with-eval-after-load 'vterm
            (define-key vterm-mode-map (kbd "C-c C-r") #'agnostic-llm-show-last-response)))

(use-package darr
  :vc (:url "https://github.com/rails-to-cosmos/darr.git" :branch "master" :rev :newest)
  :bind ("C-x y d i" . darr))

(use-package agnostic-translate
  :bind ("C-x y t r" . agnostic-translate))

(require 'xrandr)
(require 'xmobarrc-mode)
(require 'network-manager)

(require 'make)

(use-package table-view)

(use-package org-glance
  :bind (("C-x j" . org-glance-transient))
  :init (org-glance-init "~/sync/views"))

(global-set-key (kbd "C-x y m") #'make-menu)
(global-set-key (kbd "C-x y r a") #'table-view-ray-actors)
(global-set-key (kbd "C-x y r j") #'table-view-ray-jobs)
(global-set-key (kbd "C-x y r n") #'table-view-ray-nodes)
(global-set-key (kbd "C-x y r t") #'table-view-ray-tasks)
(global-set-key (kbd "C-x C-o") #'other-frame)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)) t)

;; Haskell-generated config (from emacs-config.hs; regenerate via `make emacs-config`)
(load (expand-file-name "generated.el" user-emacs-directory) t)

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
