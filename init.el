;;; init.el --- my emacs configuration  -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; (add-to-list 'load-path (expand-file-name "src/parquet-mode" user-emacs-directory))

(require 'custom)
(require 'package)
(require 'warnings)

;; Native compilation of third-party packages can report calls into optional
;; integrations as undefined.  Keep the warnings in *Warnings* for diagnosis,
;; but do not pop that buffer up during startup.  This changes display only;
;; native compiler warnings and errors remain available in the warnings log.
(add-to-list 'warning-suppress-types '(native-compiler))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("rails-to-cosmos" . "https://rails-to-cosmos.github.io/elpa/")))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

;; Packages can load vterm while being byte-compiled, before its declaration
;; below is reached.  Never prompt on stdin during a batch/bootstrap run.
(setq vterm-always-compile-module t)

;; Keep the bootstrap manifest in version control.  `custom.el' is deliberately
;; ignored and may not exist on a fresh checkout, so it cannot be the only
;; record of packages needed while loading the modules below.
(defconst mijn-required-packages
  '(transient use-package a ace-window agnostic-translate browse-kill-ring buttercup
    cmake-font-lock cmake-mode company company-eask consult danneskjold-theme
    dap-mode dash default-text-scale diminish dired-narrow dired-rainbow
    disaster disk-usage dockerfile-mode eask eask-mode eglot-java eldoc-eask
    elm-mode envrc eshell-prompt-extras
    exec-path-from-shell expand-region f flycheck flycheck-eask flycheck-nim
    flymake-eask ggtags go-mode haskell-mode highlight-doxygen lsp-metals
    lsp-mode lsp-ui magit marginalia mise multiple-cursors nim-mode nix-mode
    ob-mermaid orderless org-contrib org-glance org-glance-llm org-re-reveal
    ox-reveal ox-reveal-layouts paredit posframe rainbow-delimiters rainbow-mode
    reverse-im rg rust-mode sbt-mode scala-mode session-buffer-cycle sly
    smartparens table-view table-view-native undo-tree vertico vterm
    whitespace-cleanup-mode yaml-mode yasnippet zig-mode agnostic-llm
    company-statistics company-quickhelp go-guru yasnippet-capf jinja2-mode
    poetry pyimpsort py-autopep8 flycheck-mypy flymake-ruff ruff-format
    lsp-pyright)
  "Packages required by the configuration's eagerly loaded modules.")

(defconst mijn-vc-packages '(darr)
  "Packages installed through `package-vc-install' by `use-package'.")

(defun mijn-sync-package-selected-packages ()
  "Expose the tracked package roots to package.el.
Keep selections made interactively or in `custom.el', while ensuring that
`package-autoremove' never mistakes configured packages for dependencies."
  (setq package-selected-packages
        (delete-dups
         (append mijn-required-packages
                 mijn-vc-packages
                 package-selected-packages))))

;; Provision the declared package set BEFORE loading any config module.  A
;; refresh here also replaces stale rolling-archive metadata whose package tar
;; files may already have disappeared from MELPA.
(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)) t)
(let ((required (delete-dups
                 (append mijn-required-packages package-selected-packages))))
  (when (seq-find (lambda (pkg) (not (package-installed-p pkg))) required)
    (package-refresh-contents)
    (dolist (pkg required)
      (unless (package-installed-p pkg)
        (ignore-errors (package-install pkg))))))
(mijn-sync-package-selected-packages)

(use-package diminish
  :ensure t)

(use-package dash
  :ensure t)

(use-package f
  :ensure t)

(use-package vterm
  :ensure t)

(use-package magit
  :ensure t)

(use-package rainbow-delimiters
  :ensure t)

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

;; LSP hack for SVG support.  `image-types' is absent in headless builds.
(when (boundp 'image-types)
  (cl-pushnew 'svg image-types))

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
(require 'mijn-restart)
(require 'repos)

(require 'mijn-darwin)

(require 'mijn-completion)
(require 'mijn-org)
(require 'mijn-editor)

(require 'mijn-dired)
(require 'mijn-terminal)
(require 'mijn-search)

(require 'mijn-git)

(require 'mijn-prog)                ; shared baseline for all programming modes
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
(require 'mijn-elm)
(require 'mijn-lisette)
(require 'mijn-nix)
(require 'mijn-docker)
(require 'mijn-os)
(require 'mijn-ab)
(require 'ray-cluster)              ; M-x table-view-ray-actors


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

(use-package table-view
  :config (progn
            (require 'table-view-csv)
            (add-to-list 'auto-mode-alist '("\\.csv\\'" . table-view-csv-mode))
            (add-to-list 'auto-mode-alist '("\\.tsv\\'" . table-view-csv-mode))
            (global-set-key (kbd "C-x y c") #'table-view-csv)))

;; Optional Rust backend: installed but not loaded until a large table (e.g. a
;; big CSV) requires it -- then `table-view-display' routes to it, or recommends
;; building its binary (M-x table-view-native-compile).
(use-package table-view-native
  :defer t
  :commands (table-view-native-display table-view-native-compile))

(use-package org-glance
  :bind (("C-x j" . org-glance-transient))
  :custom ((org-glance-plugins '(llm)))
  :init (org-glance-init "~/sync/views")
  :ensure org-glance-llm)

(global-set-key (kbd "C-x y m") #'make-menu)
(global-set-key (kbd "C-x y r a") #'table-view-ray-actors)
(global-set-key (kbd "C-x y r j") #'table-view-ray-jobs)
(global-set-key (kbd "C-x y r n") #'table-view-ray-nodes)
(global-set-key (kbd "C-x y r t") #'table-view-ray-tasks)
(global-set-key (kbd "C-x C-o") #'other-frame)

(load (setq custom-file (expand-file-name "custom.el" user-emacs-directory)) t)
(mijn-sync-package-selected-packages)

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((darr :url "https://github.com/rails-to-cosmos/darr.git" :branch
           "master"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
