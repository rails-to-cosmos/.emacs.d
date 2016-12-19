;;; init-ido.el --- description
;;; Commentary:
;;; Code:

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(use-package ido
  :config (progn
            (setq-default
             ido-enable-flex-matching t
             ido-use-filename-at-point nil
             ido-auto-merge-work-directories-length -1
             ido-use-virtual-buffers t
             ido-confirm-unique-completion t
             ido-default-buffer-method 'selected-window)
            (ido-mode)))

(use-package idomenu
  :ensure t)

(use-package ido-vertical-mode
  :config (progn
            (setq-default
             ido-vertical-define-keys 'C-n-and-C-p-only)
            (ido-vertical-mode))
  :ensure t)

(use-package ido-completing-read+
  :ensure t)

(use-package ido-ubiquitous
  :config (ido-ubiquitous-mode)
  :ensure t)

(use-package smex
  ;; Smex is a M-x enhancement for Emacs, it provides a convenient interface to
  ;; your recently and most frequently used commands.
  :config (progn
            (global-set-key [remap execute-extended-command] 'smex))
  :ensure t)

(provide 'init-ido)
;;; init-ido.el ends here
