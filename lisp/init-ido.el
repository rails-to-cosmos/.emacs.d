;; Use C-f during file selection to switch to regular find-file
(use-package ido
  :ensure t)
(use-package ido-vertical-mode
  :ensure t)
(use-package idomenu
  :ensure t)
(use-package ido-completing-read+
  :ensure t)

(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere t)

(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)

(use-package ido-ubiquitous
  :ensure t)

(ido-ubiquitous-mode t)

(use-package smex
  :ensure t)
;; Use smex to handle M-x

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))

(defun bind-ido-keys ()
  "Keybindings for ido mode."
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p")   'ido-prev-match))

(add-hook 'ido-setup-hook #'bind-ido-keys)


(provide 'init-ido)
