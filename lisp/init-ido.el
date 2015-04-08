;; Use C-f during file selection to switch to regular find-file
(require 'ido)
(require-package 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length 0)
(setq ido-use-virtual-buffers t)

(when (eval-when-compile (>= emacs-major-version 24))
 (require-package 'ido-ubiquitous)
 (ido-ubiquitous-mode t))

;; Use smex to handle M-x
(when (eval-when-compile (>= emacs-major-version 24))
  (require-package 'smex)
  ;; Change path for ~/.smex-items
  ;; (setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
  (global-set-key [remap execute-extended-command] 'smex))

(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

(require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

;; http://www.reddit.com/r/emacs/comments/21a4p9/use_recentf_and_ido_together/cgbprem
(add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))




(provide 'init-ido)
