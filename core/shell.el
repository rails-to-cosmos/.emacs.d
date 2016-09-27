(defvar eshell-buffer-maximum-lines)
(setq eshell-buffer-maximum-lines 500)

(defvar eshell-output-filter-functions)
(setq eshell-output-filter-functions '(eshell-truncate-buffer
                                       eshell-postoutput-scroll-to-bottom
                                       eshell-handle-control-codes
                                       eshell-handle-ansi-color
                                       eshell-watch-for-password-prompt))

(defvar eshell-command-aliases-list)
(defun eshell-init-aliases()
  (add-to-list 'eshell-command-aliases-list '("ff" "find-file"))
  (add-to-list 'eshell-command-aliases-list '("d" "dired $1"))
  (add-to-list 'eshell-command-aliases-list '("l" "ls"))
  (add-to-list 'eshell-command-aliases-list '("ll" "ls -la")))
(add-hook 'eshell-mode-hook 'eshell-init-aliases)

(use-package exec-path-from-shell
  :config (progn
            (when (memq window-system '(mac ns))
              (exec-path-from-shell-initialize)
              (exec-path-from-shell-copy-env "SSH_AGENT_PID")
              (exec-path-from-shell-copy-env "SSH_AUTH_SOCK")
              (setenv "LANG" "en_US.UTF-8")
              (setenv "LC_ALL" "en_US.UTF-8")
              (setenv "LC_CTYPE" "en_US.UTF-8")))
  :ensure t)

(setq password-cache t
      password-cache-expiry 3600)

(provide 'shell)
