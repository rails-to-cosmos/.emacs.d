;;; init-shell.el --- provide shell settings
;;
;; Filename: init-shell.el
;; Description: provide shell settings
;; Author: Dmitry Akatov
;; Created: <2016-11-25 Fri 8:30am>
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d/core/init-shell.el
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(setq-default eshell-buffer-maximum-lines 500
              eshell-output-filter-functions '(eshell-truncate-buffer
                                               eshell-postoutput-scroll-to-bottom
                                               eshell-handle-control-codes
                                               eshell-handle-ansi-color
                                               eshell-watch-for-password-prompt)
              password-cache t
              password-cache-expiry 3600)

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

(provide 'init-shell)
;;; init-shell.el ends here
