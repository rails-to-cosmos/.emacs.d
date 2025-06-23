(require 'tramp)

(use-package tramp
  ;; https://coredumped.dev/2025/06/18/making-tramp-go-brrrr./
  :config (progn

            (setq remote-file-name-inhibit-locks t
                  tramp-use-scp-direct-remote-copying t
                  remote-file-name-inhibit-auto-save-visited t
                  tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                                          "-o ControlMaster=auto -o ControlPersist=yes")
                  tramp-use-ssh-controlmaster-options nil
                  tramp-auto-save-directory nil
                  tramp-copy-size-limit (* 1024 1024) ;; 1MB
                  tramp-verbose 2

                  magit-tramp-pipe-stty-settings 'pty)

            (connection-local-set-profile-variables 'remote-direct-async-process '((tramp-direct-async-process . t)))
            (connection-local-set-profiles '(:application tramp :machine "server") 'remote-direct-async-process))

  :ensure magit)

(provide 'init-tramp)
