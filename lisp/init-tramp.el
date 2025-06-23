(require 'tramp)

(use-package tramp
  :config (setq remote-file-name-inhibit-locks t
                tramp-use-scp-direct-remote-copying t
                remote-file-name-inhibit-auto-save-visited t
                tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                                        "-o ControlMaster=auto -o ControlPersist=yes")
                tramp-use-ssh-controlmaster-options nil
                tramp-auto-save-directory nil))

(provide 'init-tramp)
