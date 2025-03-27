(require 'tramp)

(use-package tramp
  :custom
  (tramp-ssh-controlmaster-options (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
                                           "-o ControlMaster=auto -o ControlPersist=yes"))
  (tramp-use-ssh-controlmaster-options nil)
  (tramp-auto-save-directory nil))

(provide 'init-tramp)
