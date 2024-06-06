(customize-set-variable
 'tramp-ssh-controlmaster-options
 (concat "-o ControlPath=/tmp/ssh-ControlPath-%%r@%%h:%%p "
         "-o ControlMaster=auto -o ControlPersist=yes"))

(customize-set-variable 'tramp-use-ssh-controlmaster-options nil)

(provide 'init-tramp)
