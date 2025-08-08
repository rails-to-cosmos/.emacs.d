;; (require 'direnv)
;; (direnv-mode)

(require 'exec-path-from-shell)
(when (memq window-system '(mac ns gnulinux x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))
