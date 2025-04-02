(cl-defun ab-vpn-connect ()
  (interactive)
  (eshell-command "sudo wg-quick up wg-crypto"))

(cl-defun ab-vpn-disconnect ()
  (interactive)
  (eshell-command "sudo wg-quick down wg-crypto"))

(provide 'init-ab)
