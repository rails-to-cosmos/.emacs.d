(require 'nix-mode)
(require 'nix-company)

(defun nixos-rebuild ()
  (interactive)
  (let* ((passwd (read-passwd "Root passwd: "))
         (buffer (get-buffer-create "*nixos-rebuild*"))
         (proc (start-process "nixos-rebuild" buffer "sudo" "nixos-rebuild" "switch")))
    (process-send-string proc passwd)
    (process-send-string proc "\r")
    (process-send-eof proc)
    (set-process-sentinel proc #'(lambda (proc event)
                                   (when (string= (s-trim event) "finished")
                                     (bury-buffer (get-buffer-create "*nixos-rebuild*")))))
    (switch-to-buffer-other-window buffer)))

(defun nix-company-init ()
  (setq-local company-backends '(company-files company-lsp)))

(defun nix-set-keys ()
  (define-key nix-mode-map (kbd "C-c C-c") 'nixos-rebuild))

(with-eval-after-load 'nix-mode
  (progn
    (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                      :major-modes '(nix-mode)
                      :server-id 'nix))))

(add-hook 'nix-mode-hook #'nix-company-init)
(add-hook 'nix-mode-hook #'nix-set-keys)
(add-hook 'nix-mode-hook #'lsp-mode)
