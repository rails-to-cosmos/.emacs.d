(require 'files)
(require 'strings)

(use-package mise
  :ensure t)

(cl-defun mise-trusted-p ()
  (->> (with-output-to-string (mise--call standard-output "trust" "--show"))
       (string-contains-p "untrusted")
       (not)))

(cl-defun mise-enable ()
  (interactive)
  (when (and (not (mise-trusted-p))
             (yes-or-no-p "Mise config is untrusted. Trust it?"))
    (message (with-output-to-string (mise--call standard-output "trust")))))

(use-package envrc
  :ensure t)

(cl-defun my-direnv ()
  (interactive)
  (let ((direnv-file-mode-map '(("mise.toml" mise-enable)
                                (".envrc" envrc-mode))))
    (cl-loop for (mode-file mode-hook) in direnv-file-mode-map
             when (locate-dominating-file default-directory mode-file)
             do (funcall mode-hook))))

(provide 'init-direnv)
