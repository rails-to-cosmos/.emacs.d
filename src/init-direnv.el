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
  (when (executable-find "mise")
    (if (mise-trusted-p)
        (mise-mode)
      (when (yes-or-no-p "Mise config is untrusted. Trust it?")
        (message (with-output-to-string (mise--call standard-output "trust")))
        (mise-mode)))))

(use-package pyvenv
  :ensure t)

(cl-defun pyenv-enable ()
  (when-let (venv (f-join (locate-dominating-file default-directory ".venv") ".venv"))
    (pyvenv-activate venv)))

(use-package envrc
  :ensure t)

(cl-defun my-direnv ()
  (interactive)
  (let ((direnv-file-mode-map '(("mise.toml" mise-enable)
                                (".envrc" envrc-mode)
                                (".venv" pyenv-enable))))
    (cl-loop for (mode-file mode-hook) in direnv-file-mode-map
             for work-directory = (locate-dominating-file default-directory mode-file)
             when work-directory
             do (message "Mode file %s found in %s, executing #'%s" mode-file work-directory mode-hook)
                (funcall mode-hook))))

(provide 'init-direnv)
