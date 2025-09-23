(require 'files)
(require 'strings)

(use-package mise
  :ensure t)

(cl-defun mise-trusted-p ()
  (->> (with-output-to-string (mise--call standard-output "trust" "--show"))
       (string-contains-p "untrusted")
       (not)))

(cl-defun mise-trust ()
  (when (yes-or-no-p "Mise config is untrusted. Trust it?")
    (message (with-output-to-string (mise--call standard-output "trust")))
    t))

(cl-defun mise-enable (project-root)
  (interactive)
  (when (and (executable-find "mise")
             (or (mise-trusted-p) (mise-trust)))
    (mise-mode)))

(cl-defun pyenv-enable (project-root)
  (pyvenv-mode)
  ;; (let* ((bin (file-truename (f-join project-root ".venv" "bin")))
  ;;        (path (->> (or (getenv "PATH") "")
  ;;                   (s-split ":")
  ;;                   (append (list bin) exec-path)
  ;;                   (seq-uniq))))
  ;;   (setenv "PATH" (s-join ":" path))
  ;;   (setq-local exec-path path))
  )

(use-package pyvenv
  :ensure t)

(use-package envrc
  :ensure t)

(cl-defun direnv (&rest project-files)
  (interactive)
  (or (cl-loop with project-directory
               with report
               with direnv-file-mode-map = '(("mise.toml" mise-enable)
                                             (".envrc" envrc-mode)
                                             (".venv" pyenv-enable))
               for (mode-file mode-hook) in direnv-file-mode-map
               for work-directory = (locate-dominating-file default-directory mode-file)
               when work-directory
               do (cl-pushnew (format "Mode file %s found in %s, executing #'%s" mode-file work-directory mode-hook)
                              report)
                  (funcall mode-hook work-directory)
                  (setq project-directory work-directory)
               finally do
                  (message (s-join "\n" report))
                  (cl-return project-directory))
      (cl-loop for project-file in project-files
               for work-directory = (locate-dominating-file default-directory project-file)
               when work-directory
               return work-directory)))

(provide 'direnv)
