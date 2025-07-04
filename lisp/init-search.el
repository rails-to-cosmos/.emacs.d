(require 'files)
(require 'f)

(cl-defun my-project-root (&optional (dir (file-truename default-directory)))
  "Find the root directory by traversing upwards from DIR."
  (cond ((f-root? dir) '(nil nil))
        ((f-exists? (f-join dir ".git")) `(git ,dir))
        ((f-exists? (f-join dir ".hg")) `(hg ,dir))
        ((f-exists? (f-join dir ".project")) `(project ,dir))
        (t (my-project-root (f-parent dir)))))

(use-package rg
  :bind (("C-c r" . rg-project))
  :ensure t)

(defun my-fzf ()
  (interactive)

  (let ((fzf/executable "fd")
        (fd-command (or (getenv "FZF_DEFAULT_COMMAND") "fd --type f --strip-cwd-prefix"))
        (default-directory (cadr (my-project-root))))

    (unless (executable-find fzf/executable t)
      (user-error "Can't find executable '%s'. Is it in your OS PATH?"
                  fzf/executable))

    (->> (shell-command-to-string fd-command)
         (s-trim)
         (s-split "\n")
         (completing-read "Find file: ")
         (find-file))))

(global-set-key (kbd "C-x f") 'my-fzf)

(provide 'init-search)
