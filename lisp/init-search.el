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

(use-package fzf
  :config (progn
            (require 'fzf)

            (defun fzf-project (&optional with-preview)
              "Starts an fzf session at the root of the current projectile project."
              (interactive "P")
              (pcase (my-project-root)
                (`(git ,dir) (fzf-git-files))
                (`(hg ,dir) (fzf-hg-files))
                (`(project ,dir) (let ((fzf/args (if with-preview (concat fzf/args " " fzf/args-for-preview) fzf/args))
                                       (fzf--target-validator (fzf--use-validator (function fzf--validate-filename))))
                                   (fzf--start () () #'fzf--action-find-file)))
                (otherwise 'nil))))
  :bind (("C-x f" . fzf-project))
  :ensure t)

(provide 'init-search)
