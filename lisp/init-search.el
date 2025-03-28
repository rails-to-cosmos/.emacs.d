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

;; (use-package fzf
;;   :load-path "~/sync/stuff/fzf.el/"
;;   :config (progn
;;             (require 'fzf)

;;             (defun fzf-project (&optional with-preview)
;;               "Starts an fzf session at the root of the current projectile project."
;;               (interactive "P")
;;               (pcase (my-project-root)
;;                 (`(git ,dir) (fzf-git-files))
;;                 (`(hg ,dir) (fzf-hg-files))
;;                 (`(project ,dir) (let ((fzf/args (if with-preview (concat fzf/args " " fzf/args-for-preview) fzf/args))
;;                                        (fzf--target-validator (fzf--use-validator (function fzf--validate-filename))))
;;                                    (fzf--start () () #'fzf--action-find-file)))
;;                 (otherwise 'nil))))
;;   :bind (("C-x f" . fzf-project))
;;   :ensure t)

(defun my-fzf ()
  (interactive)

  (let ((fzf/executable "fd")
        (fd-command (or (getenv "FZF_DEFAULT_COMMAND") "fd --type f --strip-cwd-prefix")))

    (unless (executable-find fzf/executable t)
      (user-error "Can't find executable '%s'. Is it in your OS PATH?"
                  fzf/executable))

    (find-file (completing-read "Find file: " (->> (save-window-excursion
                                                     (eshell-command fd-command)
                                                     (unwind-protect (with-current-buffer (get-buffer-create "*Eshell Command Output*")
                                                                       (buffer-substring-no-properties (point-min) (point-max)))
                                                       (bury-buffer "*Eshell Command Output*")))
                                                   (s-split "\n"))))))

(global-set-key (kbd "C-x f") 'my-fzf)

(provide 'init-search)
