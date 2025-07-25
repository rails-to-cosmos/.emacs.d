(require 'eshell)
(require 'esh-mode)
(require 'init-org)

(ob-add-language 'shell (cons "shell" "src shell"))
(ob-add-language 'eshell (cons "eshell" "src eshell"))

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize)
  :ensure t)

(use-package eshell-prompt-extras
  :ensure t)

(defun eshell-compl-space ()
  (interactive)
  (cond
   ((looking-back "cd" 1)
    (condition-case exc
        (progn
          (insert " ")
          (sit-for 0.05)
          (progn
            (insert (shell-quote-argument (read-directory-name "Choose directory: ")))
            (eshell-send-input)
            (insert "ls -la")
            (eshell-send-input)))
      ('quit
       (eshell-kill-input))))
   ((looking-back "\\./" 1)
    (condition-case exc
        (progn
          (sit-for 0.05)
          (insert (shell-quote-argument (file-relative-name (read-file-name "Choose file or directory: ")))))
      ('quit )))
   (t (insert " "))))

(defun eshell-compl-back ()
  (interactive)
  (cond
    ((or (equal (point-marker) eshell-last-input-start)
         (looking-back "λ " 1))
     (setq eshell-last-input-start (point-marker)))
    (t (delete-backward-char 1))))

(with-eval-after-load 'em-alias
    ;;; TODO: This conflicts with `evil-define-key' during the initialization of
    ;;; the first eshell session: the map in insert-mode will not take the changes
    ;;; into account. Going to normal mode and back to insert mode works.
  (eshell-read-aliases-list)
  (dolist
      (alias
       '(("l" "ls -l $*")
         ("la" "ls -lAh $*")
         ("ll" "ls -lh $*")
         ("cp" "*cp -i $*")
         ("mv" "*mv -i $*")
         ("mkdir" "*mkdir -p $*")
         ("mkcd" "*mkdir -p $* && cd $1")))
    (add-to-list 'eshell-command-aliases-list alias))
  (eshell-write-aliases-list))

(autoload 'epe-theme-lambda "eshell-prompt-extras")
(define-key global-map (kbd "C-x y e") #'eshell)

(defun eshell-set-keys ()
  (define-key eshell-mode-map (kbd "<SPC>") 'eshell-compl-space)
  (define-key eshell-mode-map (kbd "<backspace>") 'eshell-compl-back))

(add-hook 'eshell-first-time-mode-hook 'eshell-set-keys)

(require 'ol)

(defun eshell-browse-directory (dir)
  (interactive "sDirectory: ")
  (let ((eshell-buffer-name (format "*eshell:%s*" (file-name-directory dir))))
    (eshell)
    (insert (format "cd %s" dir))
    (eshell-send-input)
    (insert "ls -la")
    (eshell-send-input)))

(org-link-set-parameters
 "eshell"
 :follow #'eshell-browse-directory
 ;; :face 'org-glance-link-materialize-face
 ;; :complete 'org-glance-link:choose-thing-for-materialization
 ;; :export #'org-glance-link:export
 ;; :store #'org-glance-link:store-link
 )

(defun my-run-make-target-from-project ()
  "Go to project root, scan Makefile, let user choose a make target, and run it in eshell."
  (interactive)
  (save-window-excursion
    (when-let (project-root (locate-dominating-file default-directory "Makefile"))
      (let ((makefile (expand-file-name "Makefile" project-root)))
        (with-temp-buffer
          (insert-file-contents makefile)
          (let (targets)
            (goto-char (point-min))
            ;; Match lines like "target: deps"
            (while (re-search-forward "^\\([^#[:space:]\n][^:[:space:]]*\\):" nil t)
              (let ((target (match-string 1)))
                (unless (string= target ".PHONY")
                  (cl-pushnew target targets))))
            (let* ((choice (completing-read "Choose make target: " (sort targets #'string<)))
                   (cmd (concat "make " choice))
                   (default-directory project-root)
                   (make-output-buffer (let ((eshell-buffer-name "*Make Process Output*"))
                                         (when (get-buffer eshell-buffer-name)
                                           (kill-buffer eshell-buffer-name))
                                         (eshell))))
              (with-current-buffer make-output-buffer
                (insert cmd)
                (eshell-send-input))))))))
  (unless (equal (current-buffer) (get-buffer-create "*Make Process Output*"))
    (switch-to-buffer-other-window "*Make Process Output*")
    (other-window -1)))

(global-set-key (kbd "C-x y m") #'my-run-make-target-from-project)

(provide 'init-shell)
