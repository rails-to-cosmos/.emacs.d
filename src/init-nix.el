(use-package nix-mode
  :hook ((nix-mode . smartparens-strict-mode))
  :ensure t)

;; --- Nix Flake Environment Loader (No Direnv) ---

;; (defvar my-nix-flake-env-cache (make-hash-table :test 'equal)
;;   "A cache for Nix Flake environments. Maps project root to an alist of env vars.")

;; (defun my-nix-flake--parse-env (env-string)
;;   "Parse the output of the 'env' command into an alist."
;;   (with-temp-buffer
;;     (insert env-string)
;;     (goto-char (point-min))
;;     (let (vars)
;;       (while (not (eobp))
;;         (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
;;                (parts (split-string line "=" 2)))
;;           (when (cdr parts) ; Ensure there was a '='
;;             (push (cons (car parts) (cadr parts)) vars)))
;;         (forward-line 1))
;;       vars)))

;; (defun my-nix-flake--get-env (project-root)
;;   "Get the environment for a project root, using a cache.
;; This is the slow part as it may invoke 'nix develop'."
;;   (or (gethash project-root my-nix-flake-env-cache)
;;       (let* ((default-directory project-root)
;;              (nix-command "nix develop --command sh -c 'env'")
;;              (env-output (shell-command-to-string nix-command)))
;;         (message "Nix: Activating flake environment for %s..." project-root)
;;         (let ((parsed-env (my-nix-flake--parse-env env-output)))
;;           (puthash project-root parsed-env my-nix-flake-env-cache)
;;           parsed-env))))

;; (defun my-nix-flake-activate-for-buffer ()
;;   "Check for a flake.nix and activate the environment for the current buffer."
;;   (interactive)
;;   (when-let (project-root (locate-dominating-file default-directory "flake.nix"))
;;     (let* ((env-alist (my-nix-flake--get-env project-root))
;;            (new-path (cdr (assoc "PATH" env-alist)))
;;            (original-env process-environment))

;;       (setq-local exec-path (append (split-string new-path path-separator) exec-path))

;;       ;; (setq-local exec-path (cons nix-path exec-path))

;;       (setq-local process-environment
;;                   (append (mapcar (lambda (cell) (format "%s=%s" (car cell) (cdr cell))) env-alist)
;;                           original-env))

;;       (cl-loop initially (message "Exec path:")
;;                for path in exec-path
;;                do (message path))

;;       ;; ;; 2. Set exec-path for interactive commands (like M-x compile)
;;       ;; (when new-path
;;       ;;   (setq-local exec-path (append (split-string new-path path-separator) exec-path)))



;;       )))

;; (defun my-nix-flake-reset-cache-for-project ()
;;   "Clear the cached Nix environment for the current project.

;; This forces the environment to be re-evaluated the next time a file
;; in this project is accessed."
;;   (interactive)
;;   (let ((project-root (locate-dominating-file buffer-file-name "flake.nix")))
;;     (if project-root
;;         (if (remhash project-root my-nix-flake-env-cache)
;;             (message "Nix cache reset for project: %s" project-root)
;;           (message "No active cache was found for project: %s" project-root))
;;       (message "Not inside a Nix flake project."))))

;; Activate the environment when opening files in programming modes.
;; (add-hook 'prog-mode-hook #'my-nix-flake-activate-for-buffer)

;; (message "Custom Nix Flake environment loader is active.")

(provide 'init-nix)
