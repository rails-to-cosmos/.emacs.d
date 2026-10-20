;;; test-wrike-token-refresh.el --- Tests for Wrike token refresh -*- lexical-binding: t; -*-

(require 'ert)
(require 'wrike-token-refresh)

(ert-deftest wrike-token-refresh-env-preserves-other-lines ()
  (let* ((dir (make-temp-file "wrike-token-test-" t))
         (file (expand-file-name ".env" dir))
         (updates (make-hash-table :test 'equal))
         (value (concat "one'" "$` two")))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "# Keep this comment\nOTHER=untouched\nWRIKE_PAT=old\n"))
          (puthash "WRIKE_PAT" (wrike-token-refresh--quote value) updates)
          (puthash "ANTHROPIC_AUTH_TOKEN" "${AI_PROXY_ACCESS_TOKEN}" updates)
          (wrike-token-refresh--update-env file updates)
          (let ((contents (with-temp-buffer
                            (insert-file-contents file) (buffer-string))))
            (should (string-match-p "# Keep this comment\nOTHER=untouched" contents))
            (should (= 1 (with-temp-buffer
                           (insert contents)
                           (how-many "^WRIKE_PAT=" (point-min) (point-max)))))
            (should (string-match-p
                     (regexp-quote "ANTHROPIC_AUTH_TOKEN=${AI_PROXY_ACCESS_TOKEN}")
                     contents)))
          (should (equal value (gethash "WRIKE_PAT" (wrike-token-refresh--read-env file))))
          (when (executable-find "zsh")
            (with-temp-buffer
              (should (= 0 (call-process "zsh" nil t nil "-c"
                                         (concat "source " (shell-quote-argument file)
                                                 "; print -r -- $WRIKE_PAT"))))
              (should (equal (concat value "\n") (buffer-string)))))
          (should (= #o600 (file-modes file))))
      (delete-directory dir t))))

(ert-deftest wrike-token-refresh-jwt-expiry ()
  (let* ((payload (base64-encode-string "{\"iat\":100,\"exp\":200}" t))
         (token (concat "header." payload ".signature"))
         (state (make-hash-table :test 'equal)))
    (should (equal '(100 . 200) (wrike-token-refresh--jwt-claims token)))
    (wrike-token-refresh--record state "ai-proxy" token)
    (should (= 200 (gethash "expiresAt"
                            (gethash "ai-proxy" (gethash "tokens" state)))))))

(ert-deftest wrike-token-refresh-status-with-no-state ()
  (should (equal "no record"
                 (wrike-token-refresh--status (car wrike-token-refresh--tokens)
                                              (make-hash-table :test 'equal)))))

(ert-deftest wrike-token-refresh-json-roundtrip ()
  (let* ((dir (make-temp-file "wrike-token-test-" t))
         (file (expand-file-name "config.json" dir))
         (config (make-hash-table :test 'equal)))
    (unwind-protect
        (progn
          (puthash "profile" "both" config)
          (wrike-token-refresh--save-json file config)
          (should (equal "both" (gethash "profile" (wrike-token-refresh--json-file file))))
          (should (= #o600 (file-modes file))))
      (delete-directory dir t))))

(ert-deftest wrike-token-refresh-interactive-flow ()
  (let* ((dir (make-temp-file "wrike-token-test-" t))
         (wrike-token-refresh-config-file (expand-file-name "config.json" dir))
         (wrike-token-refresh-state-file (expand-file-name "state.json" dir))
         (wrike-token-refresh-default-env-file (expand-file-name ".env" dir))
         (config (make-hash-table :test 'equal))
         (answers '("proxy-example" "wrike-example" "glpat-example"))
         (emacs-values nil))
    (unwind-protect
        (progn
          (puthash "profile" "gui" config)
          (wrike-token-refresh--save-json wrike-token-refresh-config-file config)
          (cl-letf (((symbol-function 'wrike-token-refresh--show-token) #'ignore)
                    ((symbol-function 'wrike-token-refresh--gui-env) #'ignore)
                    ((symbol-function 'wrike-token-refresh--offer-restart) #'ignore)
                    ((symbol-function 'y-or-n-p)
                     (lambda (prompt) (string-prefix-p "Write " prompt)))
                    ((symbol-function 'read-passwd)
                     (lambda (&rest _) (prog1 (car answers) (setq answers (cdr answers)))))
                    ((symbol-function 'setenv)
                     (lambda (key value &rest _) (push (cons key value) emacs-values))))
            (wrike-token-refresh))
          (let ((env (wrike-token-refresh--read-env wrike-token-refresh-default-env-file))
                (state (wrike-token-refresh--json-file wrike-token-refresh-state-file)))
            (should (equal "proxy-example" (gethash "AI_PROXY_ACCESS_TOKEN" env)))
            (should (equal "wrike-example" (gethash "WRIKE_PAT" env)))
            (should (equal "glpat-example" (gethash "GITLAB_PERSONAL_ACCESS_TOKEN" env)))
            (should (equal "proxy-example" (cdr (assoc "ANTHROPIC_AUTH_TOKEN" emacs-values))))
            (should (gethash "ai-proxy" (gethash "tokens" state)))))
      (delete-directory dir t))))

(provide 'test-wrike-token-refresh)
;;; test-wrike-token-refresh.el ends here
