;;; wrike-token-refresh.el --- Refresh Wrike tokens from Emacs -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1"))

;;; Commentary:
;; Run M-x wrike-token-refresh.  No Node.js package is required.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defgroup wrike-token-refresh nil
  "Refresh Wrike access tokens."
  :group 'tools)

(defcustom wrike-token-refresh-default-env-file
  (expand-file-name "~/.wrike.env")
  "Default file for token assignments."
  :type 'file)

(defcustom wrike-token-refresh-config-file
  (expand-file-name "~/.config/wrike-tokens/config.json")
  "Preferences file shared with the npm tool."
  :type 'file)

(defcustom wrike-token-refresh-state-file
  (expand-file-name "~/.config/wrike-tokens/state.json")
  "Expiry tracking file shared with the npm tool."
  :type 'file)

(defconst wrike-token-refresh--tokens
  '(("ai-proxy" "AI_PROXY_ACCESS_TOKEN" "AI Proxy Access Token" 28800
     "https://auth.ai.wrke.in/auth/selfservice"
     "Create a token with the aiproxy scope.")
    ("wrike-pat" "WRIKE_PAT" "Wrike Personal Access Token" 28800
     "https://incubator-api-token-service.apps.wrke.in/token/issue"
     "Issue a token for account 5 with read/write access.")
    ("gitlab-pat" "GITLAB_PERSONAL_ACCESS_TOKEN" "GitLab Personal Access Token" nil
     "https://git.wrke.in/-/user_settings/personal_access_tokens"
     "Create a personal access token with the api scope. It starts with glpat-.")))

(defconst wrike-token-refresh--auto-vars
  '(("ANTHROPIC_BASE_URL" . "https://aiproxy.wrke.in/anthropic")
    ("DISABLE_TELEMETRY" . "1")
    ("DISABLE_ERROR_REPORTING" . "1")
    ("DISABLE_BUG_COMMAND" . "1")
    ("CLAUDE_CODE_DISABLE_EXPERIMENTAL_BETAS" . "1")
    ("GITLAB_URL" . "https://git.wrke.in")))

(defun wrike-token-refresh--json-file (file)
  (if (file-readable-p file)
      (condition-case err
          (with-temp-buffer
            (insert-file-contents file)
            (json-parse-buffer :object-type 'hash-table :array-type 'list))
        (error (user-error "Cannot read %s: %s" file (error-message-string err))))
    (make-hash-table :test 'equal)))

(defun wrike-token-refresh--write-private (file contents)
  (let* ((dir (file-name-directory file))
         (temp nil))
    (make-directory dir t)
    (unwind-protect
        (progn
          (setq temp (make-temp-file (expand-file-name ".wrike-token-" dir)))
          (set-file-modes temp #o600)
          (with-temp-file temp (insert contents))
          (rename-file temp file t)
          (setq temp nil))
      (when (and temp (file-exists-p temp)) (delete-file temp)))))

(defun wrike-token-refresh--save-json (file object)
  (let ((json-encoding-pretty-print t))
    (wrike-token-refresh--write-private
     file (concat (json-encode object) "\n"))))

(defun wrike-token-refresh--env-value (line)
  (when (string-match "^[ \t]*\\([A-Za-z_][A-Za-z0-9_]*\\)[ \t]*=\\(.*\\)$" line)
    (let ((value (string-trim (match-string 2 line))))
      (cons (match-string 1 line)
            (cond ((string-match "\\`'\\(.*\\)'\\'" value)
                   (replace-regexp-in-string (regexp-quote "'\\''") "'"
                                             (match-string 1 value) t t))
                  ((string-match "\\`\"\\(.*\\)\"\\'" value)
                   (match-string 1 value))
                  (t value))))))

(defun wrike-token-refresh--read-env (file)
  (let ((result (make-hash-table :test 'equal)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (dolist (line (split-string (buffer-string) "\n"))
          (when-let* ((pair (wrike-token-refresh--env-value line)))
            (puthash (car pair) (cdr pair) result)))))
    result))

(defun wrike-token-refresh--quote (value)
  (concat "'" (string-join (split-string value "'" nil) "'\\''") "'"))

(defun wrike-token-refresh--update-env (file updates)
  (let ((lines (if (file-exists-p file)
                   (with-temp-buffer
                     (insert-file-contents file)
                     (split-string (buffer-string) "\n"))
                 nil))
        (seen (make-hash-table :test 'equal)))
    (setq lines
          (mapcar (lambda (line)
                    (let* ((pair (wrike-token-refresh--env-value line))
                           (key (car-safe pair)))
                      (if (and key (gethash key updates))
                          (progn
                            (puthash key t seen)
                            (concat key "=" (gethash key updates)))
                        line)))
                  lines))
    (while (and lines (string-empty-p (car (last lines))))
      (setq lines (butlast lines)))
    (let (new-lines)
      (maphash (lambda (key value)
                 (unless (gethash key seen)
                   (push (concat key "=" value) new-lines)))
               updates)
      (when new-lines
        (when lines (setq lines (append lines '(""))))
        (setq lines (append lines (nreverse new-lines)))))
    (wrike-token-refresh--write-private file (concat (string-join lines "\n") "\n"))))

(defun wrike-token-refresh--jwt-claims (token)
  (condition-case nil
      (let* ((parts (split-string token "\\."))
             (payload (and (= (length parts) 3) (nth 1 parts)))
             (claims (and payload
                          (json-parse-string
                           (base64-decode-string
                            (replace-regexp-in-string "_" "/"
                             (replace-regexp-in-string "-" "+" payload t t) t t))
                           :object-type 'hash-table))))
        (when claims
          (let ((iat (gethash "iat" claims))
                (exp (gethash "exp" claims)))
            (when (or (numberp iat) (numberp exp))
              (cons (and (numberp iat) iat) (and (numberp exp) exp))))))
    (error nil)))

(defun wrike-token-refresh--record (state id value)
  (let* ((tokens (or (gethash "tokens" state)
                     (let ((table (make-hash-table :test 'equal)))
                       (puthash "tokens" table state) table)))
         (claims (wrike-token-refresh--jwt-claims value))
         (entry (make-hash-table :test 'equal)))
    (puthash "issuedAt" (or (car claims) (floor (float-time))) entry)
    (when (cdr claims) (puthash "expiresAt" (cdr claims) entry))
    (puthash id entry tokens)))

(defun wrike-token-refresh--status (token state)
  (let* ((tokens (gethash "tokens" state))
         (entry (and (hash-table-p tokens) (gethash (car token) tokens)))
         (expiry (and entry
                      (or (gethash "expiresAt" entry)
                          (and (nth 3 token) (gethash "issuedAt" entry)
                               (+ (gethash "issuedAt" entry) (nth 3 token))))))
         (remaining (and expiry (- expiry (float-time)))))
    (cond ((not entry) "no record")
          ((not expiry) "expiry unknown")
          ((<= remaining 0) (format "expired %s ago" (wrike-token-refresh--duration (- remaining))))
          (t (format "%s remaining%s"
                     (wrike-token-refresh--duration remaining)
                     (if (<= remaining 900) " (expiring)"
                       (if (<= remaining 3600) " (under 1 hour)" "")))))))

(defun wrike-token-refresh--duration (seconds)
  (let ((minutes (floor (/ seconds 60))))
    (cond ((>= minutes 60) (format "%dh %dm" (/ minutes 60) (% minutes 60)))
          ((> minutes 0) (format "%dm" minutes))
          (t "<1m"))))

(defun wrike-token-refresh--old-node-versions ()
  (let ((dir (expand-file-name "~/.nvm/versions/node/")))
    (when (file-directory-p dir)
      (cl-remove-if-not
       (lambda (name)
         (and (string-match "\\`v\\([0-9]+\\)\\." name)
              (< (string-to-number (match-string 1 name)) 18)))
       (directory-files dir nil "\\`v[0-9]+\\.")))))

;;;###autoload
(defun wrike-token-refresh-status ()
  "Show tracked Wrike token expiry without exposing token values."
  (interactive)
  (let ((state (wrike-token-refresh--json-file wrike-token-refresh-state-file)))
    (with-help-window "*Wrike token status*"
      (princ "Wrike token status\n\n")
      (dolist (token wrike-token-refresh--tokens)
        (princ (format "%s (%s): %s\n"
                       (nth 2 token) (nth 1 token)
                       (wrike-token-refresh--status token state))))
      (when-let* ((old (wrike-token-refresh--old-node-versions)))
        (princ (format "\nPre-18 nvm Node versions may break Claude Desktop MCP servers: %s\n"
                       (string-join old ", ")))))))

(defun wrike-token-refresh--profile (config reconfigure)
  (let* ((stored (gethash "profile" config))
         (migrated (pcase stored
                     ("designer" "gui")
                     ("developer" "terminal")
                     ((or "gui" "terminal" "both") stored))))
    (if (or reconfigure (not migrated))
        (completing-read "Use tokens in (GUI/terminal/both): "
                         '("gui" "terminal" "both") nil t nil nil "both")
      migrated)))

(defun wrike-token-refresh--source-line (file)
  (let ((quoted (shell-quote-argument file)))
    (format "[ -f %s ] && { set -a; source %s; set +a; }" quoted quoted)))

(defun wrike-token-refresh--offer-zshrc (file)
  (let* ((zshrc (expand-file-name "~/.zshrc"))
         (line (wrike-token-refresh--source-line file))
         (existing (if (file-readable-p zshrc)
                       (with-temp-buffer (insert-file-contents zshrc) (buffer-string))
                     "")))
    (when (and (not (string-match-p (regexp-quote file) existing))
               (y-or-n-p (format "Add source line for %s to ~/.zshrc? " file)))
      (with-temp-buffer
        (insert (if (or (string-empty-p existing) (string-suffix-p "\n" existing)) "" "\n"))
        (insert "\n# Wrike tokens (managed by wrike-token-refresh.el)\n" line "\n")
        (append-to-file (point-min) (point-max) zshrc))
      (message "Added source line to ~/.zshrc"))))

(defun wrike-token-refresh--call (program &rest args)
  (condition-case nil
      (eq 0 (apply #'call-process program nil nil nil args))
    (file-error nil)))

(defun wrike-token-refresh--gui-env (values)
  (when (eq system-type 'darwin)
    (maphash (lambda (key value)
               (unless (wrike-token-refresh--call "launchctl" "setenv" key value)
                 (display-warning 'wrike-token-refresh
                                  (format "Could not set %s in the GUI environment" key))))
             values)))

(defun wrike-token-refresh--claude-running-p ()
  (wrike-token-refresh--call "pgrep" "-x" "Claude"))

(defun wrike-token-refresh--show-token (token status)
  (let ((buffer (get-buffer-create "*Wrike token refresh*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "%s (%s)\n\nStatus: %s\n\n%s\n%s\n"
                        (nth 2 token) (nth 1 token) status
                        (nth 5 token) (nth 4 token)))
        (special-mode)))
    (display-buffer buffer)))

(defun wrike-token-refresh--offer-restart ()
  (when (and (eq system-type 'darwin)
             (wrike-token-refresh--claude-running-p)
             (y-or-n-p "Restart Claude Desktop to load the new tokens? "))
    (wrike-token-refresh--call "osascript" "-e" "tell application \"Claude\" to quit")
    (let ((deadline (+ (float-time) 20)))
      (while (and (wrike-token-refresh--claude-running-p)
                  (< (float-time) deadline))
        (sit-for 0.2)))
    (unless (wrike-token-refresh--call "open" "-a" "Claude")
      (display-warning 'wrike-token-refresh "Could not relaunch Claude Desktop"))))

;;;###autoload
(defun wrike-token-refresh (&optional reconfigure)
  "Interactively refresh Wrike tokens and update the environment.
With prefix argument RECONFIGURE, choose the GUI/terminal profile again."
  (interactive "P")
  (let* ((config (wrike-token-refresh--json-file wrike-token-refresh-config-file))
         (state (wrike-token-refresh--json-file wrike-token-refresh-state-file))
         (profile (wrike-token-refresh--profile config reconfigure))
         (saved-path (gethash "envFilePath" config))
         (file (expand-file-name
                (if (string= profile "gui")
                    (or saved-path wrike-token-refresh-default-env-file)
                  (read-file-name "Environment file: " nil
                                  (or saved-path wrike-token-refresh-default-env-file)))))
         (current (wrike-token-refresh--read-env file))
         (updates (make-hash-table :test 'equal))
         (resolved (make-hash-table :test 'equal))
         (records nil)
         (count 0))
    (dolist (token wrike-token-refresh--tokens)
      (let* ((id (nth 0 token))
             (key (nth 1 token))
             (old (gethash key current))
             (status (wrike-token-refresh--status token state)))
        (wrike-token-refresh--show-token token status)
        (when (y-or-n-p (format "Open %s token page? " (nth 2 token)))
          (browse-url (nth 4 token)))
        (let* ((new (read-passwd
                     (format "%s (blank keeps existing%s): " key
                             (if old "" "; required"))))
               (value (if (string-empty-p new) old new)))
          (unless (and value (not (string-empty-p value)))
            (user-error "%s is required; nothing was written" key))
          (puthash key value resolved)
          (when (not (string-empty-p new))
            (puthash key (wrike-token-refresh--quote new) updates)
            (push (cons id new) records))
          (when (and (string-empty-p new) (wrike-token-refresh--jwt-claims value))
            (push (cons id value) records)))))
    (dolist (pair wrike-token-refresh--auto-vars)
      (let ((key (car pair)) (value (cdr pair)))
        (puthash key (or (gethash key current) value) resolved)
        (unless (gethash key current)
          (puthash key (wrike-token-refresh--quote value) updates))))
    (puthash "ANTHROPIC_AUTH_TOKEN" (gethash "AI_PROXY_ACCESS_TOKEN" resolved) resolved)
    (puthash "ANTHROPIC_AUTH_TOKEN" "${AI_PROXY_ACCESS_TOKEN}" updates)
    (maphash (lambda (_ _) (cl-incf count)) updates)
    (unless (y-or-n-p (format "Write %d variable%s to %s? "
                              count (if (= count 1) "" "s") file))
      (user-error "Cancelled; nothing was written"))
    (wrike-token-refresh--update-env file updates)
    (puthash "profile" profile config)
    (puthash "envFilePath" file config)
    (wrike-token-refresh--save-json wrike-token-refresh-config-file config)
    (dolist (record records)
      (wrike-token-refresh--record state (car record) (cdr record)))
    (when records
      (wrike-token-refresh--save-json wrike-token-refresh-state-file state))
    (maphash #'setenv resolved)
    (when (member profile '("gui" "both"))
      (wrike-token-refresh--gui-env resolved))
    (when (member profile '("terminal" "both"))
      (wrike-token-refresh--offer-zshrc file))
    (when (member profile '("gui" "both"))
      (wrike-token-refresh--offer-restart))
    (when-let* ((old (wrike-token-refresh--old-node-versions)))
      (display-warning
       'wrike-token-refresh
       (format "Pre-18 nvm Node versions may break Claude Desktop MCP servers: %s"
               (string-join old ", "))))
    (message "Updated %s; %d variables written%s" file count
             (if (member profile '("terminal" "both"))
                 (format ". Run: source %s" (shell-quote-argument file)) ""))))

(provide 'wrike-token-refresh)
;;; wrike-token-refresh.el ends here
