;;; mijn-release.el --- Bump a package version and refresh its changelog -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "28.1") (agnostic-llm "0.4"))

;;; Commentary:
;;
;; `mijn-release' bumps the current project's version by running `make KIND'
;; (the major/minor/patch/... targets in the project's Makefile), then hands
;; the project's `agnostic-llm' session a prompt asking the LLM to update the
;; changelog from the git history.
;;
;; Requires `agnostic-llm' — it supplies the project-root resolver and the
;; session send used under the hood.  Drop this on `load-path' and bind or
;; `M-x mijn-release'.

;;; Code:

(require 'subr-x)
(require 'agnostic-llm)

(defgroup mijn-release nil
  "Bump a package version and refresh its changelog."
  :group 'tools
  :prefix "mijn-release-")

(defcustom mijn-release-bump-kinds '("patch" "minor" "major" "build" "rev")
  "Version-bump `make' targets offered by `mijn-release'.
Each is passed to `make' as its target in the project root, so the
project's Makefile must define it."
  :type '(repeat string)
  :group 'mijn-release)

(defcustom mijn-release-changelog-file "CHANGELOG.md"
  "Changelog file `mijn-release' asks the LLM to update.
A path relative to the project root."
  :type 'string
  :group 'mijn-release)

(defcustom mijn-release-prompt
  "I just bumped the project version with `make %1$s'%2$s.

Update %3$s (create it if absent, newest entry first, matching any existing
style) with a dated entry for this release.  Derive the user-facing changes
from the git history; recent commits for reference:

%4$s

Edit only %3$s.  Keep entries concise."
  "Prompt template `mijn-release' sends to the LLM.
Formatted with, in order: the bump KIND, a version clause such as \" to
1.2.3\" or empty, the changelog file (used twice), and a recent git log."
  :type 'string
  :group 'mijn-release)

(defun mijn-release--run-make (root target)
  "Run `make TARGET' in ROOT; return its output, or signal on failure."
  (let ((default-directory root))
    (with-temp-buffer
      (let ((status (call-process "make" nil t nil target)))
        (unless (eq status 0)
          (user-error "`make %s' failed: %s" target
                      (string-trim (buffer-string))))
        (buffer-string)))))

(defun mijn-release--git-log (root)
  "Return recent git-log lines for ROOT, or a placeholder string.
Uses commits since the last tag when one exists, else the last 40."
  (let ((default-directory root))
    (or (ignore-errors
          (let* ((tag (car (ignore-errors
                             (process-lines "git" "describe" "--tags" "--abbrev=0"))))
                 (args (append '("log" "--no-color" "--pretty=format:%h %s")
                               (if tag (list (concat tag "..HEAD")) '("-n" "40"))))
                 (lines (apply #'process-lines "git" args)))
            (and lines (string-join lines "\n"))))
        "(git history unavailable)")))

;;;###autoload
(defun mijn-release (kind)
  "Bump the project version, then ask the LLM to update the changelog.
Runs `make KIND' in the project root — exactly what `make major/minor/...'
does — then hands the project's `agnostic-llm' session a prompt
\(`mijn-release-prompt') to update `mijn-release-changelog-file' from the git
history.  KIND is completed from `mijn-release-bump-kinds'."
  (interactive
   (list (completing-read "Bump: " mijn-release-bump-kinds nil t
                          nil nil (car mijn-release-bump-kinds))))
  (let* ((root    (agnostic-llm--current-root))
         (out     (mijn-release--run-make root kind))
         (version (when (string-match "->[ \t]*\\([^ \t\n]+\\)" out)
                    (match-string 1 out)))
         (vclause (if version (format " to %s" version) ""))
         (prompt  (format mijn-release-prompt
                          kind vclause
                          mijn-release-changelog-file
                          (mijn-release--git-log root))))
    (message "Bumped (make %s)%s — asking the LLM to update %s"
             kind vclause mijn-release-changelog-file)
    (agnostic-llm--send-prompt prompt root)))

(provide 'mijn-release)
;;; mijn-release.el ends here
