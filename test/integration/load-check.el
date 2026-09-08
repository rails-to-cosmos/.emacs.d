;;; load-check.el --- Integration smoke test for the full config  -*- lexical-binding: t; -*-

;;; Commentary:
;; Loads the real init.el headlessly and asserts the configuration comes up
;; cleanly.  Run inside the integration container:
;;
;;   emacs --batch -l test/integration/load-check.el
;;
;; Exit non-zero on any error signalled during init, on a missing invariant,
;; or -- only when the environment variable EMACS_CONFIG_STRICT is set -- on any
;; warning that is not on the third-party/headless allowlist.

;;; Code:

;; Deliberately leave `debug-on-error' nil: init bootstraps packages and
;; catches per-package install failures with `ignore-errors'.  A backtrace for
;; those caught errors is noise.  We assert on what actually escapes the load
;; (via the condition-case below) and on the post-load invariants.

(defvar ic-strict
  (member (downcase (or (getenv "EMACS_CONFIG_STRICT") "")) '("1" "true" "yes" "on"))
  "When non-nil, unexpected warnings fail the run.
Note: `getenv' yields the string \"0\", which is truthy in Lisp, so test the
value rather than its mere presence.")

(defvar ic-allowed-warning-regexps
  '("mijn-fonts"                         ; headless has no fonts: warn-not-error path
    "Package cl-lib is deprecated"
    "Package .* is deprecated"
    "is obsolete"
    "Cannot load.*mise"                  ; optional external tools, absent in CI
    "Cannot load.*envrc")
  "Warnings tolerated even under strict mode.")

(defvar ic-required-features '(mijn-ui mijn-editor mijn-prog mijn-fonts
                               mijn-dired mijn-terminal mijn-git)
  "Features that must be provided after a successful init.")

(defvar ic-required-functions '(smartparens-strict-mode mijn-prog-setup)
  "Functions that must be bound after a successful init.")

(defvar ic-warnings nil "Collected `display-warning' payloads.")

(advice-add 'display-warning :before
            (lambda (type message &optional _level &rest _)
              (push (format "%s: %s" type message) ic-warnings)))

;; --- Load the real configuration -------------------------------------------
(let ((init (expand-file-name "init.el" user-emacs-directory)))
  (message "== loading %s on Emacs %s ==" init emacs-version)
  (condition-case err
      (load init nil nil nil t)
    (error
     (message "Integration: init aborted with an uncaught error:\n  %S" err)
     (kill-emacs 1))))

;; --- Invariants: prove the config actually wired itself up -----------------
(dolist (feat ic-required-features)
  (unless (featurep feat)
    (error "Integration: feature `%s' not provided after init" feat)))

(dolist (fn ic-required-functions)
  (unless (fboundp fn)
    (error "Integration: function `%s' unbound after init" fn)))

;; A prog-mode buffer must gain strict smartparens without error.
(with-temp-buffer
  (let ((prog-mode-hook '(mijn-prog-setup)))
    (prog-mode))
  (unless (bound-and-true-p smartparens-strict-mode)
    (error "Integration: smartparens-strict-mode not active in prog-mode")))

;; --- Warning ledger ---------------------------------------------------------
(let ((unexpected
       (seq-remove (lambda (w)
                     (seq-some (lambda (re) (string-match-p re w))
                               ic-allowed-warning-regexps))
                   ic-warnings)))
  (when ic-warnings
    (message "\n== %d warning(s) during init ==" (length ic-warnings))
    (dolist (w (reverse ic-warnings)) (message "  %s" w)))
  (when (and ic-strict unexpected)
    (error "Integration: %d unexpected warning(s) under strict mode"
           (length unexpected))))

(message "\nOK: init loaded clean on Emacs %s" emacs-version)
;;; load-check.el ends here
