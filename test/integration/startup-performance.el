;;; startup-performance.el --- Interactive startup regression check -*- lexical-binding: t; -*-
;; Run after normal init with --fg-daemon; batch mode skips usage tracking.
(require 'cl-lib)

(add-hook
 'emacs-startup-hook
 (lambda ()
   (condition-case err
       (progn
         (when (or noninteractive init-file-had-error)
           (error "Interactive init did not complete cleanly"))
         (when-let* ((warnings (get-buffer "*Warnings*")))
           (with-current-buffer warnings
             (when (string-match-p "Error (use-package)" (buffer-string))
               (error "use-package reported a startup error"))))
         (unless (and (bound-and-true-p package-usage-mode)
                      package-usage--events)
           (error "Package usage tracking was not exercised"))
         (unless (= gc-cons-threshold mijn-startup-gc-cons-threshold)
           (error "Startup GC threshold was not restored"))
         (let ((elapsed (float-time (time-subtract (current-time) before-init-time)))
               (budget (string-to-number (or (getenv "EMACS_STARTUP_BUDGET") "20"))))
           (message "Interactive startup: %.3fs (budget %.1fs), %d GCs"
                    elapsed budget gcs-done)
           (when (> elapsed budget)
             (error "Interactive startup exceeded its time budget")))
         (dolist (feature '(magit lsp-metals org-re-reveal ox-reveal))
           (when (featurep feature)
             (error "Optional feature loaded eagerly: %s" feature)))
         (unless (eq (key-binding (kbd "C-x g")) #'magit-status)
           (error "Magit binding is missing"))
         (dolist (command '(magit-status dap-debug ox-reveal-layouts-menu
                            org-reveal-export-to-html org-re-reveal-export-to-html))
           (unless (commandp command)
             (error "Deferred command unavailable: %s" command)))
         (unless (advice-member-p #'mijn-org-load-presentation-exporters 'org-export-dispatch)
           (error "Export menu cannot load presentation backends"))
         (unless (string-match-p "<section" (org-export-string-as "* Slide\nContent" 'reveal t))
           (error "Direct presentation export did not render a slide"))
         (dolist (backend '(reveal re-reveal))
           (unless (org-export-get-backend backend)
             (error "Export backend unavailable: %s" backend)))
         (require 'scala-mode)
         (unless (and (featurep 'lsp-metals) (featurep 'dap-mode) (featurep 'lsp-ui))
           (error "Scala did not load its language tools"))
         (message "OK: interactive startup and deferred integrations")
         (kill-emacs 0))
     (error (message "Startup regression: %S" err) (kill-emacs 1))))
 t)
