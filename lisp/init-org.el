(require 'org)
(require 'org-clock)
(require 'ob)

(global-set-key (kbd "C-c o l") #'org-store-link)

(setq org-confirm-elisp-link-function
      (lambda (link) (not (string-prefix-p "(my-docker-eshell" link))))

(setq org-element-use-cache nil
      org-adapt-indentation t
      org-attach-auto-tag nil
      org-catch-invisible-edits 'error
      org-clock-auto-clock-resolution 'when-no-clock-is-running
      org-clock-in-resume t
      org-clock-in-switch-to-state "STARTED"
      org-clock-into-drawer t
      org-clock-out-remove-zero-time-clocks t
      org-clock-out-switch-to-state "PENDING"
      org-clock-out-when-done '("DONE" "CANCELLED")
      org-clock-persist t
      org-clock-persist-query-resume nil
      org-clock-report-include-clocking-task t
      org-confirm-babel-evaluate nil
      org-cycle-separator-lines 0
      org-deadline-warning-days 30
      org-edit-timestamp-down-means-later t
      org-ellipsis " ↓"
      org-fast-tag-selection-single-key 'expert
      org-glance-clone-on-repeat-p t
      org-glance:log-level 800
      org-hide-block-startup nil
      org-hide-emphasis-markers t
      org-hide-leading-stars t
      org-highlight-latex-and-related '(latex entities)
      org-indirect-buffer-display 'other-window
      org-latex-inputenc-alist '(("utf8" . "utf8x"))
      org-link-elisp-confirm-function 'yes-or-no-p
      org-link-shell-confirm-function 'yes-or-no-p
      org-log-done 'time
      org-log-into-drawer t
      org-log-note-clock-out nil
      org-log-note-headings '((done . "CLOSING NOTE %t")
                              (state . "State %-12s from %-12S %t")
                              (note . "Note taken on %t")
                              (reschedule . "Rescheduled from %S on %t")
                              (delschedule . "Not scheduled, was %S on %t")
                              (redeadline . "New deadline from %S on %t")
                              (deldeadline . "Removed deadline, was %S on %t")
                              (refile . "Refiled on %t")
                              (clock-out . "Clocked out on %t"))
      org-log-redeadline t
      org-log-reschedule t
      org-log-state-notes-insert-after-drawers nil
      org-outline-path-complete-in-steps t
      org-read-date-prefer-future 'time
      org-special-ctrl-a/e t
      org-special-ctrl-k t
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-src-window-setup 'current-window
      org-startup-align-all-tables "align"
      org-startup-indented t
      org-startup-with-inline-images t
      org-tags-column 80
      org-todo-keywords '((sequence "TODO(t)" "STARTED(s!)" "DELEGATED(e@/!)" "PENDING(p!)" "|" "DONE(d!)" "CANCELLED(c!)"))
      org-use-property-inheritance t
      org-use-tag-inheritance t)

(cl-pushnew 'org-checklist org-modules)

(unless org-modules-loaded (org-load-modules-maybe t))

(defun org-clock-goto-directory ()
  (interactive)
  (org-clock-goto)
  (dired (org-attach-dir)))

(define-key global-map (kbd "C-c C-x C-j") #'org-clock-goto)
(define-key global-map (kbd "C-c C-x C-d") #'org-clock-goto-directory)

(require 'ob)
(defvar ob-languages '())

(cl-defun ob-add-language (language &rest templates)
  (cl-loop for tpl in templates
           do (cl-pushnew tpl org-structure-template-alist))

  (cl-pushnew (cons language t) ob-languages)

  (condition-case err
      (org-babel-do-load-languages 'org-babel-load-languages ob-languages)
    (error
     (setq ob-languages (remove (cons language t) ob-languages))
     (user-error "Unable to register language: %s" err))))

(use-package ob-mermaid
  :config (progn
            (ob-add-language 'mermaid (cons "mermaid" "src mermaid :file test.png")))
  :ensure t)

(define-completion org-complete-structure "<"
  "Complete org-structure template alist."
  (when (and (string= major-mode "org-mode")
             (not (org-in-src-block-p))
             (looking-back "^<" 1))
    (let* ((choice (org-completing-read "Template: " org-structure-template-alist))
           (item (cdr (assoc choice org-structure-template-alist))))
      (org-insert-structure-template item)
      (insert "\n")
      (previous-line)
      (previous-line)
      (delete-backward-char 1)
      (forward-line)
      t)))

(define-completion org-complete-rarrow "->"
  (when (and (string= major-mode "org-mode")
             (not (org-in-src-block-p)))
    (insert "→")
    t))

(defun org-add-note-to-current-task ()
  "Prompt for a log message and add it to the currently clocked-in org-mode task."
  (interactive)
  (when (org-clock-is-active)
    (save-window-excursion
      (org-clock-goto)
      (org-add-note))))

(global-set-key (kbd "C-x y l") #'org-add-note-to-current-task)

(provide 'init-org)
