;;; init-org.el --- my org-mode setup
;;; Commentary:
;;; Code:

(use-package org
  :diminish (org-indent-mode)
  :bind (("C-c l" . org-store-link)
         ("C-c a" . org-agenda)
         ("C-c c" . org-capture)
         :map org-mode-map
         ;; ("C-c C-l" . my-org-insert-link)
         ("C-M-n" . ded/org-show-next-heading-tidily)
         ("C-M-p" . ded/org-show-previous-heading-tidily))
  :config (progn
            (setq org-cycle-include-plain-lists 'integrate)
            (setq org-confirm-elisp-link-not-regexp "org-open-file")

            ;; Mark heading done when all checkboxes are checked
            ;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
            (eval-after-load 'org-list
              '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

            (defun ndk/checkbox-list-complete ()
              (save-excursion
                (org-back-to-heading t)
                (let ((beg (point)) end)
                  (end-of-line)
                  (setq end (point))
                  (goto-char beg)
                  (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
                      (if (match-end 1)
                          (if (equal (match-string 1) "100%")
                              ;; all done - do the state change
                              (org-todo 'done)
                            (org-todo 'todo))
                        (if (and (> (match-end 2) (match-beginning 2))
                                 (equal (match-string 2) (match-string 3)))
                            (org-todo 'done)
                          (org-todo 'todo)))))))

            (defun ded/org-show-next-heading-tidily ()
              "Show next entry, keeping other entries closed."
              (interactive)
              (if (save-excursion (end-of-line) (outline-invisible-p))
                  (progn (org-show-entry) (show-children))
                (org-forward-heading-same-level 1 t)
                (unless (and (bolp) (org-on-heading-p))
                  (org-up-heading-safe)
                  (hide-subtree)
                  (error "Boundary reached"))
                (org-overview)
                (org-reveal t)
                (org-show-entry)
                (show-children)))

            (defun ded/org-show-previous-heading-tidily ()
              "Show previous entry, keeping other entries closed."
              (interactive)
              (let ((pos (point)))
                (org-backward-heading-same-level 1 t)
                (unless (and (< (point) pos) (bolp) (org-on-heading-p))
                  (goto-char pos)
                  (hide-subtree)
                  (error "Boundary reached"))
                (org-overview)
                (org-reveal t)
                (org-show-entry)
                (show-children)))

            (setq org-hide-emphasis-markers t)
            (add-to-list 'org-emphasis-alist
                         '())
            (add-to-list 'org-emphasis-alist
                         '("_" ()))

            (use-package org-fstree
              :ensure t)

            (use-package org-crypt
              :disabled t
              :config
              (org-crypt-use-before-save-magic)
              (setq org-tags-exclude-from-inheritance (quote ("crypt")))
              (setq org-crypt-key nil))

            (defun org-archive-done-tasks ()
              (interactive)
              (org-map-entries 'org-archive-subtree "/DONE" 'file)
              (org-map-entries 'org-archive-subtree "/CANCELLED" 'file))

            (setq-default
             ;; org-log-done t
             org-special-ctrl-a/e t
             org-completion-use-ido t
             org-edit-timestamp-down-means-later t
             org-agenda-start-on-weekday nil
             org-agenda-span 14
             org-agenda-include-diary t
             org-agenda-window-setup 'current-window
             org-fast-tag-selection-single-key 'expert
             org-export-kill-product-buffer-when-displayed t
             org-tags-column 80
             org-refile-use-outline-path (quote file)
             org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5)))
             org-outline-path-complete-in-steps t
             org-ellipsis " ↓"
             org-hide-leading-stars t
             org-startup-indented t
             org-id-locations-file (tmp/ "org-id-locations.txt")
             org-todo-keywords (quote ((sequence "TODO(t)" "STARTED(s)" "DELEGATED(D@/!)" "TESTING(T)" "PREPARED(p)" "|" "DONE(d)")
                                       (sequence "WAITING(w)" "SOMEDAY(S)" "|" "CANCELLED(c)"))))

            (use-package org-clock
              :init (progn
                      (setq-default
                       org-clock-persistence-insinuate t
                       org-clock-persist t
                       org-clock-in-resume t
                       org-clock-in-switch-to-state "STARTED"
                       org-clock-out-remove-zero-time-clocks t)))

            (use-package org-dashboard
              :ensure t)

            (use-package org-babel
              :init (progn
                      (use-package ob-ipython
                        :config (progn
                                  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)
                                  (setq python-shell-prompt-detect-failure-warning nil)

                                  (defun ob-ipython-inline-image (b64-string)
                                    "Write the b64-string to a temporary file. Returns an org-link to the file."
                                    (let* ((tfile (make-temp-file "ob-ipython-" nil ".png"))
                                           (link (format "[[file:%s]]" tfile)))
                                      (ob-ipython--write-base64-string tfile b64-string)
                                      link))

                                  (defun org-babel-execute:ipython (body params)
                                    "Execute a block of IPython code with Babel. This function is called by `org-babel-execute-src-block'."
                                    (let* ((file (cdr (assoc :file params)))
                                           (session (cdr (assoc :session params)))
                                           (result-type (cdr (assoc :result-type params))))
                                      (org-babel-ipython-initiate-session session params)
                                      (-when-let (ret (ob-ipython--eval
                                                       (ob-ipython--execute-request
                                                        (org-babel-expand-body:generic (encode-coding-string body 'utf-8)
                                                                                       params (org-babel-variable-assignments:python params))
                                                        (ob-ipython--normalize-session session))))
                                        (let ((result (cdr (assoc :result ret)))
                                              (output (cdr (assoc :output ret))))
                                          (if (eq result-type 'output)
                                              (concat
                                               output
                                               (format "%s"
                                                       (mapconcat 'identity
                                                                  (loop for res in result
                                                                        if (eq 'image/png (car res))
                                                                        collect (ob-ipython-inline-image (cdr res)))
                                                                  "\n")))
                                            (ob-ipython--create-stdout-buffer output)
                                            (cond ((and file (string= (f-ext file) "png"))
                                                   (->> result (assoc 'image/png) cdr (ob-ipython--write-base64-string file)))
                                                  ((and file (string= (f-ext file) "svg"))
                                                   (->> result (assoc 'image/svg+xml) cdr (ob-ipython--write-string-to-file file)))
                                                  (file (error "%s is currently an unsupported file extension." (f-ext file)))
                                                  (t (->> result (assoc 'text/plain) cdr)))))))))
                        :ensure t)

                      (use-package ob-async
                        :load-path "packages/ob-async"
                        :config (progn
                                  (add-to-list 'org-ctrl-c-ctrl-c-hook 'ob-async-org-babel-execute-src-block)))

                      (add-hook 'org-mode-hook 'org-hide-block-all)

                      (org-babel-do-load-languages
                       'org-babel-load-languages
                       '((python . t)
                         (ipython . t)
                         (sql . t)
                         (C . t)
                         (shell . t)))
                      (setq org-src-fontify-natively t)
                      (setq org-confirm-babel-evaluate nil)
                      (setq org-confirm-shell-link-function nil)
                      (setq org-confirm-elisp-link-function nil)))

            (add-hook 'org-mode-hook (lambda () (modify-syntax-entry (string-to-char "") "w")))
            (setq org-startup-align-all-tables "align"))
  :ensure t)

(provide 'init-org)
;;; init-org.el ends here
