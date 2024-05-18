(require 'eglot-booster)
(fset #'jsonrpc--log-event #'ignore)
;; (fset #'eglot-events-buffer-size 0)

(use-package company
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous)
              ("/" . company-filter-candidates))

  :custom
  (company-idle-delay 0.2)
  (company-echo-delay 0.2)
  (company-minimum-prefix-length 1)

  :ensure t
  :ensure company-statistics
  :ensure company-quickhelp
  :ensure yasnippet)

;; (use-package lsp-mode
;;   :custom (lsp-completion-enable t)
;;   :ensure t)

(condition-case nil
    (minibuffer-keyboard-quit)
  (error nil)
  (quit 1))

(defun my-lsp-complete ()
  (interactive)
  (when (or (--some (lsp--client-completion-in-comments? (lsp--workspace-client it))
                    (lsp-workspaces))
            (not (nth 4 (syntax-ppss))))
    (let* ((trigger-chars (-> (lsp--capability-for-method "textDocument/completion")
                              (lsp:completion-options-trigger-characters?)))
           (bounds-start (or (cl-first (bounds-of-thing-at-point 'symbol))
                             (point)))
           result done?
           (candidates (lsp--catch 'input
                           (let ((lsp--throw-on-input lsp-completion-use-last-result)
                                 (same-session? (and lsp-completion--cache
                                                     ;; Special case for empty prefix and empty result
                                                     (or (cl-second lsp-completion--cache)
                                                         (not (string-empty-p
                                                               (plist-get (cddr lsp-completion--cache) :prefix))))
                                                     (equal (cl-first lsp-completion--cache) bounds-start)
                                                     (s-prefix?
                                                      (plist-get (cddr lsp-completion--cache) :prefix)
                                                      (buffer-substring-no-properties bounds-start (point))))))
                             (cond
                              ((or done? result) result)
                              ((and (not lsp-completion-no-cache) same-session? (listp (cl-second lsp-completion--cache)))
                               (setf result (apply #'lsp-completion--filter-candidates
                                                   (cdr lsp-completion--cache))))
                              (t
                               (-let* ((resp (lsp-request-while-no-input
                                              "textDocument/completion"
                                              (plist-put (lsp--text-document-position-params)
                                                         :context (lsp-completion--get-context trigger-chars same-session?))))
                                       (completed (and resp
                                                       (not (and (lsp-completion-list? resp)
                                                                 (lsp:completion-list-is-incomplete resp)))))
                                       (items (lsp--while-no-input
                                                (--> (cond
                                                      ((lsp-completion-list? resp)
                                                       (lsp:completion-list-items resp))
                                                      (t resp))
                                                     (if (or completed
                                                             (seq-some #'lsp:completion-item-sort-text? it))
                                                         (lsp-completion--sort-completions it)
                                                       it)
                                                     (-map (lambda (item)
                                                             (lsp-put item
                                                                      :_emacsStartPoint
                                                                      (or (lsp-completion--guess-prefix item)
                                                                          bounds-start)))
                                                           it))))
                                       (markers (list bounds-start (copy-marker (point) t)))
                                       (prefix (buffer-substring-no-properties bounds-start (point)))
                                       (lsp-completion--no-reordering (not lsp-completion-sort-initial-results)))
                                 (lsp-completion--clear-cache same-session?)
                                 (setf done? completed
                                       lsp-completion--cache (list bounds-start
                                                                   (cond
                                                                    ((and done? (not (seq-empty-p items)))
                                                                     (lsp-completion--to-internal items))
                                                                    ((not done?) :incomplete))
                                                                   :lsp-items nil
                                                                   :markers markers
                                                                   :prefix prefix)
                                       result (lsp-completion--filter-candidates
                                               (cond (done?
                                                      (cl-second lsp-completion--cache))
                                                     (lsp-completion-filter-on-incomplete
                                                      (lsp-completion--to-internal items)))
                                               :lsp-items items
                                               :markers markers
                                               :prefix prefix))))))
                         (:interrupted lsp-completion--last-result)
                         (`,res (setq lsp-completion--last-result res)))))
      (yas-expand-snippet (condition-case nil
                              (completing-read "Item: " candidates)
                            (quit ""))))))

(use-package vertico
  :ensure t)

(use-package consult
  :init (progn
          (unbind-key (kbd "C-x i")))
  :bind (("C-x i m" . #'consult-imenu))
  :ensure t)

(use-package orderless
  :ensure t)

(use-package marginalia
  :ensure t)

(use-package yasnippet
  :ensure t)

(require 'vertico-directory)

(vertico-mode)
;; (vertico-posframe-mode)
(recentf-mode)
(marginalia-mode)

;; Persist history over Emacs restarts. Vertico sorts by history position.
(require 'savehist)
(savehist-mode)

(consult-customize consult-completion-in-region
                   :completion-styles '(orderless)
                   :cycle-threshold 3)
(setq completion-in-region-function 'consult-completion-in-region
      orderless-smart-case t)

(define-key vertico-map (kbd "RET") #'vertico-directory-enter)
(define-key vertico-map (kbd "DEL") #'vertico-directory-delete-char)
(define-key vertico-map (kbd "M-DEL") #'vertico-directory-delete-word)
;; (rfn-eshadow-update-overlay . vertico-directory-tidy)

(define-key global-map (kbd "C-x b") #'consult-buffer)
(define-key global-map (kbd "M-g M-g") #'consult-goto-line)
(define-key global-map (kbd "C-x SPC") #'consult-mark)
(define-key global-map (kbd "C-c C-r") #'(lambda () (interactive) (consult-ripgrep default-directory (thing-at-point 'symbol))))

;; Use the `orderless' completion style. Additionally enable
;; `partial-completion' for file path expansion. `partial-completion' is
;; important for wildcard support. Multiple files can be opened at once
;; with `find-file' if you enter a wildcard. You may also give the
;; `initials' completion style a try.
(setq completion-styles '(orderless) ;; basic substring partial-completion flex
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(defmacro define-completion (name string &rest body)
  (declare (doc-string 3) (indent 2))
  `(prog1
       (defun ,name nil
         ,(when (stringp (car body)) (car body))
         (interactive)
         (let ((point (point)))
           (when (condition-case nil
                     (when (save-excursion
                             (search-backward
                              ,string (- (point) (length ,string)) t))
                       ,@body)
                   (quit nil))
             (save-excursion
               (goto-char (- point (length ,string)))
               (delete-char (+ 0 (length ,string)))))))
     (add-hook 'post-self-insert-hook ',name)))

;; Enable richer annotations using the Marginalia package

;; (use-package corfu
;;     ;; :bind
;;     ;; Configure SPC for separator insertion
;;     ;; (:map corfu-map ("SPC" . corfu-insert-separator))
;;     ;; Optional customizations
;;     :custom
;;   (corfu-cycle t)                   ;; Enable cycling for `corfu-next/previous'
;;   (corfu-auto t)                 ;; Enable auto completion
;;   (corfu-auto-delay 0.1)
;;   ;; (corfu-separator ?-)          ;; Orderless field separator
;;   (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
;;   (corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
;;   ;; (corfu-preview-current nil)    ;; Disable current candidate preview
;;   (corfu-preselect-first t)    ;; Disable candidate preselection
;;   ;; (corfu-on-exact-match 'insert)     ;; Configure handling of exact matches
;;   ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
;;   ;; (corfu-scroll-margin 5)        ;; Use scroll margin

;;   ;; You may want to enable Corfu only for certain modes.
;;   ;; :hook ((prog-mode . corfu-mode))

;;   ;; Recommended: Enable Corfu globally.
;;   ;; This is recommended since dabbrev can be used globally (M-/).
;;   :init
;;   (corfu-global-mode)
;;   )

;; Add extensions
;; (use-package cape
;;     :init
;;   ;; Add `completion-at-point-functions', used by `completion-at-point'.
;;   ;; (add-to-list 'completion-at-point-functions #'cape-file)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-tex)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
;;   (add-to-list 'completion-at-point-functions #'cape-keyword)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-anaconda)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-sgml)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-rfc1345)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-ispell)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-dict)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
;;   ;; (add-to-list 'completion-at-point-functions #'cape-line)
;;   )

;; (defun my/corfu-commit-predicate ()
;;   "Auto-commit candidates if:
;; 1. A '.' is typed, except after a SPACE.
;; 2. A selection was made, aside from entering SPACE.
;; 3. Just one candidate exists, and we continue to non-symbol info.
;; 4. The 1st match is exact."
;;   (cond
;;     ((seq-contains-p (this-command-keys-vector) ?.)
;;      (or (string-empty-p (car corfu--input))
;; 	 (not (string= (substring (car corfu--input) -1) " "))))

;;     ((/= corfu--index corfu--preselect) ; a selection was made
;;      (not (seq-contains-p (this-command-keys-vector) ? )))

;;     ((eq corfu--total 1) ;just one candidate
;;      (seq-intersection (this-command-keys-vector) [?: ?, ?\) ?\] ?\( ? ]))

;;     ((and corfu--input ; exact 1st match
;; 	  (string-equal (substring (car corfu--input) corfu--base)
;; 			(car corfu--candidates)))
;;      (seq-intersection (this-command-keys-vector) [?: ?. ?, ?\) ?\] ?\" ?' ? ]))))

;; (setq corfu-commit-predicate #'my/corfu-commit-predicate)

(provide 'init-completion)
