(require 'files)

(require 'init-org)
(require 'init-completion)

(require 'python-override)

;; (use-package lsp-pyright
;;   :ensure t
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-pyright)
;;                          (lsp-deferred))))

(use-package python-mode
  :init (progn
          (ob-add-language 'python (cons "python" "src python")))

  :hook ((python-mode . abbrev-mode)
         (python-mode . company-mode)
         (python-mode . flycheck-mode)
         (python-mode . my-python-flycheck-setup)
         (python-mode . smartparens-strict-mode)
         (python-mode . my-python-highlight-structured-pattern-match-hook)
         (python-mode . yas-minor-mode)

         ;; (python-mode . lsp-deferred)
         ;; (python-mode . anaconda-eldoc-mode)
         ;; (python-mode . anaconda-mode)
         ;; (python-mode . eglot-ensure)
         ;; (python-mode . pipenv-mode)

         (python-mode . python-highlight-breakpoints)
         (python-mode . company-quickhelp-mode)
         (python-mode . subword-mode)

         (python-mode . (lambda ()
                          ;; (setq-local company-backends '(company-files company-capf))
                          (poetry-venv-workon)
                          (eglot-ensure)))

         ;; (python-mode . (lambda () (add-hook 'post-command-hook #'my-python-smart-complete)))

         (inferior-python-mode . smartparens-strict-mode))

  :bind (:map python-mode-map
              ("C-c C-c" . my-python-paragraph-eval)
              ("C-c C-k" . my-python-kill-comments))

  :ensure jinja2-mode
  :ensure poetry
  :ensure eglot
  :ensure company
  :ensure pyimpsort
  :ensure py-autopep8
  :ensure flycheck
  :ensure flycheck-mypy
  :ensure flymake-ruff
  :ensure ruff-format)

(defun my-python-highlight-structured-pattern-match-hook ()
  (font-lock-add-keywords nil '(("\\<match\\>" . font-lock-keyword-face)
                                ("\\<case\\>" . font-lock-keyword-face))))

;; (add-hook 'python-mode-hook #'flycheck-pycheckers-setup)
;; (setq-default flycheck-pycheckers-checkers '(mypy3 bandit))

(flycheck-define-checker python-pycodestyle
  "A Python syntax and style checker using pycodestyle (former pep8)."

  :command ("pycodestyle" source-inplace)
  :error-patterns
  ((error line-start (file-name) ":" line ":" column ":" (message) line-end))
  :modes python-mode)

(defun my-python-flycheck-setup ()
  (setq-local flycheck-checkers '(python-flake8
                                  python-pylint
                                  python-pycompile
                                  python-pyright
                                  python-mypy
                                  python-pycodestyle)))

(defun python-highlight-breakpoints ()
  (interactive)
  (highlight-lines-matching-regexp "import ipdb")
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "set_trace()")
  (highlight-phrase "TODO")
  (highlight-regexp "FIXME"))

(define-abbrev-table 'python-mode-abbrev-table
  '(("pdb" "import pdb; pdb.set_trace()" nil 0)
    ("ipdb" "import ipdb; ipdb.set_trace()" nil 0)))

;; (add-to-list 'ob-languages '(python . t))
;; (org-babel-do-load-languages 'org-babel-load-languages ob-languages)

(defun my-get-thing-at-point (thing)
  (condition-case nil
      (substring-no-properties (thing-at-point thing))
    (error "")))

;; (defun my-python-smart-complete ()
;;   (when (and (string= major-mode "python-mode")
;;              (looking-at "$")
;;              (not (python-syntax-comment-or-string-p))
;;              (eq this-command 'self-insert-command))
;;     (let* ((line (my-get-thing-at-point 'line))
;;            (ws (my-get-thing-at-point 'whitespace))
;;            (words (s-split-words line))
;;            (init-cmd (car words))
;;            (last-cmd (car (last words))))
;;       (when (or
;;              ;; (and (string= init-cmd "from") (looking-back "\\." 1))
;;              ;; (looking-back "[a-z]\\." 2)
;;              (and (looking-back " " 1) ;; space completions
;;                   (or                  ;; complete imports
;;                    (string= last-cmd "import")
;;                    (string= last-cmd "from")
;;                    (string= last-cmd "with")

;;                    (and (string= init-cmd "from") (looking-back ", " 1)))))
;;         (my-lsp-complete)
;;         ;; (anaconda-mode-complete)
;;         ))))

;; (defun anaconda-mode-complete-callback (result)
;;   "Start interactive completion on RESULT receiving."
;;   (let* ((bounds (bounds-of-thing-at-point 'symbol))
;;          (start (or (car bounds) (point)))
;;          (stop (or (cdr bounds) (point)))
;;          (collection (anaconda-mode-complete-extract-names result))
;;          (completion-extra-properties '(:annotation-function anaconda-mode-complete-annotation)))
;;     (delete-region start stop)
;;     (insert (org-completing-read "Anaconda complete: " collection))))

;; (cl-defun my-python-paragraph-bounds ()
;;   (let* ((bounds (save-excursion
;;                    (while (and (not (bobp))
;;                                (string-prefix-p " " (thing-at-point 'line t)))
;;                      (python-nav-beginning-of-block)
;;                      (when (string-prefix-p " " (thing-at-point 'line t))
;;                        (forward-line -1)))

;;                    (beginning-of-line)

;;                    (let ((end (save-excursion (python-nav-end-of-block) (point))))
;;                      (while (save-excursion
;;                               (forward-line -1)
;;                               (string-prefix-p "@" (thing-at-point 'line t)))
;;                        (forward-line -1))
;;                      (cons (point) end))))
;;          (beg (car bounds))
;;          (end (cdr bounds)))

;;     (when (= beg end)
;;       (let ((bounds (expal:bounds)))
;;         (setq beg (car bounds)
;;               end (min (cdr bounds) (point-max)))))

;;     (cons beg end)))

;; (cl-defun my-python-kill-comments ()
;;   (interactive)
;;   (save-excursion
;;     (let ((bounds (my-python-paragraph-bounds)))
;;       (goto-char (1- (cdr bounds)))
;;       (end-of-line))

;;     (if (org-at-comment-p)
;;         (progn (while (org-at-comment-p)
;;                  (forward-line -1))
;;                (forward-line))
;;       (forward-line))

;;     (while (org-at-comment-p)
;;       (beginning-of-line)
;;       (kill-line)
;;       (unless (eobp)
;;         (kill-line)))

;;     ;; (when (string-empty-p (s-trim (thing-at-point 'line t)))
;;     ;;   (kill-line))
;;     ))

;; (defvar my-python-interpreters (make-hash-table :test 'equal))

;; (cl-defun my-python-paragraph-eval ()
;;   (interactive)
;;   (let* ((bounds (my-python-paragraph-bounds))
;;          (beg (car bounds))
;;          (end (cdr bounds)))

;;     (pulse-momentary-highlight-region beg end 'region)
;;     (redisplay)

;;     (let* ((contents
;;             (let ((actual (s-trim (buffer-substring-no-properties beg end))))
;;               (with-temp-buffer
;;                 (insert actual)
;;                 (goto-char (point-min))
;;                 (cond
;;                  ((s-contains-p " = " (thing-at-point 'line t))
;;                   (concat
;;                    "from pprint import pprint"
;;                    "\n"
;;                    actual
;;                    "\n"
;;                    "pprint("
;;                    (buffer-substring-no-properties
;;                     (point-min)
;;                     (save-excursion
;;                       (- (search-forward " = ") 3)))
;;                    ")"))
;;                  ((s-ends-with-p "?" actual)
;;                   (concat
;;                    "from pprint import pprint"
;;                    "\n"
;;                    "pprint(dir("
;;                    (substring-no-properties actual 0 (- (length actual) 1))
;;                    "))"))
;;                  ((s-ends-with-p "^^" actual)
;;                   (concat
;;                    "from pprint import pprint\n"
;;                    "pprint("
;;                    (substring-no-properties actual 0 (- (length actual) 2))
;;                    ")"))
;;                  (t actual)))))

;;            (result (save-window-excursion
;;                      (let* ((root (project-root (project-current)))
;;                             (process (pcase (gethash root my-python-interpreters)
;;                                        ((or (pred null) (pred (-compose 'not 'process-live-p)))
;;                                         (puthash root (run-python) my-python-interpreters))
;;                                        (_ (gethash root my-python-interpreters)))))
;;                        (python-shell-send-string-no-output contents process)))))

;;       (save-excursion
;;         (goto-char (1- end))
;;         (end-of-line)

;;         (if (eobp)
;;             (newline)
;;           (my-python-kill-comments)
;;           (forward-char))

;;         (pp result)

;;         ;; (let ((comment "# ")
;;         ;;       (none "None")
;;         ;;       (result-beg (point))
;;         ;;       result-end)
;;         ;;   (if (or (string-empty-p result)
;;         ;;           (string= result none))
;;         ;;       (hlt-highlight-region beg end 'expal-block-success-face)
;;         ;;     (insert result "\n")
;;         ;;     (setq result-end (min (point) (point-max)))
;;         ;;     (cl-loop
;;         ;;        initially (goto-char result-beg)
;;         ;;        while (< (point) result-end)
;;         ;;        do (progn
;;         ;;             (beginning-of-line)
;;         ;;             (insert comment)
;;         ;;             (forward-line)
;;         ;;             (setq result-end (+ result-end (length comment))))
;;         ;;        finally
;;         ;;          (unless (string-empty-p (s-trim (thing-at-point 'line t)))
;;         ;;            (newline)))))
;;         ))))

(defun my-python-send-buffer ()
  (interactive)
  (run-python)
  (python-shell-send-buffer)
  (save-window-excursion
    (switch-to-buffer-other-window (python-shell-get-buffer))))

;; (add-to-list 'hippie-expand-try-functions-list #'my-lsp-complete)

(define-key python-mode-map (kbd "C-c C-l") #'my-python-send-buffer)
(define-key python-mode-map (kbd "M-/") #'hippie-expand)

;; (defvar my-python-test-runner "pipenv run mypy .")

;; (defun my-python-run-tests ()
;;   (interactive)
;;   (async-shell-command my-python-test-runner))

;; (define-key python-mode-map (kbd "C-c C-t") #'my-python-run-tests)

(provide 'init-python)
