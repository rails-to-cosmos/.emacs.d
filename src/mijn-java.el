;;; mijn-java.el --- Java development setup -*- lexical-binding: t; -*-

(defun mijn-java--jdt-uri-handler (_operation &rest args)
  "Robust replacement for `eglot-java--jdt-uri-handler'.
Resolve a `jdt://' URI (library/compiled class) to a cached .java file
under the project's .eglot-java/ directory.  The upstream handler's regex
requires a literal `?' query suffix after `.class'; URIs without it (e.g.
many JDK module classes) fail to match, and the target collapses to the
.eglot-java/ directory itself — surfacing as \"Read error: Is a directory\".
Here the suffix is optional and a hash fallback guarantees a real filename."
  (let* ((uri (car args))
         (cache-dir (expand-file-name ".eglot-java" (project-root (project-current t))))
         (class-name
          (if (string-match "jdt://contents/[^/]+/\\(.*?\\)\\.class\\(\\?.*\\)?\\'" uri)
              (replace-regexp-in-string "/" "." (match-string 1 uri) t t)
            (format "eglot-java-%x" (sxhash-equal uri))))
         (source-file (expand-file-name (concat class-name ".java") cache-dir)))
    (unless (file-readable-p source-file)
      (let ((content (jsonrpc-request (eglot-java--find-server)
                                      :java/classFileContents (list :uri uri)))
            (metadata-file (expand-file-name (concat class-name ".metadata") cache-dir)))
        (unless (file-directory-p cache-dir) (make-directory cache-dir t))
        (with-temp-file source-file (insert (or content "")))
        (with-temp-file metadata-file (insert uri))))
    source-file))

(defun mijn-java--init ()
  "Per-buffer Java setup: indentation, completion, snippets, LSP, format-on-save."
  (setq-local c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)
  ;; Tell jdtls to fetch dependency sources/javadoc so jump-to-definition lands
  ;; on real source (not decompiled stubs). Buffer-local — must NOT clobber the
  ;; global `eglot-workspace-configuration' that `mijn-rust' sets.
  (setq-local eglot-workspace-configuration
              '(:java (:maven (:downloadSources t)
                       :eclipse (:downloadSources t)
                       :references (:includeDecompiledSources t))))
  (subword-mode)
  (smartparens-strict-mode)
  (yas-minor-mode)
  (company-mode)
  (company-quickhelp-mode 1)
  ;; `eglot-java-mode' registers the Eclipse JDT server, downloads it on first
  ;; use, and starts eglot — enabling it is enough, no separate `eglot-ensure'.
  (eglot-java-mode)
  ;; Let jdtls format (and organize imports via format) on save, buffer-locally.
  (add-hook 'before-save-hook #'eglot-format-buffer nil t))

(use-package eglot-java
  :hook (java-mode . mijn-java--init)
  :config
  ;; Fix jump-to-definition into libraries/JDK (see `mijn-java--jdt-uri-handler').
  (advice-add 'eglot-java--jdt-uri-handler :override #'mijn-java--jdt-uri-handler)
  (define-key eglot-java-mode-map (kbd "C-c l n") #'eglot-java-file-new)
  (define-key eglot-java-mode-map (kbd "C-c l x") #'eglot-java-run-main)
  (define-key eglot-java-mode-map (kbd "C-c l t") #'eglot-java-run-test)
  (define-key eglot-java-mode-map (kbd "C-c l N") #'eglot-java-project-new)
  (define-key eglot-java-mode-map (kbd "C-c l T") #'eglot-java-project-build-task)
  (define-key eglot-java-mode-map (kbd "C-c l R") #'eglot-java-project-build-refresh)
  :ensure t
  :ensure eglot
  :ensure company
  :ensure smartparens
  :ensure yasnippet)

(provide 'mijn-java)
;;; mijn-java.el ends here
