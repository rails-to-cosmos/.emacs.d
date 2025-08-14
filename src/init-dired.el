(require 'init-direnv)

(use-package dired-narrow
  :ensure t)

(use-package dired-rainbow
  :ensure t)

(require 'dired-plus)
(require 'dired)
(require 'dired-x)

;; http://pragmaticemacs.com/emacs/case-insensitive-sorting-in-dired-on-os-x/
;; using ls-lisp with these settings gives case-insensitve
;; sorting on OS X
(require 'ls-lisp)

(define-key global-map (kbd "C-x d") #'dired-default-directory)

(eval-after-load "dired-aux"
  '(add-to-list 'dired-compress-file-suffixes
    '("\\.zip\\'" ".zip" "unzip")))

(toggle-diredp-find-file-reuse-dir t)

(defun dired:xdg-open-marked-files ()
  "View files, either as HTML or media.
From https://www.reddit.com/r/emacs/comments/cgbpvl/opening_media_files_straight_from_gnu_emacs_dired/"
  (interactive)
  (let ((file-list (dired-get-marked-files)))
    (mapc
     (lambda (file-path)
       (let ((process-connection-type nil))
         (cond ((eq system-type 'darwin) (start-process "" nil "open" file-path))
               (t (start-process "" nil "xdg-open" file-path)))))
     file-list)))

(defun dired-up-please ()
  (interactive)
  (let ((b (current-buffer)))
    (diredp-up-directory)
    (bury-buffer b))
  (when (eq major-mode 'dired-mode)
    (rename-buffer (concat "/dired:" dired-directory))))

(defun dired-down-please ()
  (interactive)
  (let ((b (current-buffer)))
    (dired-find-file)
    (bury-buffer b))
  (when (eq major-mode 'dired-mode)
    (rename-buffer (concat "/dired:" dired-directory))))

(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'dired-after-readin-hook
          (lambda ()
            ;; Set name of dired buffers to absolute directory name.
            ;; Use `generate-new-buffer-name' for vc-directory
            ;; which creates duplicate buffers.
            (dired-hide-details-mode -1))
          'append)

(setq-default dired-omit-files-p t
              dired-omit-verbose t
              dired-omit-files
              (rx (or (seq bol (? ".") "#")
                      (seq bol "." (0+ anything) eol)
                      (seq bol "." eol)
                      (seq bol "__pycache__" eol)
                      (seq bol ".." eol))))

(add-hook 'dired-mode-hook 'dired-omit-mode)
(add-hook 'dired-mode-hook 'my-direnv)

(defun dired-switch-buffers ()
  "Quickly switch between dired buffers."
  (interactive)
  (let ((dired-buffer-list
         (--map (buffer-name it)
                (--filter
                 (with-current-buffer it (derived-mode-p 'dired-mode))
                 (buffer-list)))))
    (cond ((= (length dired-buffer-list) 0) (dired default-directory))
          ((= (length dired-buffer-list) 1) (switch-to-buffer (car dired-buffer-list)))
          (t (switch-to-buffer
              (org-completing-read "Dired buffer: " dired-buffer-list))))))

(defun dired-default-directory ()
  (interactive)
  (dired default-directory)
  (rename-buffer (generate-new-buffer-name (concat "/dired:" dired-directory)))
  ;; (dired-collapse-mode)
  )

(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
(dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
(dired-rainbow-define-chmod executable-unix "#B3DE81" "-[rw-]+x.*")

(defun dired-smart-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(add-hook 'dired-after-readin-hook #'dired-smart-sort)

(defun my-dired-apply-recursively (fn)
  "Apply FN to the file or directory at point in Dired.
If it's a directory, apply recursively to all files and subdirectories."
  (interactive)
  (let ((path (dired-get-filename)))
    (my--apply-recursively fn path)
    (revert-buffer)))

(defun my--apply-recursively (fn path)
  "Helper to apply FN to PATH and its contents recursively if directory."
  (let ((new-path (funcall fn path))) ; apply function, and get potentially renamed path
    (when (file-directory-p new-path)
      (dolist (entry (directory-files new-path t "^[^.]" t)) ; skip . and ..
        (my--apply-recursively fn entry)))))

(defun dired-set-keys ()
  (define-key dired-mode-map "/" #'dired-narrow-fuzzy)
  (define-key dired-mode-map "~" #'(lambda () (interactive) (dired "~")))
  (define-key dired-mode-map (kbd "C-w") #'diredp-copy-abs-filenames-as-kill)
  (define-key dired-mode-map (kbd "!") #'dired:xdg-open-marked-files)
  (define-key dired-mode-map (kbd "<DEL>") #'dired-up-please)
  (define-key dired-mode-map (kbd "e") #'eshell)
  (define-key dired-mode-map (kbd "<RET>") #'dired-down-please)
  (define-key dired-mode-map (kbd "C-c C-p") #'wdired-change-to-wdired-mode))

(add-hook 'dired-mode-hook 'dired-set-keys)
(define-key global-map (kbd "C-x C-d") #'dired-switch-buffers)

(require 'dash)

(defun my-clean-file-name (name)
  "Clean up NAME: lowercase, remove [hash], replace non-alphanumerics with dashes."
  (->> name
       (replace-regexp-in-string " \\[[^]]+\\]$" "") ; remove trailing [hash]
       downcase
       (replace-regexp-in-string "[^а-яa-z0-9]+" "-")
       (replace-regexp-in-string "-+" "-")
       (replace-regexp-in-string "^-\\|-$" "")
       (replace-regexp-in-string "^_\\|_$" "")))

(defun my-clean-file-name-preserve-extension (path)
  "Rename PATH based on cleaned-up name, preserving extension if file."
  (let* ((parent (file-name-directory path))
         (ext (if (file-directory-p path) "" (file-name-extension path t)))
         (base (file-name-base path))
         (new-name (concat (my-clean-file-name base) ext))
         (new-path (expand-file-name new-name parent)))
    (unless (string= path new-path)
      (rename-file path new-path))
    new-path))

(defun my-dired-normalize-permissions ()
  (interactive)
  (shell-command "find . -type f -exec chmod 644 {} + && find . -type d -exec chmod 755 {} +"))

(defun my-dired-cleanup ()
  (shell-command "find . -type f -name '._*' -delete"))

(defun my-transliterate-ru-to-en (text)
  "Transliterate Russian TEXT to Latin (English) characters."
  (let* ((map '(("а" . "a")  ("б" . "b")  ("в" . "v")  ("г" . "g")  ("д" . "d")
                ("е" . "e")  ("ё" . "e")  ("ж" . "zh") ("з" . "z")  ("и" . "i")
                ("й" . "i")  ("к" . "k")  ("л" . "l")  ("м" . "m")  ("н" . "n")
                ("о" . "o")  ("п" . "p")  ("р" . "r")  ("с" . "s")  ("т" . "t")
                ("у" . "u")  ("ф" . "f")  ("х" . "kh") ("ц" . "ts") ("ч" . "ch")
                ("ш" . "sh") ("щ" . "shch") ("ъ" . "")  ("ы" . "y")  ("ь" . "")
                ("э" . "e")  ("ю" . "yu") ("я" . "ya")
                ;; Uppercase
                ("А" . "A")  ("Б" . "B")  ("В" . "V")  ("Г" . "G")  ("Д" . "D")
                ("Е" . "E")  ("Ё" . "E")  ("Ж" . "Zh") ("З" . "Z")  ("И" . "I")
                ("Й" . "I")  ("К" . "K")  ("Л" . "L")  ("М" . "M")  ("Н" . "N")
                ("О" . "O")  ("П" . "P")  ("Р" . "R")  ("С" . "S")  ("Т" . "T")
                ("У" . "U")  ("Ф" . "F")  ("Х" . "Kh") ("Ц" . "Ts") ("Ч" . "Ch")
                ("Ш" . "Sh") ("Щ" . "Shch") ("Ъ" . "")  ("Ы" . "Y")  ("Ь" . "")
                ("Э" . "E")  ("Ю" . "Yu") ("Я" . "Ya")))
         (result text))
    (dolist (pair map result)
      (setq result (replace-regexp-in-string (car pair) (cdr pair) result t t)))))

(defun my-dired-standardize ()
  (interactive)
  (my-dired-normalize-permissions)
  (my-dired-cleanup)
  (my-dired-apply-recursively #'my-clean-file-name-preserve-extension))

(provide 'init-dired)
