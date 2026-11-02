;; -*- lexical-binding: t; -*-
(require 'direnv)

(up dired-narrow
  :ensure t)

(up dired-rainbow
  :config
  (dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
  (dired-rainbow-define media "#ce5c00" ("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg"))
  (dired-rainbow-define log (:inherit default :italic t) ".*\\.log")
  (dired-rainbow-define-chmod executable-unix "#B3DE81" "-[rw-]+x.*")
  :ensure t)

(require 'dired-plus)
(require 'dired)
(require 'dired-x)
(require 'seq)

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
(add-hook 'dired-mode-hook 'direnv)

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

(defun dired-smart-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(add-hook 'dired-after-readin-hook #'dired-smart-sort)

(defun dired-set-keys ()
  (define-key dired-mode-map "/" #'dired-narrow-fuzzy)
  (define-key dired-mode-map "~" #'(lambda () (interactive) (dired "~")))
  (define-key dired-mode-map (kbd "C-w") #'diredp-copy-abs-filenames-as-kill)
  (define-key dired-mode-map (kbd "!") #'dired:xdg-open-marked-files)
  (define-key dired-mode-map (kbd "<DEL>") #'dired-up-please)
  (define-key dired-mode-map (kbd "e") #'eshell)
  (define-key dired-mode-map (kbd "<RET>") #'dired-down-please)
  (define-key dired-mode-map (kbd "C-c C-p") #'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "j") #'next-line)
  (define-key dired-mode-map (kbd "k") #'previous-line)
  (define-key dired-mode-map (kbd "h") #'dired-up-please)
  (define-key dired-mode-map (kbd "l") #'dired-down-please)
  )

(add-hook 'dired-mode-hook 'dired-set-keys)
(define-key global-map (kbd "C-x C-d") #'dired-switch-buffers)

(require 'dash)

(defun my--normalize-name (name)
  "Transliterate NAME and return it lowercase and dash-separated."
  (->> name
       my-transliterate-ru-to-en
       downcase
       (replace-regexp-in-string "[^а-яa-z0-9]+" "-")
       (replace-regexp-in-string "-+" "-")
       (replace-regexp-in-string "^-\\|-$" "")
       (replace-regexp-in-string "^_\\|_$" "")))

(defun my-normalized-path (path)
  "Return PATH with a lowercase, dash-separated final component.
Preserve a leading dot and the final extension separator."
  (let* ((name (file-name-nondirectory (directory-file-name path)))
         (hidden (and (string-prefix-p "." name) (> (length name) 1)))
         (body (if hidden (substring name 1) name))
         (raw-extension (unless (file-directory-p path)
                          (file-name-extension body)))
         (extension (and raw-extension
                         (not (string-empty-p raw-extension))
                         raw-extension))
         (base (if extension (file-name-sans-extension body) body))
         (clean-base (my--normalize-name base))
         (clean-extension
          (and extension (my--normalize-name extension))))
    (when (string-empty-p clean-base)
      (user-error "Cannot normalize empty file name: %s" path))
    (expand-file-name
     (concat (and hidden ".") clean-base
             (and clean-extension (concat "." clean-extension)))
     (file-name-directory path))))

(defun my--directory-normalization-plan (directory recursive)
  "Return a bottom-up rename plan for DIRECTORY's entries.
Include descendants when RECURSIVE is non-nil, without following symlinks."
  (let (plan)
    (dolist (path (directory-files directory t directory-files-no-dot-files-regexp t))
      (when (and recursive (file-directory-p path) (not (file-symlink-p path)))
        (setq plan
              (nconc plan (my--directory-normalization-plan path recursive))))
      (setq plan (nconc plan (list (cons path (my-normalized-path path))))))
    plan))

(defun my--numbered-normalized-target (source target number)
  "Add NUMBER before TARGET's extension, using SOURCE to detect directories."
  (let* ((name (file-name-nondirectory target))
         (extension (unless (file-directory-p source)
                      (file-name-extension name t)))
         (base (if extension (file-name-sans-extension name) name)))
    (expand-file-name (format "%s-%d%s" base number (or extension ""))
                      (file-name-directory target))))

(defun my--disambiguate-directory-normalization-plan (plan)
  "Return PLAN with deterministic numeric suffixes for duplicate targets.
Already-normalized entries retain their names."
  (let ((targets (make-hash-table :test #'equal))
        result)
    (dolist (rename plan)
      (when (equal (car rename) (cdr rename))
        (puthash (cdr rename) t targets)))
    (dolist (rename plan (nreverse result))
      (let ((source (car rename))
            (target (cdr rename)))
        (unless (equal source target)
          (let ((number 2))
            (while (gethash target targets)
              (setq target
                    (my--numbered-normalized-target source (cdr rename) number))
              (setq number (1+ number)))))
        (puthash target t targets)
        (push (cons source target) result)))))

(defun my--rename-path-case-safely (source target)
  "Rename SOURCE to TARGET, including case-only renames."
  (if (and (file-exists-p target) (file-equal-p source target))
      (let ((temporary
             (make-temp-name
              (expand-file-name ".normalize-name-" (file-name-directory source)))))
        (rename-file source temporary)
        (condition-case error-data
            (rename-file temporary target)
          (error
           (rename-file temporary source)
           (signal (car error-data) (cdr error-data)))))
    (rename-file source target)))

(defun my-normalize-directory-names (&optional recursive)
  "Normalize every file and directory name under `default-directory'.
Names become lowercase, Latin, and dash-separated.  With prefix argument
RECURSIVE, also normalize descendants.  Hidden entries keep their leading dot."
  (interactive "P")
  (let* ((plan
          (my--disambiguate-directory-normalization-plan
           (my--directory-normalization-plan default-directory recursive)))
         (renames (seq-filter (lambda (rename)
                                (not (equal (car rename) (cdr rename))))
                              plan)))
    (dolist (rename renames)
      (my--rename-path-case-safely (car rename) (cdr rename)))
    (when (derived-mode-p 'dired-mode)
      (revert-buffer))
    (message "Normalized %d name%s%s"
             (length renames)
             (if (= (length renames) 1) "" "s")
             (if recursive " recursively" ""))))

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
  (my-normalize-directory-names t))

(provide 'mijn-dired)
