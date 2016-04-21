;;; init.el --- emacs configuration by Dmitry Akatov
;;
;; Filename: init.el
;; Description: my emacs configuration
;; Author: Dmitry Akatov
;; Created: Sun Aug 09 21:49:00 2015 (-0400)
;; Version: 1.0.0
;; URL: https://github.com/rails-to-cosmos/.emacs.d
;; Keywords: Emacs 24.5
;; Compatibility: emacs >= 24.5
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Init use-package
(require 'package)

(setq dist-packages-dir (concat user-emacs-directory "dist-packages/")
      package-user-dir (concat dist-packages-dir "elpa/"))

(make-directory dist-packages-dir t)
(make-directory package-user-dir t)

(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/"))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))

(eval-when-compile
  (require 'use-package))

(use-package use-package
  :init (progn
          (use-package diminish
            :commands diminish
            :ensure t)
          (use-package bind-key
            :ensure t)
          (setq use-package-verbose t)))

(use-package my/global-settings
  :init (progn
          (defconst *is-a-mac* (eq system-type 'darwin))
          (setq emacs-persistence-directory (concat user-emacs-directory "persistence/")
                savehist-file (concat emacs-persistence-directory ".minibuffer-history")
                session-save-file (concat emacs-persistence-directory ".session")
                frame-restore-parameters-file (concat emacs-persistence-directory ".frame-restore-parameters")
                desktop-path (list emacs-persistence-directory)
                recentf-save-file (concat emacs-persistence-directory ".recentf")
                custom-file (concat emacs-persistence-directory ".custom")
                mc/list-file (concat emacs-persistence-directory ".mc-lists")
                abbjrev-file-name (concat emacs-persistence-directory ".abbrev-defs")
                mac-command-modifier 'meta)
          (make-directory emacs-persistence-directory t)
          (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
          (add-to-list 'load-path (expand-file-name "dist-packages" user-emacs-directory))

          (let ((default-directory "/usr/local/share/emacs/site-lisp/"))
            (normal-top-level-add-subdirs-to-load-path))))

(use-package my/shell
  :init (progn
          (use-package exec-path-from-shell
            :config (progn
                      (when (memq window-system '(mac ns))
                        (exec-path-from-shell-initialize)
                        (setenv "LANG" "en_US.UTF-8")
                        (setenv "LC_ALL" "en_US.UTF-8")
                        (setenv "LC_CTYPE" "en_US.UTF-8")))
            :ensure t)

          (defun spawn-shell (name &rest commands)
            "Invoke shell with commands"
            (interactive "MName of shell buffer to spawn: ")
            (defvar spawn-shell/old-buffer (current-buffer))
            (with-current-buffer (get-buffer-create name)
              (setq default-eshell-buffer-name
                    (if (string= (boundp 'eshell-buffer-name) nil)
                        "*eshell*"
                      eshell-buffer-name)
                    eshell-buffer-name name)
              (eshell)
              (setq eshell-buffer-name default-eshell-buffer-name)
              (loop for command in commands
                    do (insert (concat command "\n")))
              (eshell-send-input)
              (goto-char (point-max)))
            (switch-to-buffer spawn-shell/old-buffer))

          (use-package term+
            :config (progn
                      (add-hook 'term-mode-hook (lambda () (yas-minor-mode -1))))
            :ensure t)

          (defun eshell-init-aliases()
            (add-to-list 'eshell-command-aliases-list '("ff" "find-file"))
            (add-to-list 'eshell-command-aliases-list '("d" "dired $1"))
            (add-to-list 'eshell-command-aliases-list '("l" "ls"))
            (add-to-list 'eshell-command-aliases-list '("ll" "ls -la"))
            (add-to-list 'eshell-command-aliases-list '("pip-update" "pip freeze --local | grep -v '^\\-e' | cut -d = -f 1  | xargs -n1 pip install -U")))

          (add-hook 'eshell-mode-hook 'eshell-init-aliases)
          (setq eshell-buffer-maximum-lines 500
                password-cache t
                password-cache-expiry 3600
                eshell-output-filter-functions '(eshell-truncate-buffer
                                                 eshell-postoutput-scroll-to-bottom
                                                 eshell-handle-control-codes
                                                 eshell-handle-ansi-color
                                                 eshell-watch-for-password-prompt))))

(use-package my/user-interface
  :init (progn
          (setq-default buffers-menu-max-size 30
                        case-fold-search t
                        compilation-scroll-output t
                        ediff-split-window-function 'split-window-horizontally
                        ediff-window-setup-function 'ediff-setup-windows-plain
                        grep-highlight-matches t
                        grep-scroll-output t
                        make-backup-files nil
                        mouse-yank-at-point t
                        save-interprogram-paste-before-kill t
                        scroll-preserve-screen-position 'always
                        set-mark-command-repeat-pop t
                        show-trailing-whitespace nil
                        tooltip-delay 1.5
                        truncate-lines nil
                        truncate-partial-width-windows nil
                        visible-bell t
                        line-spacing 7
                        indent-tabs-mode nil
                        x-use-underline-position-properties t
                        underline-minimum-offset 3
                        cursor-type 'box)

          (custom-set-faces
           '(default ((t (:inherit nil
                                   :stipple nil
                                   :inverse-video nil
                                   :box nil
                                   :strike-through nil
                                   :overline nil
                                   :underline nil
                                   :slant normal
                                   :weight normal
                                   :height 120
                                   :width normal
                                   :foundry nil
                                   :family "Menlo")))))

          (set-cursor-color "#D0E1F9")

	  (use-package my/themes
	    :init (progn
		    (defvar custom-themes-dir (concat dist-packages-dir "themes/"))
		    (add-to-list 'custom-theme-load-path custom-themes-dir)
		    (make-directory custom-themes-dir t)
                    (use-package danneskjold-theme
                      :ensure t)))

	  (use-package frame-cmds
	    :commands toggle-max-frame
	    :ensure t)

          (use-package init-gui-frames
            :if window-system
            :init (progn
                    (defun set-frame-alpha (arg &optional active)
                      (interactive "nEnter alpha value (1-100): \np")
                      (let* ((elt (assoc 'alpha default-frame-alist))
                             (old (frame-parameter nil 'alpha))
                             (new (cond ((atom old)     `(,arg ,arg))
                                        ((eql 1 active) `(,arg ,(cadr old)))
                                        (t              `(,(car old) ,arg)))))
                        (if elt (setcdr elt new) (push `(alpha ,@new) default-frame-alist))
                        (set-frame-parameter nil 'alpha new)))
                    (global-set-key (kbd "C-c t") 'set-frame-alpha)))

          ;; (use-package mode-icons
          ;;   :config (progn
          ;;             (mode-icons-mode t))
          ;;   :ensure t)

          (use-package smart-mode-line
            :config (progn
                      (setq-default
                       mode-line-format
                       '("%e"
                         mode-line-front-space
                         mode-line-mule-info
                         mode-line-client
                         mode-line-modified
                         mode-line-remote
                         mode-line-frame-identification
                         mode-line-buffer-identification
                         "   "
                         mode-line-position
                         (vc-mode vc-mode)
                         "  "
                         mode-line-modes
                         mode-line-misc-info
                         mode-line-end-spaces)))
            :ensure t)

          (use-package split-window
            :init (progn
                    (defun split-window-multiple-ways (x y)
                      "Split the current frame into a grid of X columns and Y rows."
                      (interactive "nColumns: \nnRows: ")
                      ;; one window
                      (delete-other-windows)
                      (dotimes (i (1- x))
                        (split-window-horizontally)
                        (dotimes (j (1- y))
                          (split-window-vertically))
                        (other-window y))
                      (dotimes (j (1- y))
                        (split-window-vertically))
                      (balance-windows))
                    (autoload 'windmove-find-other-window "windmove"
                      "Return the window object in direction DIR. fn dir &optional arg window)")
                    (declare-function windmove-find-other-window "windmove" (dir &optional arg window))
                    (defun get-window-in-frame (x y &optional frame)
                      "Find Xth horizontal and Yth vertical window from top-left of FRAME."
                      (let ((orig-x x) (orig-y y)
                            (w (frame-first-window frame)))
                        (while (and (windowp w) (> x 0))
                          (setq w (windmove-find-other-window 'right 1 w)
                                x (1- x)))
                        (while (and (windowp w) (> y 0))
                          (setq w (windmove-find-other-window 'down 1 w)
                                y (1- y)))
                        (unless (windowp w)
                          (error "No window at (%d, %d)" orig-x orig-y))
                        w))
                    (defun set-window-buffer-in-frame (x y buffer &optional frame)
                      "Set Xth horizontal and Yth vertical window to BUFFER from top-left of FRAME."
                      (set-window-buffer (get-window-in-frame x y frame) buffer))))

          (defun what-face (pos)
            (interactive "d")
            (let ((face (or (get-char-property (point) 'read-face-name)
                            (get-char-property (point) 'face))))
              (if face (message "Face: %s" face) (message "No face at %d" pos))))))

(use-package my/elisp-utils
  :init (progn
          (bind-key "<f1>" 'help-command)
          (bind-key "C-h" 'delete-backward-char)
          (bind-key "C-M-h" 'backward-kill-word)

          (defmacro after-load (feature &rest body)
            "After FEATURE is loaded, evaluate BODY."
            (declare (indent defun))
            `(eval-after-load ,feature
               '(progn ,@body)))

          (defun add-auto-mode (mode &rest patterns)
            "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
            (dolist (pattern patterns)
              (add-to-list 'auto-mode-alist (cons pattern mode))))

          (defun delete-this-file ()
            "Delete the current file, and kill the buffer."
            (interactive)
            (or (buffer-file-name) (error "No file is currently being edited"))
            (when (yes-or-no-p (format "Really delete '%s'?"
                                       (file-name-nondirectory buffer-file-name)))
              (delete-file (buffer-file-name))
              (kill-this-buffer)))

          (defun rename-this-file-and-buffer (new-name)
            "Renames both current buffer and file it's visiting to NEW-NAME."
            (interactive "sNew name: ")
            (let ((name (buffer-name))
                  (filename (buffer-file-name)))
              (unless filename
                (error "Buffer '%s' is not visiting a file!" name))
              (if (get-buffer new-name)
                  (message "A buffer named '%s' already exists!" new-name)
                (progn
                  (when (file-exists-p filename)
                    (rename-file filename new-name 1))
                  (rename-buffer new-name)
                  (set-visited-file-name new-name)))))

          (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
            "Prevent annoying \"Active processes exist\" query when you quit Emacs."
            (cl-flet ((process-list ())) ad-do-it))

          (defun project-buffer-name-by-feature (project-name feature-name)
            (concatenate 'string "*" project-name "-" feature-name "*"))

          (setq kill-buffer-query-functions
                (remq 'process-kill-buffer-query-function
                      kill-buffer-query-functions))))

(use-package my/macros-utils
  :commands (save-macro)
  :init (progn
          (defun save-macro (name)
            "save a macro. Take a name as argument
        and save the last defined macro under
        this name at the end of your .emacs"
            (interactive "SName of the macro:") ; ask for the name of the macro
            (kmacro-name-last-macro name)        ; use this name for the macro
            (find-file user-init-file)   ; open ~/.emacs or other user init file
            (goto-char (point-max))      ; go to the end of the .emacs
            (newline)                    ; insert a newline
            (insert-kbd-macro name)      ; copy the macro
            (newline)                    ; insert a newline
            (switch-to-buffer nil))))

;; (use-package my/games
;;   :init (progn
;;           (use-package steam
;;             :config (progn
;;                       (setq steam-username "bezdnaskov")))))

(use-package my/keybinding-presuppositions
  :init (progn
          (dolist (key '("\C-l"))
            (global-unset-key key))))

(use-package my/text-editing-utils
  :init (progn
          (use-package autocomplete
            ;; (require 'ej-autocomplete)
            :config (progn
                      ;; (use-package bash-completion
                      ;;   :commands bash-completion-dynamic-complete
                      ;;   :init (progn
                      ;;           (add-hook 'shell-dynamic-complete-functions
                      ;;                     'bash-completion-dynamic-complete))
                      ;;   :config (progn
                      ;;             (bash-completion-setup))
                      ;;   :ensure t)
                      (use-package auto-complete-nxml
                        :ensure t)
                      ;; (use-package popup
                      ;;   :ensure t)
                      ;; (use-package pos-tip)
                      ;; (use-package popup-kill-ring)
                      ;; (use-package auto-complete-config)
                      (ac-config-default)
                      (setq ac-comphist-file (concat emacs-persistence-directory ".ac-comphist")
                            ac-use-menu-map t
                            hippie-expand-verbose t
                            smart-tab-using-hippie-expand t
                            hippie-expand-try-functions-list
                            '(yas/hippie-try-expand
                              try-complete-file-name-partially
                              try-expand-all-abbrevs
                              try-expand-dabbrev
                              try-expand-dabbrev-all-buffers
                              try-expand-dabbrev-from-kill
                              try-complete-lisp-symbol-partially
                              try-complete-lisp-symbol)
                            ac-comphist-file (concat emacs-persistence-directory "ac-comphist.dat"))))

          (use-package key-combo
            :config (progn
                      (global-key-combo-mode t)
                      ;; (key-combo-load-default)
                      )
            :ensure t)

          ;; Align command
          ;; from http://stackoverflow.com/questions/3633120/emacs-hotkey-to-align-equal-signs
          ;; another information: https://gist.github.com/700416
          ;; use rx function http://www.emacswiki.org/emacs/rx
          (defun align-to-colon (begin end)
            "Align region to colon (:) signs"
            (interactive "r")
            (align-regexp begin end
                          (rx (group (zero-or-more (syntax whitespace))) ":") 1 1 ))

          (defun align-to-comma (begin end)
            "Align region to comma  signs"
            (interactive "r")
            (align-regexp begin end
                          (rx "," (group (zero-or-more (syntax whitespace))) ) 1 1 ))

          (defun align-to-equals (begin end)
            "Align region to equal signs"
            (interactive "r")
            (align-regexp begin end
                          (rx (group (zero-or-more (syntax whitespace))) "=") 1 1 ))

          (defun align-to-hash (begin end)
            "Align region to hash ( => ) signs"
            (interactive "r")
            (align-regexp begin end
                          (rx (group (zero-or-more (syntax whitespace))) "=>") 1 1 ))

          ;; work with this
          (defun align-to-comma-before (begin end)
            "Align region to equal signs"
            (interactive "r")
            (align-regexp begin end
                          (rx (group (zero-or-more (syntax whitespace))) ",") 1 1 ))

          (defun align-to-whitespace (start end)
            "Align columns by whitespace"
            (interactive "r")
            (align-regexp start end
                          "\\(\\s-*\\)\\s-" 1 0 t))

          (defun remove-dos-eol ()
            "Do not show ^M in files containing mixed UNIX and DOS line endings."
            (interactive)
            (setq buffer-display-table (make-display-table))
            (aset buffer-display-table ?\^M []))

          (defun bjm/align-whitespace (start end)
            "Align columns by whitespace"
            (interactive "r")
            (align-regexp start end
                          "\\(\\s-*\\)\\s-" 1 0 t))

          (defun align-to-ampersand (start end)
            "Align columns by ampersand"
            (interactive "r")
            (align-regexp start end
                          "\\(\\s-*\\)&" 1 1 t))

          (use-package syntax-subword
            :config (progn
                      (global-syntax-subword-mode t))
            :ensure t)

          (use-package wgrep
            :ensure t)

          (setq read-file-name-completion-ignore-case t
                read-buffer-completion-ignore-case t)
          (mapc (lambda (x)
                  (add-to-list 'completion-ignored-extensions x))
                '(".$$$" ".000" ".a" ".a26" ".a78" ".acn" ".acr" ".agdai" ".aif" ".alg" ".ali" ".aliases" ".annot" ".ap_" ".api" ".api-txt" ".apk" ".app" ".aps" ".autosave" ".aux" ".auxlock" ".avi" ".azurePubxml" ".bak" ".bbl" ".bcf" ".bck" ".beam" ".beams" ".bim.layout" ".bin" ".blg" ".booproj" ".bowerrc" ".box" ".bpi" ".bpl" ".brf" ".bs" ".build.csdef" ".byte" ".cachefile" ".c_date" ".cfg" ".cfgc" ".cgo1.go" ".cgo2.c" ".chi" ".chs.h" ".class" ".cma" ".cmi" ".cmo" ".cmp" ".cmx" ".cmxa" ".cmxs" ".crc" ".crs" ".csproj" ".css.map" ".cubin" ".d" ".dart.js" ".db" ".dbmdl" ".dbproj.schemaview" ".dcp" ".dcu" ".debug" ".debug.app" ".def" ".DEPLOYED" ".dex" ".dll" ".dmb" ".dotCover" ".DotSettings.user" ".dox" ".dpth" ".drc" ".drd" ".dres" ".dri" ".drl" ".dsk" ".dump" ".dvi" ".dylib" ".dyn_hi" ".dyn_o" ".ear" ".pyc" ".xls" ".DS_Store"))

          (use-package mmm-mode
            :config (progn
                      (setq mmm-global-mode 'maybe)
                      (mmm-add-classes
                       '((python-rst
                          :submode rst-mode
                          :front "^ *[ru]?\"\"\"[^\"]*$"
                          :back "^ *\"\"\""
                          :include-front t
                          :include-back t
                          :end-not-begin t)))
                      (mmm-add-mode-ext-class 'python-mode nil 'python-rst))
            :ensure t)))

(use-package my/log-utils
  :init (progn
          (use-package interaction-log
            :config (progn
                      (interaction-log-mode t))
            :ensure t)
          (use-package mwe-log-commands
            :ensure t)))

(use-package scratch
  :ensure t)

;; (use-package yasnippet
;;   :config (progn
;;             (yas-global-mode 1)
;;             (bind-key "C-j" 'yas-expand yas-minor-mode-map))
;;   :ensure t)

(use-package impatient-mode
  :commands impatient-mode
  :ensure t)

(use-package restclient
  :commands restclient-mode
  :mode ("\\.rest\\'" . restclient-mode)
  :ensure t)

(use-package my/databases
  :init (progn
          (use-package redis
            :ensure t)))

(use-package emacsql
  :init (progn
          (use-package pg
            :ensure t)
          (use-package sql-indent
            :ensure t))
  :config (progn
            (defun sql-indent-string ()
              "Indents the string under the cursor as SQL."
              (interactive)
              (save-excursion
                (er/mark-inside-quotes)
                (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
                       (pos (region-beginning))
                       (column (progn (goto-char pos) (current-column)))
                       (formatted-text (with-temp-buffer
                                         (insert text)
                                         (delete-trailing-whitespace)
                                         (sql-indent-buffer)
                                         (replace-string "\n" (concat "\n" (make-string column (string-to-char " "))) nil (point-min) (point-max))
                                         (buffer-string))))
                  (delete-region (region-beginning) (region-end))
                  (goto-char pos)
                  (insert formatted-text))))))

(use-package my/prog-mode
  :init (progn
          (use-package lorem-ipsum
            :ensure t)
          (use-package fixmee
            :config (progn
                      (add-hook 'prog-mode-hook 'fixmee-mode))
            :ensure t)))

(use-package python
  :commands python-mode
  :mode ("\\.py\\'" . python-mode)
  :interpreter ("ipython" . python-mode)
  :load-path "python/"
  :init (progn
          (use-package jedi
            :config (progn
                      (add-hook 'python-mode-hook 'jedi:setup))
            (jedi:install-server)
            :ensure t)

          (use-package pungi
            :ensure t)

          (use-package cinspect
            :ensure t)

          (use-package py-isort
            :ensure t)

          (use-package py-yapf
            :ensure t)

          (use-package virtualenv
            :init (progn
                    (use-package virtualenvwrapper
                      :ensure t)

                    ;; (use-package pyenv-mode
                    ;;   :ensure t)

                    (use-package pyvenv
                      :config (progn
                                (make-directory "~/.virtualenvs" t))
                      :ensure t))
            :ensure t)

          ;; https://github.com/davidmiller/pony-mode
          (use-package pony-mode
            :config ;; (progn
            ;;   ;; Pony mode config for the megacorp project
            ;;   ((nil . ;; This applies these settings regardless of major mode

            ;;         ((pony-settings (make-pony-project
            ;;                          :python "/home/david/virtualenvs/megacorp/production/bin/python"
            ;;                          :pythonpath "/home/david/megacorp/libs/projectzero"
            ;;                          :settings "local_settings_file"
            ;;                          :appsdir "testproject/apps/")
            ;;                         )))))
            :ensure t)

          ;; (use-package yasnippet
          ;;   :ensure t)
          ;; (use-package live-py-mode
          ;;   :ensure t)
          )
  :config (progn
            (setq-default python-indent-offset 4)

            (defun python-highlight-breakpoints ()
              (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))

            (defun python-add-breakpoint ()
              "Add a break point"
              (interactive)
              (insert "import ipdb; ipdb.set_trace()")
              (python-highlight-breakpoints))
            (add-hook 'python-mode-hook 'rainbow-delimiters-mode)
            (add-hook 'python-mode-hook 'linum-mode)
            ;; (add-hook 'python-mode-hook 'yas-minor-mode)
            ;; (add-hook 'python-mode-hook 'python-highlight-breakpoints)
            )
  :bind (("C-c C-b" . python-add-breakpoint))
  :ensure t)

(require 'init-isearch)

(use-package flycheck
  :config (progn
            (add-hook 'after-init-hook 'global-flycheck-mode)
            (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled)
                  flycheck-idle-change-delay 0.8
                  flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))
  :ensure t)

(use-package imenu-anywhere
  :config (progn
            (after-load 'imenu-anywhere (global-set-key (kbd "C-.") 'imenu-anywhere))
            (bind-key "C-x i" 'imenu-anywhere))
  :ensure t)

(use-package ido
  :config (progn
            (ido-mode)
            (ido-vertical-mode)
            (ido-everywhere)
            (ido-ubiquitous-mode)
            (setq ido-enable-flex-matching t
                  ido-use-filename-at-point nil
                  ido-auto-merge-work-directories-length -1
                  ido-use-virtual-buffers t
                  ido-confirm-unique-completion t
                  smex-save-file (concat emacs-persistence-directory ".smex-items")
                  ido-default-buffer-method 'selected-window
                  ido-save-directory-list-file (concat emacs-persistence-directory ".ido-last"))
            (global-set-key [remap execute-extended-command] 'smex)
            (defadvice ido-find-file (after find-file-sudo activate)
              "Find file as root if necessary."
              (unless (and buffer-file-name
                           (file-writable-p buffer-file-name))
                (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))
            (defun bind-ido-keys ()
              "Keybindings for ido mode."
              (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
              (define-key ido-completion-map (kbd "C-p") 'ido-prev-match))
            (add-hook 'ido-setup-hook (lambda () (define-key ido-completion-map [up] 'previous-history-element)))
            (add-hook 'ido-setup-hook #'bind-ido-keys))
  :ensure imenu-anywhere
  :ensure ido-vertical-mode
  :ensure idomenu
  :ensure ido-completing-read+
  :ensure ido-ubiquitous
  :ensure smex)

(require 'init-hippie-expand)

(use-package switch-window
  :config (progn
            (setq switch-window-shortcut-style 'alphabet)
            ;;----------------------------------------------------------------------------
            ;; When splitting window, show (other-buffer) in the new window
            ;;----------------------------------------------------------------------------
            (defun split-window-func-with-other-buffer (split-function)
              (lexical-let ((s-f split-function))
                (lambda ()
                  (interactive)
                  (funcall s-f)
                  (set-window-buffer (next-window) (other-buffer)))))
            ;;----------------------------------------------------------------------------
            ;; Rearrange split windows
            ;;----------------------------------------------------------------------------
            (defun split-window-horizontally-instead ()
              (interactive)
              (save-excursion
                (delete-other-windows)
                (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))
            (defun split-window-vertically-instead ()
              (interactive)
              (save-excursion
                (delete-other-windows)
                (funcall (split-window-func-with-other-buffer 'split-window-vertically))))
            (defun my/split-vertically ()
              (interactive)
              (funcall (split-window-func-with-other-buffer 'split-window-vertically)))
            (defun my/split-horizontally ()
              (interactive)
              (funcall (split-window-func-with-other-buffer 'split-window-horizontally))))
  :bind (("C-x o" . switch-window)
         ("C-x 1" . delete-other-windows)
         ("C-x 2" . my/split-vertically)
         ("C-x 3" . my/split-horizontally)
         ("\C-x|" . split-window-horizontally-instead)
         ("\C-x_" . split-window-vertically-instead))
  :ensure t)

(use-package web-mode
  :init (progn
          (use-package js
            :config (progn
                      (setq js-indent-level 4))
            :ensure t)
          (use-package emmet-mode
            :config (progn
                      (add-hook 'web-mode-hook 'emmet-mode))
            :commands emmet-mode
            :ensure t)
          (use-package web-beautify
            :ensure t))
  :config (progn
            (add-auto-mode 'web-mode "\\.html\\'")
            (add-auto-mode 'web-mode "\\.htm\\'")
            (add-auto-mode 'web-mode "\\.jsx\\'")
            (setq web-mode-markup-indent-offset 4
                  web-mode-css-indent-offset 4
                  web-mode-code-indent-offset 4))
  :ensure t)

(require 'init-editing-utils)
(require 'init-darcs)
(require 'init-csv)
(require 'init-javascript)
(require 'init-nxml)
(require 'init-haml)
(require 'init-paredit)
(require 'init-lisp)
(require 'init-misc)

(use-package org
  :config (progn
            (use-package org-fstree
              :ensure t)

            (when *is-a-mac*
              (use-package org-mac-link
                :ensure t)
              (autoload 'org-mac-grab-link "org-mac-link" nil t)
              (use-package org-mac-iCal
                :ensure t))

            (use-package org-crypt
              :disabled t
              :config
              (org-crypt-use-before-save-magic)
              (setq org-tags-exclude-from-inheritance (quote ("crypt")))
              (setq org-crypt-key nil))

            (define-key global-map (kbd "C-c l") 'org-store-link)
            (define-key global-map (kbd "C-c a") 'org-agenda)
            (define-key global-map (kbd "C-c c") 'org-capture)
            (define-key org-mode-map (kbd "C-M-n") 'org-forward-heading-same-level)
            (define-key org-mode-map (kbd "C-M-p") 'org-backward-heading-same-level)

            (setq org-log-done t
                  org-completion-use-ido t
                  org-edit-timestamp-down-means-later t
                  org-agenda-start-on-weekday nil
                  org-agenda-span 14
                  org-agenda-include-diary t
                  org-agenda-window-setup 'current-window
                  org-fast-tag-selection-single-key 'expert
                  org-export-kill-product-buffer-when-displayed t
                  org-tags-column 80)
            ;; Refile targets include this file and any file contributing to the agenda - up to 5 levels deep
            (setq org-refile-targets (quote ((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))))
            ;; Targets start with the file name - allows creating level 1 tasks
            (setq org-refile-use-outline-path (quote file))
            ;; Targets complete in steps so we start with filename, TAB shows the next level of targets etc
            (setq org-outline-path-complete-in-steps t)
            (setq org-todo-keywords
                  (quote ((sequence "TODO(t)" "STARTED(s)" "DELEGATED(D@/!)" "TESTING(T)" "|" "DONE(d!/!)")
                          (sequence "WAITING(w!/!)" "SOMEDAY(S)" "|" "CANCELLED(c!/!)"))))

            (setq org-ellipsis "..." )
            (setq org-hide-leading-stars t)
            (setq org-startup-indented t)

            (use-package org-clock
              :init (progn
                      (setq org-clock-persistence-insinuate t
                            org-clock-persist t
                            org-clock-in-resume t
                            org-clock-in-switch-to-state "STARTED"
                            org-clock-out-remove-zero-time-clocks t)))

            (use-package org-babel
              :init (progn
                      (org-babel-do-load-languages
                       'org-babel-load-languages
                       '((python . t)))
                      (setq org-src-fontify-natively t)))

            (use-package org-pomodoro
              :config (progn
                        (after-load 'org-agenda
                          (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)))
              :ensure t)

            (after-load 'org
              (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
              (when *is-a-mac*
                (define-key org-mode-map (kbd "M-h") nil))
              (define-key org-mode-map (kbd "C-M-<up>") 'org-up-element)
              (when *is-a-mac*
                (define-key org-mode-map (kbd "C-c g") 'org-mac-grab-link))
              (setq org-imenu-depth 10))

            (add-hook 'org-mode-hook (lambda () (modify-syntax-entry (string-to-char "") "w")))
            (setq org-startup-align-all-tables "align"))
  :ensure t)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;; Extra packages which don't require any configuration

(use-package multiple-cursors
  :commands (mc/mark-next-like-this
             mc/mark-previous-like-this
             mc/mark-all-like-this)
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this)))

(use-package regex-tool
  :commands regex-tool)

(use-package pdf-tools
  :commands pdf-tools-install)

(use-package hl-line+
  :config (progn
            (set-face-background hl-line-face "#363636")))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; (use-package hackernews
;;   :commands hackernews)

;; (use-package list-processes+
;;   :commands list-processes+)

(use-package camcorder
  :commands camcorder-mode)

(use-package nhexl-mode)

;; https://github.com/kiwanami/emacs-calfw
(use-package calfw
  :config
  (require 'calfw-org))

(use-package keyfreq
  :config (progn
            (keyfreq-mode t)
            (keyfreq-autosave-mode t)))

(use-package magit
  :commands (magit-status
             magit-blame
             magit-commit
             magit-push-current
             magit-log-buffer-file)
  :config (progn
            (use-package git-gutter+
              :config (progn
                        (global-git-gutter+-mode))
              :ensure t)
            (use-package github-clone
              :ensure t)
            (use-package yagist
              :ensure t)
            (use-package github-browse-file
              :ensure t)
            (use-package bug-reference-github
              :ensure t)
            (use-package magit-gh-pulls
              :ensure t)
            (setq magit-completing-read-function 'magit-ido-completing-read)
            ;; (magit-add-section-hook 'magit-status-sections-hook
            ;;                         'magit-insert-unpulled-module-commits
            ;;                         'magit-insert-unpulled-from-pushremote)
            )
  :bind (("C-x g s" . magit-status)
         ("C-x g b" . magit-blame)
         ("C-x g l" . magit-log-buffer-file)
         ("C-x g c" . magit-commit)
         ("C-x g p c" . magit-push-current))
  :ensure t)

(use-package my/project-management
  :init (progn
          (use-package dizzee
            :commands (dz-defservice dz-defservice-group)
            :config (progn
                      (defun dz-restart-current ()
                        (interactive)
                        (setq dz-buffer-name (replace-regexp-in-string "*" "" (buffer-name)))
                        (setq dz-restart-expr (concatenate 'string dz-buffer-name "-restart"))
                        (funcall (intern dz-restart-expr)))
                      (defun dz-stop-current ()
                        (interactive)
                        (setq dz-buffer-name (replace-regexp-in-string "*" "" (buffer-name)))
                        (setq dz-restart-expr (concatenate 'string dz-buffer-name "-stop"))
                        (funcall (intern dz-restart-expr)))
                      (global-set-key (kbd "<f5>") 'dz-restart-current)
                      (global-set-key (kbd "<f4>") 'dz-stop-current)))))

(use-package my/internet-services
  :init (progn
          (use-package google-translate
            :config (defun translate-text (sentence)
                      "Google translate without specifying language"
                      (interactive "sTranslate sentence: ")
                      (setq lang-regexes '(("[a-zA-Z]" . ("en" "ru"))
                                           ("[а-яА-Я]" . ("ru" "en"))))
                      (dolist (lang-regex lang-regexes)
                        (if (string-match (car lang-regex) sentence)
                            (google-translate-translate (nth 1 lang-regex) (nth 2 lang-regex) sentence))))
            :bind ("C-x y t t" . translate-text)
            :ensure t)
          ;; (use-package twittering-mode)
          ))

(use-package itail)

(use-package frame-cmds)

(use-package goto-chg
  :bind (("C-c b ," . goto-last-change)
         ("C-c b ." . goto-last-change-reverse)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(use-package my/project-management
  :init (progn
          (use-package bookmark+
            :init (progn
                    (setq-default bookmark-default-file (concat emacs-persistence-directory ".bookmarks")
                                  bookmark-file (concat emacs-persistence-directory ".bookmarks")
                                  bmkp-bmenu-state-file (concat emacs-persistence-directory ".emacs-bmk-bmenu-state")))
            :ensure t)))

(use-package my/process-management
  :init (progn
          (use-package elscreen
            :config (progn
                      (elscreen-start)
                      (setq elscreen-display-tab nil))
            :ensure t)

          (use-package lice
            :commands lice
            :ensure t)

          ;; https://github.com/ilya-babanov/emacs-bpr
          (use-package bpr
            :config (progn
                      (setq bpr-colorize-output t
                            bpr-close-after-success t
                            bpr-erase-process-buffer t)
                      (defun emacs-push-config (cm)
                        (interactive "MCommit message: ")
                        (let* ((bpr-process-directory user-emacs-directory))
                          (bpr-spawn (concatenate 'string "fab push:cm=\'" cm "\'")))))
            :ensure t)

          (use-package prodigy
            :commands (prodigy
                       prodigy-start-all-services)
            :bind ("C-x y p" . prodigy)
            :config (progn
                      (defun ido-prodigy-menu ()
                        (interactive)
                        (let* ((ido-prodigy-choices (mapcar (lambda (serv)
                                                              (let* ((status (prodigy-service-started-p serv))
                                                                     (service-name (cadr serv))
                                                                     (stopped-label service-name)
                                                                     (started-label  service-name))
                                                                (if status started-label
                                                                  stopped-label)))
                                                            prodigy-services)))
                          (ido-completing-read "Service: " ido-prodigy-choices)))

                      (defun find-prodigy-service-with-name (service-name)
                        (let ((matches (-filter (lambda (s) (string= service-name (cadr s)))
                                                prodigy-services)))
                          (car matches)))

                      (defun prodigy-apply-to-services (services fn)
                        (prodigy-with-refresh
                         (-each services fn)))

                      (defun prodigy-start-all-services ()
                        (interactive)
                        (prodigy-apply-to-services prodigy-services
                                                   'prodigy-start-service))

                      (defun prodigy-stop-all-services ()
                        (interactive)
                        (prodigy-apply-to-services prodigy-services
                                                   'prodigy-stop-service))

                      ;; TODO pull this feature
                      (defun prodigy-stop-services-with-tag (tag)
                        (interactive "MTag: ")
                        (prodigy-apply-to-services
                         (prodigy-services-tagged-with (intern tag))
                         'prodigy-stop-service))

                      ;; TODO pull this feature
                      (defun prodigy-start-services-with-tag (tag)
                        (interactive "MTag: ")
                        (prodigy-apply-to-services
                         (prodigy-services-tagged-with (intern tag))
                         'prodigy-start-service))
                      (prodigy-define-default-status-list)
                      (prodigy-define-tag
                        :name 'django
                        :ready-message "Quit the server with CONTROL-C"))
            :ensure t)))

(use-package my/dired
  :init (progn
          ;; (use-package dired+
          ;;   :config (progn
          ;;              (diredp-make-find-file-keys-reuse-dirs))
          ;;   :ensure t)

          (use-package dired)

          (use-package dired-subtree
            :ensure t)

          (use-package dired-filetype-face
            :config (progn
                      ;; (deffiletype-face-regexp dired-git-face
                      ;;   :extensions '("git")
                      ;;   :type-for-docstring "dired-git-face")
                      ;; (deffiletype-setup "dired-git-face" "dired-git-face")
                      )
            :ensure t)

          (defun mydired-sort ()
            "Sort dired listings with directories first."
            (save-excursion
              (let (buffer-read-only)
                (forward-line 2) ;; beyond dir. header
                (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
              (set-buffer-modified-p nil)))

          (add-hook 'dired-after-readin-hook 'hl-line-mode)
          (add-hook 'dired-after-readin-hook 'mydired-sort)
          (add-hook 'dired-after-readin-hook 'dired-omit-mode)

          (require 'dired-x)
          (dired-omit-mode 1)
          (setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

          (define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)
          ;; (define-key dired-mode-map (kbd "<return>") 'dired-subtree-toggle)
          (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-toggle)))

(use-package my/buffer-management
  :init (progn
          (use-package uniquify
            :disabled t
            :init (progn
                    (setq uniquify-buffer-name-style 'reverse
                          uniquify-separator " • "
                          uniquify-after-kill-buffer-p t
                          uniquify-ignore-buffers-re "^\\*")))

          (use-package ibuffer
            :init (progn
                    (use-package fullframe
                      :ensure t)
                    (use-package ibuffer-vc
                      :ensure t)
                    (use-package ibuffer-git
                      :ensure t))
            :config (progn
                      (setq ibuffer-formats
                            '((mark modified read-only vc-status-mini " "
                                    (name 18 18 :left :elide)
                                    " "
                                    (size-h 9 -1 :right)
                                    " "
                                    (mode 16 16 :left :elide)
                                    " "
                                    filename-and-process)
                              (mark modified read-only vc-status-mini " "
                                    (name 18 18 :left :elide)
                                    " "
                                    (size-h 9 -1 :right)
                                    " "
                                    (mode 16 16 :left :elide)
                                    " "
                                    (vc-status 16 16 :left)
                                    " "
                                    filename-and-process))
                            ibuffer-filter-group-name-face 'font-lock-doc-face)
                      (define-ibuffer-column size-h
                        (:name "Size" :inline t)
                        (cond
                         ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
                         ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
                         (t (format "%8d" (buffer-size)))))
                      (defun ibuffer-set-up-preferred-filters ()
                        (ibuffer-vc-set-filter-groups-by-vc-root)
                        (unless (eq ibuffer-sorting-mode 'filename/process)
                          (ibuffer-do-sort-by-filename/process)))
                      (after-load 'ibuffer
                        (fullframe ibuffer ibuffer-quit)))
            :bind ("C-x C-b" . ibuffer)
            :ensure t)

          (use-package super-save
            :config (progn
                      (super-save-initialize))
            :ensure t)))

(use-package my/http
  :init (progn
          ;; https://github.com/tkf/emacs-request
          ;; (use-package emacs-request)

          ;; https://github.com/emacs-pe/http.el
          (use-package http
            :ensure t)))

(require 'init-local nil t)

(provide 'init)

;;; init.el ends here
