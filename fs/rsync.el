;;; Code:

(defun init-rsync () "Init rsync with my configuration." (interactive))

(defun rsync-enable ()
  "Enable rsync support."
  (interactive)
  (auto-rsync-mode t))

(defun rsync-disable ()
  "Disable rsync support."
  (interactive)
  (auto-rsync-mode -1))

;;; auto-rsync-mode -- minor mode for auto rsync
;;
;; Author: @l3msh0
;;

;;; Example
;;
;; (require 'auto-rsync)
;; (auto-rsync-mode t)
;; (setq auto-rsync-dir-alist
;;       (("/path/to/src1/" . "/path/to/dest1/")
;;        ("/path/to/src2/" . "username@hostname:/path/to/dest2/")))

;;; Customize
;;
;; (defgroup auto-rsync nil "Auto rsync")
;; (defcustom auto-rsync-command "rsync" "rsync command path" :group 'auto-rsync)
;; (defcustom auto-rsync-command-option "-avzq" "rsync command option" :group 'auto-rsync)
(defgroup auto-rsync nil "Auto rsync")
(defcustom auto-rsync-command "rsync" "rsync command path." :group 'auto-rsync)
(defcustom auto-rsync-command-option "-av --exclude='.git' --exclude='node_modules' --exclude='tmp' --exclude='*.org*' --exclude='lisp' --exclude='LICENSE' --exclude='README.md'" "options" :group 'auto-rsync)


;;; TODO
;;
;; open remote counterpart
;;

(defvar auto-rsync-dir-alist nil "Pair of rsync source and destination dir.")
(defvar auto-rsync-normalized-alist nil)

;;; Code

(defun auto-rsync-exec-rsync ()
  "Execute rsync if editing file path match src dir."
  (interactive)
  (let* ((normalized-alist (mapcar (lambda (x) (cons (file-name-as-directory (expand-file-name (car x)))
                                                     (file-name-as-directory (cdr x))))
                                   auto-rsync-dir-alist))
         (sync-pair (assoc-if (lambda (key) (string-match key buffer-file-name)) normalized-alist)))
    (when sync-pair
      (save-window-excursion
        ;; avoid annoying shell command window
        (message (format "%s %s %s %s&" auto-rsync-command auto-rsync-command-option (car sync-pair) (cdr sync-pair)))
        (shell-command (format "%s %s %s %s&" auto-rsync-command auto-rsync-command-option (car sync-pair) (cdr sync-pair)) nil)))))

(define-minor-mode auto-rsync-mode
  "automatically execute rsync when editing file's path matches `auto-rsync-dir-alist`"
  :lighter " rsync"
  :global t
  (cond (auto-rsync-mode
         (add-hook 'after-save-hook 'auto-rsync-exec-rsync))
        (t
         (remove-hook 'after-save-hook 'auto-rsync-exec-rsync))))

(provide 'rsync)
;;; rsync.el ends here
