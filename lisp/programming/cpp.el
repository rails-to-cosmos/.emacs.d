;; https://tuhdo.github.io/c-ide.html

(require 'load-relative)

(use-package modern-cpp-font-lock
    :hook ((irony-mode . modern-c++-font-lock-mode)))

;; (use-package disaster)

(use-package rmsbolt
    :hook ((cc-mode . rmsbolt-mode)))

(defun my/cpp-smart-complete ()
  (when (and (string= major-mode "c++-mode")
             (looking-at "$")
             (eq this-command 'self-insert-command))
    (let* ((line (condition-case nil
                     (substring-no-properties (thing-at-point 'line))
                   (error "")))
           (ws (condition-case nil
                   (substring-no-properties (thing-at-point 'whitespace))
                 (error "")))
           (words (s-split-words line))
           (init-cmd (car words))
           (last-cmd (car (last words))))
      (when (looking-back "^\\W*\\." 1)  ;; dot completions
        (delete-char -1)
        ;; (call-interactively 'company-yasnippet)
        (my:with-ivy-posframe--safe
          (ivy-yasnippet))))))

(defun my:senator-highlight ()
  (when-let (tag (semantic-current-tag))
    (hlt-unhighlight-region (point-min) (point-max))
    (hlt-highlight-region (semantic-tag-start tag) (semantic-tag-end tag))))

(defun my:senator-kill-tag ()
  (interactive)
  (condition-case nil
      (senator-kill-tag)
    (error
     (kill-line)))
  (my:senator-next-highlight))

(defvar my:cpp-highlight-timer (timer-create))
(defvar my:cpp-highlight-idle-timeout 0.5)

(defun my:cpp-highlight-hook ()
  (when (string= major-mode "c++-mode")
    (cancel-timer my:cpp-highlight-timer)
    (setq my:cpp-highlight-timer
          (run-with-idle-timer
           my:cpp-highlight-idle-timeout
           nil
           #'my:senator-highlight))))

(use-package cc-mode
    :hook (post-command . my/cpp-smart-complete)
          (post-command . my:cpp-highlight-hook)
    :bind (("C-M-n" . senator-next-tag)
           ("C-M-p" . senator-previous-tag)
           ("C-M-u" . senator-go-to-up-reference)
           ("C-M-k" . my:senator-kill-tag)
           ("C-M-y" . senator-yank-tag)
           ("C-M-f" . senator-fold-tag-toggle)
           ("C-M-w" . senator-copy-tag)))

(use-package ede
    :config (global-ede-mode))
