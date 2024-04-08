;; (or (scala-indent:apply-indent-rules
;;        `((scala-indent:goto-line-comment-anchor 0)
;;          (scala-indent:goto-open-parentheses-anchor scala-indent:resolve-open-parentheses-step)
;;          (scala-indent:goto-for-enumerators-anchor scala-indent:resolve-list-step)
;;          (scala-indent:goto-forms-align-anchor scala-indent:resolve-forms-align-step)
;;          (scala-indent:goto-list-anchor scala-indent:resolve-list-step)
;;          (scala-indent:goto-body-anchor scala-indent:resolve-body-step)
;;          (scala-indent:goto-run-on-anchor scala-indent:resolve-run-on-step)
;;          (scala-indent:goto-block-anchor scala-indent:resolve-block-step)
;;      )
;;        point)
;;       0)

(defun sbt-amm-complete ()
  (with-current-buffer "*sbt-amm*"
    (comint-delete-input)
    (sbt-amm-send-string "java.\t")
    (comint-simple-send "*sbt-amm*" "java.\t\n\n")
    (comint-delete-input))


  (comint-simple-send "*sbt-amm*" "\n"))

(use-package xterm-color)

(defun sbt-amm-process-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (get-buffer-create "*sbt-amm-last-cmd-output*")
      (-some->> string
        xterm-color-filter
        substring-no-properties
        (s-replace "" "")
        s-trim
        (s-replace-regexp "^@.*" "")
        s-trim
        insert))))

(defun sbt-amm-at-comment-line-p ()
  (s-match "^\\W+//.*" (thing-at-point 'line)))

(defun sbt-amm-cleanup-comments (source)
  (let ((filename (car (s-split ":" source)))
        (lineno (string-to-number (cadr (s-split ":" source)))))
    (find-file filename)
    (save-excursion
      (goto-char (point-min))
      (forward-line lineno)
      (replace-regexp "\\s-*//.*" "" nil
                      (save-excursion (beginning-of-line) (point))
                      (save-excursion (end-of-line) (point)))
      (next-line)
      (beginning-of-line)
      (let ((kill-whole-line t))
        (while (sbt-amm-at-comment-line-p)
          (kill-line))))))

(defun sbt-amm-append-comments (source output)
  (let ((filename (car (s-split ":" source)))
        (lineno (string-to-number (cadr (s-split ":" source)))))
    (save-window-excursion
      (find-file filename)
      (save-excursion
        (goto-line lineno)
        (end-of-line)
        ;; (while (thing-at-point 'whitespace)
        ;;   (delete-char 1))
        (loop for line in (s-split "\n" output)
           do (insert " // " line)
             (scala-indent:indent-code-line)
             (insert "\n"))
        (backward-delete-char 1)))))

(defun sbt-amm-output-to-comments ()
  (interactive)
  (let ((source (with-current-buffer (get-buffer-create "*sbt-amm-last-cmd-output*")
                  (goto-char (point-min))
                  (search-forward "Source:\n")
                  (buffer-substring (point) (save-excursion (end-of-line) (point)))))
        (output (with-current-buffer (get-buffer-create "*sbt-amm-last-cmd-output*")
                  (goto-char (point-min))
                  (search-forward "Output:\n")
                  (buffer-substring (point) (point-max)))))
    (sbt-amm-cleanup-comments source)
    (sbt-amm-append-comments source output)))

(defvar sbt-amm-highlight-timer (timer-create))
(defvar sbt-amm-highlight-idle-timeout 0.01)

(defun sbt-amm-highlight-hook ()
  (when (string= major-mode "scala-mode")
    (cancel-timer sbt-amm-highlight-timer)
    (setq sbt-amm-highlight-timer
          (run-with-idle-timer
           sbt-amm-highlight-idle-timeout
           nil
           #'expal:highlight))))

;; (add-hook 'post-command-hook 'sbt-amm-highlight-hook)

(defun sbt-amm-send-string (string)
  (interactive)
  (let* ((output-buffer (get-buffer-create "*sbt-amm-last-cmd-output*"))
         (source-buffer (current-buffer))
         (source-filename (buffer-file-name))
         (source-position (save-excursion
                            (goto-char end-of-expression)
                            (line-number-at-pos))))

    (with-current-buffer output-buffer
      (delete-region (point-min) (point-max))

      (let ((source (format "%s:%d" source-filename source-position))
            (input (with-temp-buffer
                     (loop for line in (s-split "\n" paragraph)
                        do (-some->> line
                             s-trim
                             insert)
                        finally (return (buffer-string))))))
        (insert (format "Source:\n%s\n\n" source))
        (insert (format "Input:\n%s\n\n" input))
        (insert "Output:\n")
        (comint-simple-send "*sbt-amm*" input)))
    (switch-to-buffer-other-window output-buffer t)
    (goto-char (point-max))
    (switch-to-buffer-other-window source-buffer)))

(defun sbt-amm-send-region (from to)
  (interactive)
  (let* ((beginning-of-expression from)
         (end-of-expression to)
         (paragraph (cond ((and (<= beginning-of-expression (point))
                                (>= end-of-expression (point)))
                           (condition-case nil
                               (buffer-substring-no-properties beginning-of-expression end-of-expression)
                             (error (buffer-substring-no-properties beginning-of-expression (point-max))))
                           )))
         (output-buffer (get-buffer-create "*sbt-amm-last-cmd-output*"))
         (source-buffer (current-buffer))
         (source-filename (buffer-file-name))
         (source-position (save-excursion
                            (goto-char end-of-expression)
                            (line-number-at-pos))))

    (hlt-unhighlight-region (point-min) (point-max))
    (hlt-highlight-region beginning-of-expression end-of-expression)

    (with-current-buffer output-buffer
      (delete-region (point-min) (point-max))

      (let ((source (format "%s:%d" source-filename source-position))
            (input (with-temp-buffer
                     (loop for line in (s-split "\n" paragraph)
                        do (progn
                             (insert (-some->> line
                                       ;; (s-replace-regexp "//.*" "")
                                       s-trim))
                             (insert "\n"))
                        finally (return (buffer-string))))))
        (insert (format "Source:\n%s\n\n" source))
        (insert (format "Input:\n%s\n\n" input))
        (insert "Output:\n")
        (comint-simple-send "*sbt-amm*" input)))
    (switch-to-buffer-other-window output-buffer t)
    (goto-char (point-max))
    (switch-to-buffer-other-window source-buffer)))

(defun sbt-amm:eval ()
  (interactive)
  (let* ((bounds (expal:bounds))
         (beginning-of-expression (car bounds))
         (end-of-expression (cdr bounds)))
    (sbt-amm-send-region beginning-of-expression end-of-expression)))

(cl-defun sbt-amm:step-backward ()
  (interactive)
  (scala-syntax:beginning-of-code-line)
  (condition-case nil
      (when (> (point) (point-min))
        (scala-syntax:backward-sexp)
        (scala-syntax:skip-backward-ignorable)
        (scala-syntax:beginning-of-code-line))
    (error nil)))

(cl-defun sbt-amm:step-forward ()
  (interactive)
  (let* ((bounds (expal:bounds))
         (beginning-of-expression (car bounds))
         (end-of-expression (cdr bounds)))
    (goto-char end-of-expression)
    (condition-case nil
        (unless (loop--last-line-p)
          (scala-syntax:forward-sexp-or-next-line)
          (scala-syntax:skip-forward-ignorable)
          (scala-syntax:forward-token)
          (scala-syntax:beginning-of-code-line))
      (error nil))))

(cl-defun sbt-amm:eval-step-forward ()
  (interactive)
  (sbt-amm:eval)
  (sbt-amm:step-forward))

(cl-defun sbt-amm:eval-step-backward ()
  (interactive)
  (sbt-amm:eval)
  (sbt-amm:step-backward))

(defun sbt-amm-send ()
  (interactive)
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "e") 'sbt-amm:eval)
    (define-key map (kbd "c") 'sbt-amm:eval-step-forward)
    (define-key map (kbd "C") 'sbt-amm:eval-step-backward)
    (define-key map (kbd "n") 'sbt-amm:step-forward)
    (define-key map (kbd "p") 'sbt-amm:step-backward)
    (set-transient-map map t)
    (sbt-amm:eval)))

(defun run-sbt-amm ()
  "Run an inferior instance of `sbt-amm-cli' inside Emacs."
  (interactive)
  (unless (-some->> "*sbt-amm*" get-buffer-process process-status)
    (let* ((default-directory "/home/akatovda/sync/stuff/scala-spark-sandbox")
           (sbt-amm-program sbt-amm-cli-file-path)
           (buffer (comint-check-proc "sbt-amm")))
      (save-window-excursion
        (pop-to-buffer-same-window
         (if (or buffer (not (derived-mode-p 'sbt-amm-mode))
                 (comint-check-proc (current-buffer)))
             (get-buffer-create (or buffer "*sbt-amm*"))
           (current-buffer)))
        (unless buffer
          (make-comint-in-buffer "sbt-amm" buffer sbt-amm-program nil "test:run")
          (sbt-amm-mode))))
    (when-let (process (get-buffer-process "*sbt-amm*"))
      (set-process-filter process #'sbt-amm-process-filter))))

(defvar sbt-amm-cli-file-path "/usr/bin/sbt"
  "Path to the program used by `run-sbt-amm")

(defvar sbt-amm-cli-arguments '()
  "Commandline arguments to pass to `sbt-amm-cli'")

(defvar sbt-amm-mode-map
  (let ((map (nconc (make-sparse-keymap) comint-mode-map)))
    ;; example definition
    (define-key map "\t" 'completion-at-point)
    map)
  "Basic mode map for `run-sbt-amm'")

(defvar sbt-amm-prompt-regexp "^\\(?:\\[[^@]+@[^@]+\\]\\)"
  "Prompt for `run-sbt-amm'.")

(defun sbt-amm--initialize ()
  "Helper function to initialize Sbt-Amm"
  (setq comint-process-echoes t)
  (setq comint-use-prompt-regexp t))

(define-derived-mode sbt-amm-mode comint-mode "Sbt-Amm"
                     "Major mode for `run-sbt-amm'.

\\<sbt-amm-mode-map>"
                     nil "Sbt-Amm"
                     ;; this sets up the prompt so it matches things like: [foo@bar]
                     (setq comint-prompt-regexp sbt-amm-prompt-regexp)
                     ;; this makes it read only; a contentious subject as some prefer the
                     ;; buffer to be overwritable.
                     (setq comint-prompt-read-only t)
                     ;; this makes it so commands like M-{ and M-} work.
                     (set (make-local-variable 'paragraph-separate) "\\'")
                     ;; (set (make-local-variable 'font-lock-defaults) '(sbt-amm-font-lock-keywords t))
                     (set (make-local-variable 'paragraph-start) sbt-amm-prompt-regexp))

;; this has to be done in a hook. grumble grumble.
(add-hook 'sbt-amm-mode-hook 'sbt-amm--initialize)

(provide-me)
