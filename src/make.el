;;; make.el --- Makefile target picker with output buffer  -*- lexical-binding: t -*-

;;; Commentary:
;; M-x make-completing-read picks a target from the dominating Makefile
;; and spawns `make TARGET' as an async process.  Output streams into
;; a buffer shown in another window.
;; Status also appears in the global mode line with mouse interaction.

;;; Code:

(require 'cl-lib)
(require 'ansi-color)
(require 'transient)

;;; Per-buffer state

(defvar make--buffers nil
  "List of tracked make output buffers (newest first).")

(defvar-local make--target nil
  "Make target running in this buffer.")

(defvar-local make--project nil
  "Project name (basename of the directory containing the Makefile).")

(defvar-local make--status 'running
  "Buffer-local status: `running', `ok', or (fail . EXIT-CODE).")

(defvar-local make--start-time nil
  "Time the make process was spawned (for elapsed-time display).")

(defvar-local make--process nil
  "Async make process for this buffer.")

;;; Faces

(defface make-status-running-face
  '((t :inherit warning))
  "Face for a running make in the mode line.")

(defface make-status-ok-face
  '((t :inherit success))
  "Face for a successful make in the mode line.")

(defface make-status-fail-face
  '((t :inherit error))
  "Face for a failed make in the mode line.")

(defface make-header-face
  '((t :inherit header-line :slant italic))
  "Face for the output buffer header line."
  :group 'make)

;;; Customization

(defgroup make nil
  "Makefile target picker with output buffer."
  :group 'tools
  :prefix "make-")

(defcustom make-mode-line-cleanup-delay 60
  "Seconds after completion before the mode-line entry auto-disappears.
Nil disables auto-cleanup."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Never" nil))
  :group 'make)

;;; Major mode

(defvar make-output-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")       #'bury-buffer)
    (define-key map (kbd "C-c C-k") #'bury-buffer)
    (define-key map (kbd "C-c C-c") #'make-kill-process)
    map)
  "Keymap for `make-output-mode'.")

(define-derived-mode make-output-mode special-mode "Make"
  "Major mode for make output."
  (setq-local truncate-lines nil)
  (setq-local word-wrap t)
  (ansi-color-for-comint-mode-on))

;;; Commands

(defun make-kill-process ()
  "Kill the running make process."
  (interactive)
  (when (process-live-p make--process)
    (kill-process make--process)
    (message "make %s: killed" make--target)))

;;; Mode-line construct

(defun make--status-glyph (status)
  "Mode-line label + face for STATUS."
  (pcase status
    ('running     (propertize "[RUN]"  'face 'make-status-running-face))
    ('ok          (propertize "[OK]"   'face 'make-status-ok-face))
    (`(fail . ,n) (propertize (format "[FAIL %d]" n)
                              'face 'make-status-fail-face))
    (_            "[?]")))

(defun make--entry-help (buf)
  "Help-echo for BUF's mode-line entry."
  (with-current-buffer buf
    (format "%s\nstatus: %s\nelapsed: %.1fs\nmouse-1 show, mouse-3 kill"
            (buffer-name buf)
            (pcase make--status
              ('running     "running")
              ('ok          "ok")
              (`(fail . ,n) (format "failed (%d)" n))
              (_            "?"))
            (if make--start-time
                (float-time (time-since make--start-time))
              0.0))))

(defun make--entry-keymap (buf)
  "Mode-line keymap for BUF: mouse-1 shows buffer, mouse-3 kills."
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1]
                (lambda (_e) (interactive "e") (make--show-buffer buf)))
    (define-key m [mode-line mouse-3]
                (lambda (_e)
                  (interactive "e")
                  (when (y-or-n-p (format "Kill %s? " (buffer-name buf)))
                    (kill-buffer buf))))
    m))

(defun make--mode-line-entry (buf)
  "Propertized mode-line string for BUF."
  (with-current-buffer buf
    (propertize (format "%s%s%s"
                        (if make--project (concat make--project "/") "")
                        (or make--target "?")
                        (make--status-glyph make--status))
                'mouse-face 'mode-line-highlight
                'help-echo (make--entry-help buf)
                'local-map (make--entry-keymap buf))))

(defun make--mode-line-construct ()
  "Mode-line value: tracked make buffers with status, or empty when none."
  (setq make--buffers (cl-remove-if-not #'buffer-live-p make--buffers))
  (when make--buffers
    (concat " make:"
            (mapconcat #'make--mode-line-entry make--buffers " "))))

(defvar make-mode-line-format '(:eval (make--mode-line-construct))
  "Construct added to `global-mode-string' to surface tracked makes.")
(put 'make-mode-line-format 'risky-local-variable t)

(unless (member make-mode-line-format
                (or (default-value 'global-mode-string) '("")))
  (setq-default global-mode-string
                (append (or (default-value 'global-mode-string) '(""))
                        (list make-mode-line-format))))

;;; Tracker

(defun make--register (buf)
  "Add BUF to the tracker; replace any previous entry with the same name."
  (setq make--buffers
        (cons buf (cl-remove buf make--buffers
                             :test (lambda (a b)
                                     (equal (buffer-name a) (buffer-name b))))))
  (force-mode-line-update t))

(defun make--unregister ()
  "Drop the current buffer from the tracker (kill-buffer-hook target)."
  (setq make--buffers (delq (current-buffer) make--buffers))
  (force-mode-line-update t))

;;; Process plumbing

(defun make--filter (proc chunk)
  "Process filter: append CHUNK with ANSI colors to PROC's buffer."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (save-excursion
          (goto-char (point-max))
          (let* ((no-crlf (replace-regexp-in-string "\r\n" "\n" chunk))
                 (no-osc (replace-regexp-in-string "\e\\][^\a]*\\(?:\a\\|\e\\\\\\)" "" no-crlf))
                 (no-csi (replace-regexp-in-string "\e\\[[?<>=]*[0-9;]*[a-zA-Z]" "" no-osc))
                 (no-esc (replace-regexp-in-string "\e" "" no-csi))
                 (clean  (ansi-color-apply no-esc))
                 (segments (split-string clean "\r")))
            (insert (car segments))
            (dolist (seg (cdr segments))
              (delete-region (line-beginning-position) (point))
              (insert seg))))
        (goto-char (point-max))
        (dolist (win (get-buffer-window-list (current-buffer) nil t))
          (set-window-point win (point-max)))))))

(defun make--sentinel (proc _event)
  "Update status when PROC exits; update header and schedule cleanup."
  (when (and (memq (process-status proc) '(exit signal))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((code (process-exit-status proc))
            (inhibit-read-only t))
        (setq-local make--status
                    (if (zerop code) 'ok (cons 'fail code)))
        (setq-local make--process nil)
        (let ((elapsed (if make--start-time
                          (float-time (time-since make--start-time))
                        0.0)))
          (save-excursion
            (goto-char (point-max))
            (insert (propertize
                     (format "\n--- %s (%.1fs) ---"
                             (pcase make--status
                               ('ok "done")
                               (`(fail . ,n) (format "failed (%d)" n)))
                             elapsed)
                     'face (if (eq make--status 'ok)
                               'make-status-ok-face
                             'make-status-fail-face)))))
        (setq header-line-format
              (propertize
               (format " make %s/%s %s  q close"
                       make--project make--target
                       (make--status-glyph make--status))
               'face 'make-header-face))
        (force-mode-line-update t)
        (message "make %s: %s" make--target
                 (pcase make--status
                   ('ok "ok")
                   (`(fail . ,n) (format "failed (%d)" n)))))
      (when make-mode-line-cleanup-delay
        (let ((buf (current-buffer)))
          (run-at-time make-mode-line-cleanup-delay nil
                       (lambda ()
                         (when (buffer-live-p buf)
                           (setq make--buffers (delq buf make--buffers))
                           (force-mode-line-update t)))))))))

;;; Show buffer

(defun make--show-buffer (buf)
  "Show BUF in another window."
  (pop-to-buffer buf))

;;; Spawn

(defun make--spawn (buffer-name target project dir)
  "Run `make TARGET' in BUFFER-NAME as an async process.
DIR is the directory containing the Makefile.
PROJECT is shown in the mode-line and header."
  (when (buffer-live-p (get-buffer buffer-name))
    (let ((old-proc (get-buffer-process buffer-name)))
      (when (and old-proc (process-live-p old-proc))
        (kill-process old-proc)))
    (kill-buffer buffer-name))
  (let ((buf (get-buffer-create buffer-name)))
    (with-current-buffer buf
      (make-output-mode)
      (setq-local make--target target)
      (setq-local make--project project)
      (setq-local make--status 'running)
      (setq-local make--start-time (current-time))
      (setq header-line-format
            (propertize (format " make %s/%s [RUN]  C-c C-c kill | q close"
                                project target)
                        'face 'make-header-face))
      (add-hook 'kill-buffer-hook #'make--unregister nil t)
      (let* ((default-directory dir)
             (proc (start-process "make" buf "make" target)))
        (setq-local make--process proc)
        (set-process-filter   proc #'make--filter)
        (set-process-sentinel proc #'make--sentinel)))
    (make--register buf)
    (make--show-buffer buf)))

;;; Entry point

(cl-defun make-completing-read ()
  "Pick a target from the dominating Makefile and run it."
  (interactive)
  (let* ((dir (or (locate-dominating-file default-directory "Makefile")
                  (user-error "No Makefile found above %s"
                              (abbreviate-file-name default-directory))))
         (default-directory dir)
         (makefile (expand-file-name "Makefile" dir))
         (targets (with-temp-buffer
                    (insert-file-contents makefile)
                    (cl-loop while (re-search-forward
                                    "^\\([^#[:space:]\n][^:[:space:]]*\\):" nil t)
                             for tgt = (match-string 1)
                             unless (string= tgt ".PHONY")
                             collect tgt into ts
                             finally (return (sort ts #'string<)))))
         (_ (unless targets (user-error "No targets in %s" makefile)))
         (target (completing-read "Make: " targets nil t))
         (project (file-name-base (directory-file-name dir)))
         (buf-name (format "*make-%s-%s*" project target)))
    (make--spawn buf-name target project dir)))

;;;###autoload
(defun make-switch-buffer ()
  "Switch between tracked make buffers, showing status."
  (interactive)
  (setq make--buffers (cl-remove-if-not #'buffer-live-p make--buffers))
  (unless make--buffers
    (user-error "No make buffers"))
  (let ((entries (cl-loop for buf in make--buffers
                          collect (cons (with-current-buffer buf
                                         (format "%s/%s %s"
                                                 (or make--project "?")
                                                 (or make--target "?")
                                                 (make--status-glyph make--status)))
                                        buf))))
    (let* ((choice (completing-read "Make buffer: "
                                    (mapcar #'car entries) nil t))
           (buf (cdr (assoc choice entries))))
      (make--show-buffer buf))))

;;; Transient

;;;###autoload
(transient-define-prefix make-menu ()
  "Make commands."
  [["Make"
    ("m" "Run target"       make-completing-read)
    ("b" "Switch buffer"    make-switch-buffer)]])

(provide 'make)

;;; make.el ends here
