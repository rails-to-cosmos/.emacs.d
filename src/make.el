;;; make.el --- Makefile target picker, backgrounded with mode-line status  -*- lexical-binding: t -*-

;;; Commentary:
;; M-x make-completing-read picks a target from the dominating Makefile
;; and spawns `make TARGET' in a buried *make-PROJECT-TARGET* vterm.  The
;; vterm runs make as its shell directly, so the process exit code maps
;; cleanly to a mode-line glyph: ● running, ✓ ok, ✗N failed.  Mouse-1 on
;; the entry visits the buffer; mouse-3 kills it.  Re-running the same
;; target replaces the tracker entry.

;;; Code:

(require 'cl-lib)

(declare-function vterm "vterm")
(declare-function eshell-send-input "esh-mode")
(defvar vterm-shell)
(defvar vterm-kill-buffer-on-exit)
(defvar eshell-buffer-name)

;;; Per-buffer state

(defvar make--buffers nil
  "List of tracked make output buffers (newest first).")

(defvar-local make--target nil
  "Make target running in this buffer.")

(defvar-local make--status 'running
  "Buffer-local status: `running', `ok', or (fail . EXIT-CODE).")

(defvar-local make--start-time nil
  "Time the make process was spawned (for elapsed-time display).")

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

;;; Mode-line construct

(defcustom make-mode-line-cleanup-delay 60
  "Seconds after a make completes before its mode-line entry auto-disappears.
The buffer stays around (buried), so you can still revisit it via
`switch-to-buffer'; only the mode-line clutter is cleaned up.
Nil disables auto-cleanup."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Never" nil))
  :group 'make)

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
    (format "%s\nstatus: %s\nelapsed: %.1fs\nmouse-1 visit, mouse-3 kill"
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
  "Mode-line keymap for BUF: mouse-1 visits, mouse-3 kills."
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1]
                (lambda (_e) (interactive "e") (switch-to-buffer buf)))
    (define-key m [mode-line mouse-3]
                (lambda (_e)
                  (interactive "e")
                  (when (y-or-n-p (format "Kill %s? " (buffer-name buf)))
                    (kill-buffer buf))))
    m))

(defun make--mode-line-entry (buf)
  "Propertized mode-line string for BUF."
  (with-current-buffer buf
    (propertize (format "%s%s"
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

(defun make--sentinel (proc _event)
  "Update buffer-local status when PROC (the vterm `make' process) exits.
After updating the status, schedule a per-buffer mode-line cleanup so the
finished entry disappears from the mode line after
`make-mode-line-cleanup-delay' seconds.  The buffer itself stays alive."
  (when (and (memq (process-status proc) '(exit signal))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((code (process-exit-status proc)))
        (setq-local make--status
                    (if (zerop code) 'ok (cons 'fail code)))
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

;;; Spawn

(defun make--spawn-vterm (buffer-name target)
  "Spawn `make TARGET' as the vterm shell in BUFFER-NAME, tracked."
  (let ((vterm-shell (format "make %s" target))
        ;; Keep the buffer alive after make exits so the user can scroll
        ;; back to read failures via mouse-1 from the mode line.
        (vterm-kill-buffer-on-exit nil))
    (save-window-excursion
      (vterm buffer-name)
      (with-current-buffer buffer-name
        (setq-local vterm-kill-buffer-on-exit nil)
        (setq-local make--target target)
        (setq-local make--status 'running)
        (setq-local make--start-time (current-time))
        (add-hook 'kill-buffer-hook #'make--unregister nil t)
        (let ((proc (get-buffer-process (current-buffer))))
          (when proc
            (set-process-sentinel proc #'make--sentinel))))))
  (make--register (get-buffer buffer-name))
  (when (get-buffer buffer-name)
    (bury-buffer buffer-name)))

(defun make--spawn-eshell (buffer-name target)
  "Fallback when vterm isn't available — no status tracking on this path."
  (with-current-buffer (let ((eshell-buffer-name buffer-name)) (eshell))
    (insert (format "make %s" target))
    (eshell-send-input)))

(defun make--spawn (buffer-name target)
  "Run `make TARGET' in BUFFER-NAME; prefer vterm-with-tracking."
  (when (buffer-live-p (get-buffer buffer-name))
    (kill-buffer buffer-name))
  (if (fboundp 'vterm)
      (condition-case err
          (make--spawn-vterm buffer-name target)
        (error
         (message "Warning: vterm failed (%s), falling back to eshell"
                  (error-message-string err))
         (make--spawn-eshell buffer-name target)))
    (message "Warning: vterm not available, falling back to eshell")
    (make--spawn-eshell buffer-name target)))

;;; Entry point

(cl-defun make-completing-read ()
  "Pick a target from the dominating Makefile and spawn it in the background.
Output goes to a buried *make-PROJECT-TARGET* buffer; status appears in
the global mode line."
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
    (make--spawn buf-name target)
    (message "Started: make %s  (status in mode line; mouse-1 to visit)"
             target)))

(provide 'make)

;;; make.el ends here
