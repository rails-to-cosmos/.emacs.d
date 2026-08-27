;;; make.el --- Makefile target picker running in vterm  -*- lexical-binding: t -*-

;;; Commentary:
;; M-x make-completing-read picks a target from the dominating Makefile
;; and runs `make TARGET' in a vterm buffer shown in another window.
;; The terminal is interactive: sudo passwords and confirmations can
;; be typed in.  Status also appears in the global mode line with
;; mouse interaction.

;;; Code:

(require 'cl-lib)
(require 'vterm)
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
  "Makefile target picker running in vterm."
  :group 'tools
  :prefix "make-")

(defcustom make-mode-line-cleanup-delay 60
  "Seconds after completion before the mode-line entry auto-disappears.
Nil disables auto-cleanup."
  :type '(choice (integer :tag "Seconds")
                 (const :tag "Never" nil))
  :group 'make)

;;; Keys

(defvar make-done-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")       #'bury-buffer)
    (define-key map (kbd "C-c C-k") #'bury-buffer)
    map)
  "Local map installed once the make process has exited.
Replaces `vterm-mode-map' so keys read the output instead of feeding
a dead terminal.")

;;; Commands

(defun make-kill-process ()
  "Kill the running make process."
  (interactive)
  (when (process-live-p vterm--process)
    (kill-process vterm--process)
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

(defun make--status-label (status)
  "Plain-text label for STATUS."
  (pcase status
    ('running     "running")
    ('ok          "ok")
    (`(fail . ,n) (format "failed (%d)" n))
    (_            "?")))

(defun make--elapsed ()
  "Seconds since the make process in this buffer was spawned."
  (if make--start-time
      (float-time (time-since make--start-time))
    0.0))

(defun make--entry-help (buf)
  "Help-echo for BUF's mode-line entry."
  (with-current-buffer buf
    (format "%s\nstatus: %s\nelapsed: %.1fs\nmouse-1 show, mouse-3 kill"
            (buffer-name buf)
            (make--status-label make--status)
            (make--elapsed))))

(defun make--entry-keymap (buf)
  "Mode-line keymap for BUF: mouse-1 shows buffer, mouse-3 kills."
  (let ((m (make-sparse-keymap)))
    (define-key m [mode-line mouse-1]
                (lambda (_e) (interactive "e") (make--show-buffer buf)))
    (define-key m [mode-line mouse-3]
                (lambda (_e)
                  (interactive "e")
                  (when (y-or-n-p (format "Kill %s? " (buffer-name buf)))
                    (let ((kill-buffer-query-functions nil))
                      (kill-buffer buf)))))
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

(defun make--header (keys)
  "Header line for the current make buffer, ending with the KEYS hint."
  (propertize (format " make %s/%s %s  %s"
                      make--project make--target
                      (make--status-glyph make--status)
                      keys)
              'face 'make-header-face))

(defun make--sentinel (proc _event)
  "Record PROC's exit status; update header, mode line and keys."
  (when (and (memq (process-status proc) '(exit signal))
             (buffer-live-p (process-buffer proc)))
    (with-current-buffer (process-buffer proc)
      (let ((code (process-exit-status proc)))
        (setq-local make--status (if (zerop code) 'ok (cons 'fail code))))
      (use-local-map make-done-map)
      (setq buffer-read-only t)
      (setq header-line-format
            (make--header (format "(%.1fs)  q close" (make--elapsed))))
      (force-mode-line-update t)
      (message "make %s: %s" make--target (make--status-label make--status))
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
  "Run `make TARGET' in a vterm buffer named BUFFER-NAME.
DIR is the directory containing the Makefile.
PROJECT is shown in the mode-line and header."
  (when (buffer-live-p (get-buffer buffer-name))
    (let ((old-proc (get-buffer-process buffer-name)))
      (when (and old-proc (process-live-p old-proc))
        (kill-process old-proc)))
    (let ((kill-buffer-query-functions nil))
      (kill-buffer buffer-name)))
  (let ((buf (get-buffer-create buffer-name)))
    ;; Display first so vterm sizes the terminal to its window.
    (make--show-buffer buf)
    (with-current-buffer buf
      (let ((default-directory dir)
            (vterm-shell (concat "make " (shell-quote-argument target)))
            (vterm-kill-buffer-on-exit nil))
        (vterm-mode))
      ;; `vterm-mode' resets local state, so set ours after it.
      (setq-local make--target target
                  make--project project
                  make--status 'running
                  make--start-time (current-time))
      (setq header-line-format (make--header "C-c C-c interrupt"))
      (add-hook 'kill-buffer-hook #'make--unregister nil t)
      (set-process-sentinel vterm--process #'make--sentinel))
    (make--register buf)))

;;; Makefile parsing

(defconst make--rule-regexp "^\\([^#\t\n][^:#=\n]*\\):[^=]"
  "Match a rule line, capturing every target it names before the colon.
One rule may name several targets, as in `major minor patch:'.
The trailing character keeps a `:=' assignment out.")

(defun make--own-target-p (target)
  "Is TARGET one make itself reads rather than one a user would pick?
Covers make's dot-targets (`.PHONY' and its like), pattern rules, and
targets spelled through a variable."
  (or (string-prefix-p "." target)
      (string-match-p "[%$]" target)))

(defun make--targets (makefile)
  "Every target MAKEFILE offers, sorted and without repeats."
  (with-temp-buffer
    (insert-file-contents makefile)
    (cl-loop while (re-search-forward make--rule-regexp nil t)
             append (split-string (match-string 1) "[ \t]+" t) into ts
             finally (return (sort (cl-remove-if #'make--own-target-p
                                                 (delete-dups ts))
                                   #'string<)))))

;;; Entry point

(cl-defun make-completing-read ()
  "Pick a target from the dominating Makefile and run it."
  (interactive)
  (let* ((dir (or (locate-dominating-file default-directory "Makefile")
                  (user-error "No Makefile found above %s"
                              (abbreviate-file-name default-directory))))
         (default-directory dir)
         (makefile (expand-file-name "Makefile" dir))
         (targets (make--targets makefile))
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
