;;; pyrun.el --- capture Python and run it on a remote host -*- lexical-binding: t; -*-

;; M-x pyrun-remote-run : capture Python (org-capture style), dotenv-ify + clean
;; it, ship it to a remote host over ssh, run `uv run python', and stream the
;; combined stdout/stderr back in realtime.

(require 'subr-x)

(defvar pyrun-host "10.17.2.107"
  "Remote host, as understood by ssh / ~/.ssh/config.")

(defvar pyrun-dir "~/stuff/tardis-alphas-on-scale"
  "Remote directory the script is written to and run from.")

(defvar pyrun-ssh-options '("-o" "LogLevel=ERROR")
  "Arguments spliced into the ssh invocation.
`LogLevel=ERROR' silences client info/warnings (e.g. the post-quantum
key-exchange notice) while still surfacing real connection errors.")

(defvar pyrun--spinner ["⠋" "⠙" "⠹" "⠸" "⠼" "⠴" "⠦" "⠧" "⠇" "⠏"]
  "Spinner frames for the realtime progress header.")

(defun pyrun--sanitize (code)
  "Normalize line endings + tabs and strip non-printable chars from CODE."
  (let ((s code))
    (setq s (replace-regexp-in-string "\r\n?" "\n" s))                ; CRLF/CR -> LF
    (setq s (replace-regexp-in-string "\t" "    " s))                 ; tab -> 4 spaces
    (setq s (replace-regexp-in-string                                 ; unicode spaces -> space
             "[   -   　]" " " s))
    (setq s (replace-regexp-in-string "[​-‍﻿]" "" s))  ; zero-width / BOM -> drop
    (setq s (replace-regexp-in-string "[^[:print:]\n]" "" s))         ; other control chars -> drop
    s))

(defun pyrun--ensure-dotenv (code)
  "Ensure CODE imports python-dotenv and calls `load_dotenv'."
  (cond
   ((string-match-p "load_dotenv(" code) code)
   ((string-match "\\(?:^[ \t]*from __future__ import[^\n]*\n\\)+" code)
    (let ((end (match-end 0)))
      (concat (substring code 0 end)
              "from dotenv import load_dotenv\nload_dotenv(override=True)\n"
              (substring code end))))
   (t (concat "from dotenv import load_dotenv\nload_dotenv(override=True)\n\n" code))))

(defun pyrun--clean-output (string)
  "Strip carriage returns and the ssh post-quantum warning from STRING."
  (setq string (replace-regexp-in-string "\r" "" string))
  (replace-regexp-in-string
   (concat "^\\*\\* \\(?:WARNING: connection is not using a post-quantum"
           "\\|This session may be vulnerable"
           "\\|The server may need to be upgraded\\)[^\n]*\n?")
   "" string))

(defun pyrun--elapsed (proc)
  "Seconds since PROC started."
  (float-time (time-subtract (current-time) (process-get proc 'start))))

(defun pyrun--filter (proc string)
  "Insert cleaned STRING from PROC into its buffer; keep windows scrolled."
  (let ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (setq string (pyrun--clean-output string))
      (with-current-buffer buf
        (save-excursion
          (goto-char (point-max))
          (insert string))
        (dolist (win (get-buffer-window-list buf nil t))
          (set-window-point win (point-max)))))))

(defun pyrun--tick (proc)
  "Refresh PROC's progress header (spinner + elapsed + bytes so far)."
  (let ((buf (process-buffer proc)))
    (when (and (buffer-live-p buf) (process-live-p proc))
      (let ((i (% (1+ (or (process-get proc 'spin) 0)) (length pyrun--spinner))))
        (process-put proc 'spin i)
        (with-current-buffer buf
          (setq header-line-format
                (format " %s  running on %s  —  %.1fs  ·  %d bytes"
                        (aref pyrun--spinner i) pyrun-host
                        (pyrun--elapsed proc) (1- (point-max))))
          (force-mode-line-update))))))

(defun pyrun--sentinel (proc event)
  "On PROC finish: stop the timer, write a footer, update the header."
  (when (memq (process-status proc) '(exit signal))
    (let ((tm (process-get proc 'timer)))
      (when (timerp tm) (cancel-timer tm)))
    (let* ((buf (process-buffer proc))
           (secs (pyrun--elapsed proc))
           (rc (process-exit-status proc))
           (ok (and (eq (process-status proc) 'exit) (zerop rc))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (goto-char (point-max))
          (insert (format "\n--- %s in %.1fs (exit %s; %s)\n"
                          (if ok "done" "FAILED") secs rc (string-trim event)))
          (setq header-line-format
                (format " %s  %s on %s  —  %.1fs (exit %s)"
                        (if ok "✓" "✗") (if ok "done" "FAILED") pyrun-host secs rc)))
        (display-buffer buf))
      (message "pyrun: %s in %.1fs (exit %s)" (if ok "done" "FAILED") secs rc))))

(defun pyrun--start (command code header)
  "Run COMMAND (argv list), feed CODE to its stdin, stream output live.
HEADER labels the output buffer.  Returns the process."
  (let ((outbuf (get-buffer-create "*pyrun-output*")))
    (with-current-buffer outbuf
      (erase-buffer)
      (insert header)
      (setq header-line-format (format " ⟳  starting on %s …" pyrun-host)))
    (display-buffer outbuf)
    (let ((proc (make-process
                 :name "pyrun" :buffer outbuf :connection-type 'pipe :noquery t
                 :command command
                 :filter #'pyrun--filter
                 :sentinel #'pyrun--sentinel)))
      (process-put proc 'start (current-time))
      (process-put proc 'timer (run-with-timer 0 0.2 #'pyrun--tick proc))
      (when code
        (process-send-string proc code)
        (process-send-eof proc))
      proc)))

(defun pyrun--ship-and-run (code)
  "Write CODE to a unique remote file under `pyrun-dir' and run it with uv."
  (let* ((fname (format "pyrun_%s_%04x.py"
                        (format-time-string "%Y%m%d-%H%M%S") (random 65536)))
         ;; -u + PYTHONUNBUFFERED so prints stream back live, not block-buffered.
         (inner (format (concat "mkdir -p %s && cat > %s/%s && cd %s && "
                                "PYTHONUNBUFFERED=1 uv run python -u %s 2>&1")
                        pyrun-dir pyrun-dir fname pyrun-dir fname))
         ;; bash -lc => login shell so `uv' is on PATH over a non-interactive ssh.
         (remote-cmd (format "bash -lc %s" (shell-quote-argument inner)))
         (header (format "# host=%s  file=%s/%s  (%s)\n\n"
                         pyrun-host pyrun-dir fname (format-time-string "%F %T"))))
    (pyrun--start (append (list "ssh") pyrun-ssh-options (list pyrun-host remote-cmd))
                  code header)
    (message "pyrun: shipped %s, running on %s …" fname pyrun-host)))

(defvar pyrun-capture-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") #'pyrun-finish)
    (define-key m (kbd "C-c C-k") #'pyrun-abort)
    m)
  "Keymap for `pyrun-capture-mode'.")

(define-minor-mode pyrun-capture-mode
  "Minor mode for the pyrun capture buffer."
  :lighter " PyRun" :keymap pyrun-capture-map)

(defun pyrun-finish ()
  "Sanitize + dotenv-ify the capture buffer and run it on the remote host."
  (interactive)
  (let* ((raw (buffer-substring-no-properties (point-min) (point-max)))
         (code (pyrun--ensure-dotenv (pyrun--sanitize raw))))
    (when (string-empty-p (string-trim code))
      (user-error "pyrun: buffer is empty"))
    (let ((win (selected-window)))
      (pyrun--ship-and-run code)
      (quit-window t win))))

(defun pyrun-abort ()
  "Discard the pyrun capture buffer without running anything."
  (interactive)
  (quit-window t (selected-window)))

;;;###autoload
(defun pyrun-remote-run ()
  "Pop a capture buffer; paste Python, then C-c C-c to run it on `pyrun-host'."
  (interactive)
  (let ((buf (get-buffer-create "*pyrun-capture*")))
    (with-current-buffer buf
      (erase-buffer)
      (when (fboundp 'python-mode) (python-mode))
      (when (fboundp 'electric-indent-local-mode) (electric-indent-local-mode -1))
      (pyrun-capture-mode 1)
      (setq-local header-line-format
                  "Paste Python  —  C-c C-c: run on remote  ·  C-c C-k: cancel"))
    (pop-to-buffer buf)))

(provide 'pyrun)
;;; pyrun.el ends here
