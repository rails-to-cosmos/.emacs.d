;;; xrandr.el --- Interactive xrandr display management -*- lexical-binding: t; -*-

;;; Commentary:
;; Interactive Emacs interface for managing displays via xrandr.
;; Entry point: `M-x xrandr'

;;; Code:

(defgroup xrandr nil
  "Interactive xrandr display management."
  :group 'hardware)

(defcustom xrandr-profiles-directory
  (expand-file-name "xrandr-profiles" user-emacs-directory)
  "Directory for saved xrandr profiles."
  :type 'directory
  :group 'xrandr)

;;; Parsing

(defun xrandr--run (&rest args)
  "Run xrandr with ARGS and return output string."
  (with-output-to-string
    (with-current-buffer standard-output
      (apply #'call-process "xrandr" nil t nil args))))

(defun xrandr--parse-outputs ()
  "Parse xrandr output into a list of display plists.
Each plist has :name :connected :primary :modes :current-mode :current-rate."
  (let ((lines (split-string (xrandr--run) "\n" t))
        outputs current)
    (dolist (line lines)
      (cond
       ;; Output line: "eDP-1 connected primary 2560x1600+0+0 ..."
       ;; or: "DisplayPort-1 disconnected ..."
       ((string-match
         (rx bol
             (group (+ (not space)))          ; name
             " "
             (group (or "connected" "disconnected"))
             (? " " (group "primary"))
             (? " " (group (+ digit) "x" (+ digit))
                "+" (+ digit) "+" (+ digit)))
         line)
        (when current (push current outputs))
        (setq current
              (list :name (match-string 1 line)
                    :connected (equal (match-string 2 line) "connected")
                    :primary (and (match-string 3 line) t)
                    :current-res (match-string 4 line)
                    :modes nil)))
       ;; Mode line: "   2560x1600    165.00*+  60.00 +"
       ((and current
             (string-match
              (rx bol (+ space)
                  (group (+ digit) "x" (+ digit))
                  (+ space)
                  (group (+? anything))
                  eol)
              line))
        (let* ((res (match-string 1 line))
               (rates-str (match-string 2 line))
               (rates nil)
               (active-rate nil)
               (preferred-rate nil))
          (with-temp-buffer
            (insert rates-str)
            (goto-char (point-min))
            (while (re-search-forward
                    (rx (group (+ (or digit ".")))
                        (group (* (any "*+"))))
                    nil t)
              (let ((rate (match-string 1))
                    (flags (match-string 2)))
                (push rate rates)
                (when (string-match-p "\\*" flags)
                  (setq active-rate rate))
                (when (string-match-p "\\+" flags)
                  (setq preferred-rate rate)))))
          (plist-put current :modes
                     (append (plist-get current :modes)
                             (list (list :res res
                                         :rates (nreverse rates)
                                         :active active-rate
                                         :preferred preferred-rate))))))))
    (when current (push current outputs))
    (nreverse outputs)))

(defun xrandr--connected-outputs ()
  "Return only connected outputs."
  (seq-filter (lambda (o) (plist-get o :connected))
              (xrandr--parse-outputs)))

(defun xrandr--format-output (output)
  "Format OUTPUT plist as a display string for the buffer."
  (let ((name (plist-get output :name))
        (primary (plist-get output :primary))
        (res (plist-get output :current-res))
        (modes (plist-get output :modes)))
    (concat
     (propertize name 'face 'font-lock-function-name-face)
     (if primary
         (propertize " [primary]" 'face 'font-lock-keyword-face)
       "")
     (if res
         (format " %s" (propertize res 'face 'font-lock-constant-face))
       (propertize " (off)" 'face 'font-lock-comment-face))
     (when-let ((active-mode (seq-find (lambda (m) (plist-get m :active)) modes)))
       (format " @ %sHz" (plist-get active-mode :active))))))

;;; Buffer / UI

(defvar xrandr-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g" #'xrandr-refresh)
    (define-key map "e" #'xrandr-extend)
    (define-key map "m" #'xrandr-mirror)
    (define-key map "o" #'xrandr-only)
    (define-key map "O" #'xrandr-off)
    (define-key map "p" #'xrandr-set-primary)
    (define-key map "r" #'xrandr-set-resolution)
    (define-key map "R" #'xrandr-set-rate)
    (define-key map "s" #'xrandr-set-scale)
    (define-key map "l" #'xrandr-layout)
    (define-key map "a" #'xrandr-auto)
    (define-key map "x" #'xrandr-rotate)
    (define-key map "S" #'xrandr-save-profile)
    (define-key map "L" #'xrandr-load-profile)
    (define-key map "q" #'quit-window)
    map)
  "Keymap for `xrandr-mode'.")

(define-derived-mode xrandr-mode special-mode "Xrandr"
  "Major mode for interactive xrandr display management.

\\{xrandr-mode-map}")

(defun xrandr--output-at-point ()
  "Return the output name on the current line."
  (get-text-property (line-beginning-position) 'xrandr-output))

(defun xrandr--draw ()
  "Draw the xrandr buffer contents."
  (let ((inhibit-read-only t)
        (outputs (xrandr--parse-outputs))
        (connected nil)
        (disconnected nil))
    (erase-buffer)
    (dolist (o outputs)
      (if (plist-get o :connected)
          (push o connected)
        (push o disconnected)))
    (setq connected (nreverse connected))
    (insert (propertize "Displays" 'face 'font-lock-type-face) "\n")
    (insert (make-string 40 ?-) "\n")
    (dolist (o connected)
      (let ((start (point)))
        (insert "  " (xrandr--format-output o) "\n")
        (put-text-property start (point) 'xrandr-output (plist-get o :name))))
    (when disconnected
      (insert "\n" (propertize "Disconnected" 'face 'font-lock-comment-face) "\n")
      (dolist (o (nreverse disconnected))
        (insert "  " (propertize (plist-get o :name) 'face 'font-lock-comment-face) "\n")))
    (insert "\n"
            (propertize "Keys:" 'face 'font-lock-type-face) "\n"
            "  e  extend    m  mirror    o  only    O  off\n"
            "  p  primary   r  resolution   R  rate\n"
            "  s  scale     l  layout (position)\n"
            "  x  rotate    a  auto         q  quit\n"
            "  S  save      L  load         g  refresh\n")))

;;;###autoload
(defun xrandr ()
  "Open interactive xrandr display manager."
  (interactive)
  (let ((buf (get-buffer-create "*xrandr*")))
    (with-current-buffer buf
      (xrandr-mode)
      (xrandr--draw)
      (goto-char (point-min))
      (forward-line 2))
    (pop-to-buffer buf)))

(defun xrandr-refresh ()
  "Refresh the display list."
  (interactive)
  (xrandr--draw)
  (goto-char (point-min))
  (forward-line 2))

;;; Commands

(defun xrandr--select-output (prompt &optional only-connected)
  "Prompt for an output using PROMPT.
If ONLY-CONNECTED, show only connected outputs.
If point is on an output line, use it as default."
  (let* ((outputs (if only-connected
                      (xrandr--connected-outputs)
                    (xrandr--parse-outputs)))
         (names (mapcar (lambda (o) (plist-get o :name)) outputs))
         (default (xrandr--output-at-point)))
    (completing-read prompt names nil t nil nil default)))

(defun xrandr--select-mode (output-name)
  "Prompt for a resolution for OUTPUT-NAME."
  (let* ((outputs (xrandr--parse-outputs))
         (output (seq-find (lambda (o) (equal (plist-get o :name) output-name)) outputs))
         (modes (plist-get output :modes))
         (resolutions (mapcar (lambda (m) (plist-get m :res)) modes)))
    (completing-read (format "Resolution for %s: " output-name)
                     resolutions nil t)))

(defun xrandr--select-rate (output-name resolution)
  "Prompt for a refresh rate for OUTPUT-NAME at RESOLUTION."
  (let* ((outputs (xrandr--parse-outputs))
         (output (seq-find (lambda (o) (equal (plist-get o :name) output-name)) outputs))
         (mode (seq-find (lambda (m) (equal (plist-get m :res) resolution))
                         (plist-get output :modes)))
         (rates (plist-get mode :rates)))
    (completing-read (format "Rate for %s %s: " output-name resolution)
                     rates nil t)))

(defun xrandr--apply (&rest args)
  "Run xrandr with ARGS and refresh the buffer."
  (let ((cmd (mapconcat #'identity (cons "xrandr" args) " ")))
    (message "Running: %s" cmd)
    (apply #'call-process "xrandr" nil nil nil args)
    (when (eq major-mode 'xrandr-mode)
      (xrandr-refresh))))

(defun xrandr-extend ()
  "Extend display: place a selected output next to another."
  (interactive)
  (let* ((outputs (xrandr--connected-outputs))
         (names (mapcar (lambda (o) (plist-get o :name)) outputs))
         (source (completing-read "Extend which display? " names nil t
                                  nil nil (xrandr--output-at-point)))
         (target (completing-read "Relative to: " (remove source names) nil t))
         (position (completing-read "Position: "
                                    '("right-of" "left-of" "above" "below")
                                    nil t nil nil "right-of"))
         (mode (xrandr--select-mode source)))
    (xrandr--apply "--output" source "--auto" "--mode" mode
                   (concat "--" position) target)))

(defun xrandr-mirror ()
  "Mirror a display onto another."
  (interactive)
  (let* ((outputs (xrandr--connected-outputs))
         (names (mapcar (lambda (o) (plist-get o :name)) outputs))
         (source (completing-read "Mirror which display? " names nil t
                                  nil nil (xrandr--output-at-point)))
         (target (completing-read "Mirror onto: " (remove source names) nil t)))
    (xrandr--apply "--output" source "--auto" "--same-as" target)))

(defun xrandr-only ()
  "Enable only the selected display, turning off all others."
  (interactive)
  (let* ((outputs (xrandr--connected-outputs))
         (names (mapcar (lambda (o) (plist-get o :name)) outputs))
         (keep (completing-read "Keep only: " names nil t
                                nil nil (xrandr--output-at-point)))
         (args (list "--output" keep "--auto" "--primary")))
    (dolist (name names)
      (unless (equal name keep)
        (setq args (append args (list "--output" name "--off")))))
    (apply #'xrandr--apply args)))

(defun xrandr-off ()
  "Turn off the selected display."
  (interactive)
  (let ((output (xrandr--select-output "Turn off: " t)))
    (xrandr--apply "--output" output "--off")))

(defun xrandr-set-primary ()
  "Set the selected display as primary."
  (interactive)
  (let ((output (xrandr--select-output "Set primary: " t)))
    (xrandr--apply "--output" output "--primary")))

(defun xrandr-set-resolution ()
  "Set resolution for a display."
  (interactive)
  (let* ((output (xrandr--select-output "Set resolution for: " t))
         (mode (xrandr--select-mode output)))
    (xrandr--apply "--output" output "--mode" mode)))

(defun xrandr-set-rate ()
  "Set refresh rate for a display."
  (interactive)
  (let* ((output (xrandr--select-output "Set rate for: " t))
         (mode (xrandr--select-mode output))
         (rate (xrandr--select-rate output mode)))
    (xrandr--apply "--output" output "--mode" mode "--rate" rate)))

(defun xrandr-set-scale ()
  "Set scaling for a display."
  (interactive)
  (let* ((output (xrandr--select-output "Scale: " t))
         (scale (read-string "Scale (e.g. 1.5x1.5, 0.5x0.5): " "1x1")))
    (xrandr--apply "--output" output "--scale" scale)))

(defun xrandr-layout ()
  "Position a display relative to another."
  (interactive)
  (let* ((outputs (xrandr--connected-outputs))
         (names (mapcar (lambda (o) (plist-get o :name)) outputs))
         (source (completing-read "Position which display? " names nil t
                                  nil nil (xrandr--output-at-point)))
         (target (completing-read "Relative to: " (remove source names) nil t))
         (position (completing-read "Position: "
                                    '("right-of" "left-of" "above" "below")
                                    nil t nil nil "right-of")))
    (xrandr--apply "--output" source (concat "--" position) target)))

(defun xrandr-auto ()
  "Auto-configure all connected displays.
Enables each connected output at its preferred mode and rate,
extending right from the previous one. The first display is set as primary."
  (interactive)
  (let* ((outputs (xrandr--connected-outputs))
         (all (xrandr--parse-outputs))
         (disconnected (seq-filter (lambda (o) (not (plist-get o :connected))) all))
         (args nil)
         (prev nil))
    ;; Turn off disconnected outputs that might have been active
    (dolist (o disconnected)
      (setq args (append args (list "--output" (plist-get o :name) "--off"))))
    ;; Enable connected outputs with preferred modes
    (dolist (o outputs)
      (let* ((name (plist-get o :name))
             (modes (plist-get o :modes))
             (best (car modes))
             (res (and best (plist-get best :res)))
             (rate (and best (or (plist-get best :preferred)
                                 (car (plist-get best :rates))))))
        (setq args (append args (list "--output" name "--auto")))
        (when res
          (setq args (append args (list "--mode" res))))
        (when rate
          (setq args (append args (list "--rate" rate))))
        (if prev
            (setq args (append args (list "--right-of" prev)))
          (setq args (append args (list "--primary"))))
        (setq prev name)))
    (apply #'xrandr--apply args)))

(defun xrandr--current-setup-args ()
  "Build xrandr args that reproduce the current display setup.
Parses `xrandr --verbose' to capture position, mode, rate, rotation, and primary."
  (let ((lines (split-string (xrandr--run "--verbose") "\n"))
        (args nil)
        (current-output nil)
        (active-p nil)
        (pos nil)
        (mode nil)
        (rate nil)
        (rotation nil)
        (primary nil))
    (dolist (line lines)
      (cond
       ;; Output header
       ((string-match
         (rx bol (group (+ (not space))) " "
             (group (or "connected" "disconnected"))
             (? " " (group "primary"))
             (? " " (group (+ digit) "x" (+ digit)
                           "+" (group (+ digit)) "+" (group (+ digit)))))
         line)
        ;; Flush previous output
        (when current-output
          (if active-p
              (setq args (append args
                                 (list "--output" current-output
                                       "--mode" mode "--rate" rate
                                       "--pos" pos "--rotate" rotation)
                                 (when primary (list "--primary"))))
            (setq args (append args (list "--output" current-output "--off")))))
        (setq current-output (match-string 1 line)
              active-p (match-string 4 line)
              primary (and (match-string 3 line) t)
              pos (when active-p
                    (concat (match-string 5 line) "x" (match-string 6 line)))
              mode nil rate nil rotation "normal"))
       ;; Rotation line
       ((and current-output
             (string-match (rx "Rotation:" (+ space) (group (+ alpha))) line))
        (setq rotation (downcase (match-string 1 line))))
       ;; Active mode line (has * marker)
       ((and current-output active-p (not mode)
             (string-match
              (rx (+ space) (group (+ digit) "x" (+ digit))
                  (+ space) (group (+ (or digit "."))) "MHz")
              line))
        ;; Might be the mode header, look for active rate on next lines
        nil)
       ;; Mode with active marker: "  h: ... clock  165.00MHz"
       ;; Then resolution line: "  2560x1600 (0x48) 165.000MHz *current +preferred"
       ((and current-output active-p (not mode)
             (string-match
              (rx (+ space) (group (+ digit) "x" (+ digit))
                  (+ space) "(" (+ any) ")"
                  (+ space) (group (+ (or digit "."))) "MHz"
                  (+ any) "*current")
              line))
        (setq mode (match-string 1 line)))
       ;; Rate line with active flag: "        v: ... 165.0Hz *current"
       ((and current-output active-p mode (not rate)
             (string-match
              (rx (group (+ (or digit "."))) "Hz"
                  (+ any) "*current")
              line))
        (setq rate (match-string 1 line)))))
    ;; Flush last output
    (when current-output
      (if active-p
          (setq args (append args
                             (list "--output" current-output
                                   "--mode" mode "--rate" rate
                                   "--pos" pos "--rotate" rotation)
                             (when primary (list "--primary"))))
        (setq args (append args (list "--output" current-output "--off")))))
    (seq-filter #'identity args)))

(defun xrandr--profile-files ()
  "Return alist of (name . path) for saved profiles."
  (when (file-directory-p xrandr-profiles-directory)
    (let ((files (directory-files xrandr-profiles-directory t "\\.sh$")))
      (mapcar (lambda (f)
                (cons (file-name-sans-extension (file-name-nondirectory f)) f))
              files))))

(defun xrandr-save-profile ()
  "Save the current display setup to a named profile."
  (interactive)
  (let* ((name (read-string "Profile name: "))
         (args (xrandr--current-setup-args))
         (cmd (mapconcat #'shell-quote-argument (cons "xrandr" args) " "))
         (dir xrandr-profiles-directory)
         (file (expand-file-name (concat name ".sh") dir)))
    (make-directory dir t)
    (with-temp-file file
      (insert "#!/bin/sh\n" cmd "\n"))
    (set-file-modes file #o755)
    (message "Saved profile: %s" file)))

(defun xrandr-load-profile ()
  "Load and apply a saved display profile."
  (interactive)
  (let* ((profiles (xrandr--profile-files))
         (names (mapcar #'car profiles)))
    (unless names
      (user-error "No saved profiles in %s" xrandr-profiles-directory))
    (let* ((name (completing-read "Load profile: " names nil t))
           (file (cdr (assoc name profiles))))
      (message "Applying profile: %s" name)
      (call-process-shell-command file)
      (when (eq major-mode 'xrandr-mode)
        (xrandr-refresh)))))

(defun xrandr-rotate ()
  "Rotate a display."
  (interactive)
  (let* ((output (xrandr--select-output "Rotate: " t))
         (rotation (completing-read "Rotation: "
                                    '("normal" "left" "right" "inverted")
                                    nil t)))
    (xrandr--apply "--output" output "--rotate" rotation)))

(provide 'xrandr)
;;; xrandr.el ends here
