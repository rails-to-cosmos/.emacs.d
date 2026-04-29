;;; displays-mode.el --- Visual layout editor for X displays  -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Dmitry Akatov
;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; URL: https://github.com/rails-to-cosmos/displays-mode
;; Package-Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (transient "0.4"))
;; Keywords: hardware, multimedia

;;; Commentary:
;;
;; Buffer-based interactive editor for X display configuration.
;; Shells out to xrandr/autorandr; no Rust needed.
;;
;; Usage:
;;   M-x displays-show       open the layout buffer
;;   C-c d                   transient menu (primary, rotate, profiles, ...)
;;
;; Inside the *Displays* buffer:
;;   n / p     next / previous display
;;   h j k l   move selected display left/down/up/right relative to neighbor
;;   R         cycle resolution
;;   F         cycle refresh rate
;;   r         cycle rotation (normal / left / right / inverted)
;;   P         mark as primary
;;   d         disable display
;;   e         enable display
;;   C-c C-c   apply with xrandr
;;   C-c C-s   save current as autorandr profile
;;   C-c C-l   load autorandr profile
;;   g         refresh from xrandr

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup displays nil
  "Visual layout editor for X displays."
  :group 'hardware
  :prefix "displays-")

(defcustom displays-xrandr-command "xrandr"
  "Path to the xrandr binary."
  :type 'string)

(defcustom displays-autorandr-command "autorandr"
  "Path to the autorandr binary."
  :type 'string)

(defcustom displays-buffer-name "*Displays*"
  "Name of the displays layout buffer."
  :type 'string)

(defcustom displays-on-connect-hook nil
  "Hook run when a new display is connected.
Each function is called with the output name."
  :type 'hook)

(defcustom displays-on-disconnect-hook nil
  "Hook run when a display is disconnected."
  :type 'hook)

;;; Faces

(defface displays-box
  '((t :inherit default :box (:line-width 1)))
  "Face for the display rectangle.")

(defface displays-primary
  '((t :inherit success :weight bold))
  "Face for the primary marker.")

(defface displays-disabled
  '((t :inherit shadow :strike-through t))
  "Face for disabled displays.")

(defface displays-name
  '((t :inherit font-lock-keyword-face))
  "Face for the output name.")

(defface displays-resolution
  '((t :inherit font-lock-string-face))
  "Face for the resolution string.")

;;; Data model

(cl-defstruct displays-output
  name              ; "eDP-1"
  connected         ; t / nil
  enabled           ; t / nil  (disabled means --off)
  primary           ; t / nil
  geometry          ; (X Y W H)
  rotation          ; 'normal / 'left / 'right / 'inverted
  current-mode      ; "2160x1350"
  current-rate      ; 60.0 (Hz)
  modes)            ; alist: ("2160x1350" . (60.0 59.93)) - rates per mode

(defvar displays--state nil
  "List of `displays-output' structs reflecting current xrandr query.")

(defvar displays--dirty nil
  "Non-nil when in-buffer state diverges from applied state.")

;;; xrandr parsing

(defun displays--call (program &rest args)
  "Run PROGRAM with ARGS, return stdout as string. Errors signal."
  (with-temp-buffer
    (let ((exit (apply #'call-process program nil t nil args)))
      (unless (zerop exit)
        (error "%s %s failed (%d): %s"
               program (string-join args " ") exit (buffer-string)))
      (buffer-string))))

(defun displays--parse-xrandr (text)
  "Parse `xrandr --query` TEXT into list of `displays-output'."
  (let ((outputs nil)
        (current nil))
    (dolist (line (split-string text "\n"))
      (cond
       ;; Output header: "eDP-1 connected primary 2160x1350+0+0 ..."
       ((string-match
         (concat "^\\([A-Za-z0-9-]+\\) +"
                 "\\(connected\\|disconnected\\)"
                 "\\(?: +primary\\)?"
                 "\\(?: +\\([0-9]+\\)x\\([0-9]+\\)\\+\\([0-9]+\\)\\+\\([0-9]+\\)\\)?"
                 "\\(?: +\\(normal\\|left\\|right\\|inverted\\)\\)?")
         line)
        (when current (push current outputs))
        (let* ((primary-p (string-match-p " primary " line))
               (rotation (and (match-string 7 line)
                              (intern (match-string 7 line))))
               (w (and (match-string 3 line) (string-to-number (match-string 3 line))))
               (h (and (match-string 4 line) (string-to-number (match-string 4 line))))
               (x (and (match-string 5 line) (string-to-number (match-string 5 line))))
               (y (and (match-string 6 line) (string-to-number (match-string 6 line)))))
          (setq current
                (make-displays-output
                 :name (match-string 1 line)
                 :connected (string= (match-string 2 line) "connected")
                 :enabled (and w h)
                 :primary primary-p
                 :geometry (and w h (list x y w h))
                 :rotation (or rotation 'normal)
                 :current-mode (and w h (format "%dx%d" w h))
                 :modes nil))))
       ;; Mode line: "   2160x1350    60.00*+  59.93"
       ((and current
             (string-match "^ +\\([0-9]+x[0-9]+\\) +\\(.*\\)$" line))
        (let ((mode (match-string 1 line))
              (rates-str (match-string 2 line))
              (rates nil)
              (current-rate nil))
          (dolist (token (split-string rates-str))
            (when (string-match "^\\([0-9.]+\\)\\([*+]*\\)$" token)
              (let ((rate (string-to-number (match-string 1 token)))
                    (flags (match-string 2 token)))
                (push rate rates)
                (when (string-match-p "\\*" flags)
                  (setq current-rate rate)
                  (setf (displays-output-current-rate current) rate)))))
          (push (cons mode (nreverse rates))
                (displays-output-modes current))))))
    (when current (push current outputs))
    (nreverse outputs)))

(defun displays-refresh ()
  "Re-query xrandr and update internal state."
  (interactive)
  (setq displays--state (displays--parse-xrandr
                         (displays--call displays-xrandr-command "--query")))
  (setq displays--dirty nil)
  (when (get-buffer displays-buffer-name)
    (displays--render)))

;;; Buffer rendering

(defvar displays-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m "n" #'displays-next)
    (define-key m "p" #'displays-prev)
    (define-key m "g" #'displays-refresh)
    (define-key m "h" #'displays-move-left)
    (define-key m "j" #'displays-move-down)
    (define-key m "k" #'displays-move-up)
    (define-key m "l" #'displays-move-right)
    (define-key m "R" #'displays-cycle-resolution)
    (define-key m "F" #'displays-cycle-rate)
    (define-key m "r" #'displays-cycle-rotation)
    (define-key m "P" #'displays-toggle-primary)
    (define-key m "d" #'displays-disable)
    (define-key m "e" #'displays-enable)
    (define-key m (kbd "C-c C-c") #'displays-apply)
    (define-key m (kbd "C-c C-s") #'displays-save-profile)
    (define-key m (kbd "C-c C-l") #'displays-load-profile)
    (define-key m "?" #'displays-help)
    m)
  "Keymap for `displays-mode'.")

(define-derived-mode displays-mode special-mode "Displays"
  "Major mode for editing X display layout."
  (setq-local truncate-lines t
              cursor-type nil)
  (read-only-mode 1))

(defvar-local displays--selected nil
  "Name of currently selected output in the buffer.")

(defun displays--render ()
  "Render `displays--state' into the layout buffer."
  (with-current-buffer (get-buffer-create displays-buffer-name)
    (displays-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (propertize "Displays" 'face 'bold)
              (if displays--dirty
                  (propertize "  [unapplied]" 'face 'warning)
                "")
              "\n\n")
      (dolist (out displays--state)
        (displays--render-output out)
        (insert "\n"))
      (insert "\n"
              (propertize
               (concat "n/p select  •  hjkl move  •  R res  F rate  r rotate"
                       "  •  P primary  d disable  e enable\n"
                       "C-c C-c apply  •  C-c C-s save  •  C-c C-l load  •  g refresh  •  ? help\n")
               'face 'shadow)))
    (goto-char (point-min))))

(defun displays--render-output (out)
  "Render a single OUTPUT struct as a labeled row."
  (let* ((selected (string= (displays-output-name out) displays--selected))
         (sigil    (if selected "▶ " "  "))
         (name     (displays-output-name out))
         (conn     (displays-output-connected out))
         (enabled  (displays-output-enabled out))
         (primary  (displays-output-primary out))
         (mode     (displays-output-current-mode out))
         (rate     (displays-output-current-rate out))
         (rot      (displays-output-rotation out))
         (geom     (displays-output-geometry out)))
    (insert sigil)
    (insert (propertize name 'face
                        (cond ((not conn) 'displays-disabled)
                              ((not enabled) 'shadow)
                              (t 'displays-name))))
    (insert "  ")
    (cond
     ((not conn)
      (insert (propertize "(disconnected)" 'face 'shadow)))
     ((not enabled)
      (insert (propertize "(off)" 'face 'shadow)))
     (t
      (insert (propertize (or mode "?") 'face 'displays-resolution))
      (when rate
        (insert (format " @ %.0fHz" rate)))
      (when (and geom (not (eq rot 'normal)))
        (insert (format "  %s" rot)))
      (when geom
        (insert (format "  +%d+%d" (nth 0 geom) (nth 1 geom))))
      (when primary
        (insert "  ")
        (insert (propertize "★ primary" 'face 'displays-primary)))))
    (put-text-property (line-beginning-position) (line-end-position)
                       'displays-output name)))

;;; Selection

(defun displays--find (name)
  "Return the `displays-output' struct named NAME or nil."
  (cl-find name displays--state
           :key #'displays-output-name :test #'string=))

(defun displays--connected-names ()
  (mapcar #'displays-output-name
          (cl-remove-if-not #'displays-output-connected displays--state)))

(defun displays--ensure-selected ()
  (unless (and displays--selected (displays--find displays--selected))
    (setq displays--selected (car (displays--connected-names)))))

(defun displays-next ()
  "Select next connected display."
  (interactive)
  (let* ((names (displays--connected-names))
         (idx (cl-position displays--selected names :test #'string=)))
    (setq displays--selected
          (nth (mod (1+ (or idx -1)) (length names)) names))
    (displays--render)))

(defun displays-prev ()
  "Select previous connected display."
  (interactive)
  (let* ((names (displays--connected-names))
         (idx (cl-position displays--selected names :test #'string=)))
    (setq displays--selected
          (nth (mod (1- (or idx 1)) (length names)) names))
    (displays--render)))

;;; Mutations

(defun displays--current ()
  (displays--ensure-selected)
  (or (displays--find displays--selected)
      (user-error "No display selected")))

(defun displays--mark-dirty ()
  (setq displays--dirty t)
  (displays--render))

(defun displays-toggle-primary ()
  "Toggle primary flag on the selected display."
  (interactive)
  (let ((cur (displays--current)))
    (dolist (out displays--state)
      (setf (displays-output-primary out) nil))
    (setf (displays-output-primary cur) t)
    (displays--mark-dirty)))

(defun displays-disable ()
  "Disable the selected display (--off on apply)."
  (interactive)
  (setf (displays-output-enabled (displays--current)) nil)
  (displays--mark-dirty))

(defun displays-enable ()
  "Enable the selected display (--auto on apply)."
  (interactive)
  (setf (displays-output-enabled (displays--current)) t)
  (displays--mark-dirty))

(defun displays-cycle-rotation ()
  "Cycle rotation: normal -> left -> inverted -> right -> normal."
  (interactive)
  (let* ((cur (displays--current))
         (cycle '(normal left inverted right))
         (next (or (cadr (memq (displays-output-rotation cur) cycle))
                   (car cycle))))
    (setf (displays-output-rotation cur) next)
    (displays--mark-dirty)))

(defun displays-cycle-resolution ()
  "Cycle the selected display's resolution through available modes."
  (interactive)
  (let* ((cur (displays--current))
         (modes (mapcar #'car (displays-output-modes cur)))
         (idx (cl-position (displays-output-current-mode cur) modes :test #'string=))
         (next (nth (mod (1+ (or idx -1)) (length modes)) modes)))
    (setf (displays-output-current-mode cur) next)
    (displays--mark-dirty)))

(defun displays-cycle-rate ()
  "Cycle the refresh rate within the current resolution."
  (interactive)
  (let* ((cur (displays--current))
         (mode (displays-output-current-mode cur))
         (rates (cdr (assoc mode (displays-output-modes cur))))
         (idx (cl-position (displays-output-current-rate cur) rates))
         (next (nth (mod (1+ (or idx -1)) (length rates)) rates)))
    (setf (displays-output-current-rate cur) next)
    (displays--mark-dirty)))

(defun displays--move (dir)
  "Move selected display relative to neighbor in DIR ('left/'right/'up/'down)."
  (let* ((cur (displays--current))
         (others (cl-remove cur displays--state))
         (anchor (car (cl-remove-if-not #'displays-output-enabled others))))
    (unless anchor
      (user-error "No other enabled display to position relative to"))
    (let* ((g (displays-output-geometry anchor))
           (ax (nth 0 g)) (ay (nth 1 g))
           (aw (nth 2 g)) (ah (nth 3 g))
           (cw (nth 2 (displays-output-geometry cur)))
           (ch (nth 3 (displays-output-geometry cur)))
           (nx ax) (ny ay))
      (pcase dir
        ('right (setq nx (+ ax aw) ny ay))
        ('left  (setq nx (- ax cw) ny ay))
        ('down  (setq nx ax ny (+ ay ah)))
        ('up    (setq nx ax ny (- ay ch))))
      (setf (displays-output-geometry cur) (list nx ny cw ch)))
    (displays--mark-dirty)))

(defun displays-move-left ()  (interactive) (displays--move 'left))
(defun displays-move-right () (interactive) (displays--move 'right))
(defun displays-move-up ()    (interactive) (displays--move 'up))
(defun displays-move-down ()  (interactive) (displays--move 'down))

;;; Apply via xrandr

(defun displays--xrandr-args ()
  "Build xrandr command-line arguments from current state."
  (let (args)
    (dolist (out displays--state)
      (when (displays-output-connected out)
        (let ((name (displays-output-name out)))
          (push "--output" args)
          (push name args)
          (cond
           ((not (displays-output-enabled out))
            (push "--off" args))
           (t
            (push "--mode" args)
            (push (displays-output-current-mode out) args)
            (when-let ((r (displays-output-current-rate out)))
              (push "--rate" args)
              (push (number-to-string r) args))
            (when-let ((g (displays-output-geometry out)))
              (push "--pos" args)
              (push (format "%dx%d" (nth 0 g) (nth 1 g)) args))
            (push "--rotate" args)
            (push (symbol-name (displays-output-rotation out)) args)
            (when (displays-output-primary out)
              (push "--primary" args)))))))
    (nreverse args)))

(defun displays-apply ()
  "Apply current layout via xrandr."
  (interactive)
  (let ((args (displays--xrandr-args)))
    (apply #'displays--call displays-xrandr-command args)
    (message "Applied: xrandr %s" (string-join args " "))
    (displays-refresh)))

;;; Profiles (autorandr)

(defun displays-list-profiles ()
  "List autorandr profiles."
  (interactive)
  (message "%s" (string-trim
                 (displays--call displays-autorandr-command "--list"))))

(defun displays-save-profile (name)
  "Save current layout as autorandr profile NAME."
  (interactive "sProfile name: ")
  (displays--call displays-autorandr-command "--save" name "--force")
  (message "Saved profile: %s" name))

(defun displays-load-profile (name)
  "Load autorandr profile NAME."
  (interactive
   (list (completing-read
          "Profile: "
          (split-string
           (string-trim
            (condition-case _ (displays--call displays-autorandr-command "--list")
              (error "")))
           "\n" t))))
  (displays--call displays-autorandr-command "--load" name)
  (displays-refresh)
  (message "Loaded profile: %s" name))

;;; Hot-plug watcher

(defvar displays--watcher-process nil)
(defvar displays--last-connected nil)

(defun displays--watch-tick ()
  "Poll xrandr; fire hooks on connect/disconnect."
  (let* ((state (displays--parse-xrandr
                 (displays--call displays-xrandr-command "--query")))
         (connected (cl-remove-if-not #'displays-output-connected state))
         (names (mapcar #'displays-output-name connected)))
    (dolist (n names)
      (unless (member n displays--last-connected)
        (run-hook-with-args 'displays-on-connect-hook n)))
    (dolist (n displays--last-connected)
      (unless (member n names)
        (run-hook-with-args 'displays-on-disconnect-hook n)))
    (setq displays--last-connected names
          displays--state state)
    (when (get-buffer displays-buffer-name)
      (displays--render))))

(define-minor-mode displays-watch-mode
  "Poll for display hot-plug changes.
Fires `displays-on-connect-hook' / `displays-on-disconnect-hook'."
  :global t
  :lighter " Displays"
  (if displays-watch-mode
      (progn
        (setq displays--last-connected (displays--connected-names))
        (setq displays--watcher-process
              (run-with-timer 5 5 #'displays--watch-tick)))
    (when (timerp displays--watcher-process)
      (cancel-timer displays--watcher-process))
    (setq displays--watcher-process nil)))

;;; Entry points

;;;###autoload
(defun displays-show ()
  "Open the displays layout buffer."
  (interactive)
  (displays-refresh)
  (pop-to-buffer displays-buffer-name))

(defun displays-help ()
  (interactive)
  (describe-mode))

(provide 'displays-mode)
;;; displays-mode.el ends here
