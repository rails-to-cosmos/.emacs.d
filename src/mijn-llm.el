;;; mijn-llm.el --- Claude CLI integration for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'transient)
(require 'ansi-color)

(defvar vterm-shell)
(defvar vterm--process)
(defvar llm--prompt-queue)

(declare-function vterm "vterm")
(declare-function vterm-insert "vterm")
(declare-function vterm-send-return "vterm")
(declare-function vterm-other-window "vterm")
(declare-function face-remap-remove-relative "face-remap")

(use-package vterm
  :ensure t)

;;; Customization

(defgroup llm nil
  "Claude CLI integration for Emacs."
  :group 'tools
  :prefix "llm-")

;;; Project Root Detection

(defcustom llm-project-root-markers '(".git" ".claude" "CLAUDE.md")
  "Files/dirs that indicate a project root."
  :type '(repeat string)
  :group 'llm)

(defcustom llm-dangerously-skip-permissions nil
  "When non-nil, pass `--dangerously-skip-permissions' to claude.
Applies to the main `*claude:PROJECT*' vterm and to the inline
bubble (captured once at bubble creation as buffer-local state).

Normally toggled per-invocation via the `-d' switch in `llm-menu'
rather than set directly. Beware: with this flag Claude skips all
tool-use confirmation prompts."
  :type 'boolean
  :group 'llm)

(cl-defun llm--project-root (&optional (dir default-directory))
  "Find project root starting from DIR by looking for marker files."
  (or (cl-loop for marker in llm-project-root-markers
               for root = (locate-dominating-file dir marker)
               when root return (file-name-as-directory root))
      (file-name-as-directory dir)))

(defvar-local llm--prompt-project-root nil
  "Project root captured when the prompt buffer was opened.")

(defvar-local llm--prompt-context-prefix nil
  "Auto-generated file/region context header.
Prepended to the user's prompt at send-time but kept out of the
visible buffer so the composition area stays clean.")

(defun llm--current-root ()
  "Get the project root for the current context."
  (or llm--prompt-project-root
      (llm--project-root)))

;;; Persistence Location

(defcustom llm-persistence-strategy 'project
  "Where to store prompt history and annotations.

- `project': per-project, inside the repo at `.project/'.
  Committable across machines; `.gitignore' auto-appended to avoid
  dirtying `git status'.
- `user': per-user, per-machine, at `~/.cache/mijn-llm/<repo-id>/'.
  Never touches the repo; each checkout starts empty."
  :type '(choice (const :tag "Per-project (.project/ in repo)" project)
                 (const :tag "Per-user (~/.cache/mijn-llm/)"    user))
  :group 'llm)

(defcustom llm-user-cache-dir
  (expand-file-name "mijn-llm"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name ".cache"
                                          (or (getenv "HOME") "~"))))
  "Root directory for per-user persistence when `llm-persistence-strategy'
is `user'."
  :type 'directory
  :group 'llm)

(defun llm--project-cache-subdir (root)
  "Human-readable directory name for ROOT under `llm-user-cache-dir'.
Uses the abbreviated absolute path with `/' replaced by `!'."
  (let ((abbrev (abbreviate-file-name (directory-file-name (expand-file-name root)))))
    (replace-regexp-in-string "/" "!" abbrev)))

(defun llm--persistence-dir (root subpath)
  "Return the absolute path of SUBPATH under ROOT, per `llm-persistence-strategy'.
SUBPATH is relative (e.g. \"prompts\" or \"fixme.el\")."
  (pcase llm-persistence-strategy
    ('user
     (expand-file-name
      subpath
      (expand-file-name (llm--project-cache-subdir root)
                        llm-user-cache-dir)))
    (_
     (expand-file-name (concat ".project/" subpath) root))))

(defun llm--legacy-project-path (root subpath)
  "Return the `.project/SUBPATH' path under ROOT.
Used as a read-side fallback when `llm-persistence-strategy' is `user'
so existing in-repo annotations remain visible after switching."
  (expand-file-name (concat ".project/" subpath) root))

;;; Claude vterm buffer management

(defvar llm--buffers (make-hash-table :test 'eq)
  "Hash table mapping claude vterm buffers to their status.")

(defun llm--register-buffer (buf)
  "Register BUF as a claude buffer with initial status nil."
  (puthash buf nil llm--buffers))

(defun llm--unregister-buffer (buf)
  "Unregister BUF from the claude buffer registry."
  (remhash buf llm--buffers))

(defun llm--get-buffers ()
  "Return a list of all live claude buffers."
  (cl-remove-if-not #'buffer-live-p (hash-table-keys llm--buffers)))

;;; Status Detection Patterns and Logic

(defvar llm--permission-pattern
  (rx (or "Do you want to proceed"
          "❯ 1. Yes"
          "❯ 2. Yes"
          "[y/N]"
          "[Y/n]"
          (seq "(y" (0+ space) "/" (0+ space) "n)")
          (seq "(Y" (0+ space) "/" (0+ space) "n)")))
  "Regex matching Claude CLI permission prompts (→ blocked).
Anchored on distinctive UI markers rather than generic words like
\"allow\" or \"yes\" so it doesn't false-positive on prose.")

(defvar llm--busy-pattern
  "esc to interrupt"
  "Regex to detect when Claude is actively working (→ busy).")

(defvar llm--user-input-pattern
  "^[^[:space:]].*[%$>#λ]\\s*$"
  "Regex to detect shell prompts and user input areas (ignored for status).")

(defun llm--status-from-output (input current-status)
  "Determine new status based on vterm INPUT chunk and CURRENT-STATUS.
Rules (in order):
1. INPUT matches permission pattern → \\='blocked
2. INPUT matches busy pattern → \\='busy
3. INPUT is not a shell prompt and current isn't already busy → \\='busy
4. Otherwise → keep CURRENT-STATUS."
  (cond
   ((string-match-p llm--permission-pattern input) 'blocked)
   ((string-match-p llm--busy-pattern input) 'busy)
   ((not (string-match-p llm--user-input-pattern input))
    (unless (eq current-status 'busy) 'busy))
   (t current-status)))

(defconst llm--terminal-tail-bytes 4096
  "Bytes of trailing vterm content to scan for status detection.
Enough to cover any realistic permission prompt while keeping
regex time O(1) regardless of session length.")

(defun llm--last-terminal-line (buf)
  "Return the last non-empty line from the vterm terminal in BUF."
  (with-current-buffer buf
    (let* ((end (point-max))
           (beg (max (point-min) (- end llm--terminal-tail-bytes)))
           (content (buffer-substring-no-properties beg end)))
      (if (string-match "\\([^\n\r\t ][^\n]*\\)[\n\r\t ]*\\'" content)
          (match-string 1 content)
        ""))))

(defun llm--status-from-process (buf)
  "Determine new status for BUF based on process state and terminal content.
Rules:
1. If process is dead → \\='exited
2. If terminal still shows a permission prompt → \\='blocked
3. Otherwise → \\='idle"
  (let* ((proc vterm--process)
         (alive (and proc (process-live-p proc))))
    (cond ((not alive) 'exited)
          ((string-match-p llm--permission-pattern
                           (llm--last-terminal-line buf))
           'blocked)
          (t 'idle))))

(defun llm--project-label (directory)
  "Return (LABEL . ROOT) for the current project or directory."
  (let* ((proj (project-current nil directory))
         (root (when proj (project-root proj))))
    (cons (if root
              (file-name-nondirectory (directory-file-name root))
            (abbreviate-file-name directory))
          root)))

(defun llm--claude-shell-command (root)
  "Return the claude shell command, using `-c' if ROOT has a `.claude/' dir.
Appends `--dangerously-skip-permissions' when `llm-dangerously-skip-permissions'
is non-nil."
  (let ((base (if (and root (file-directory-p (expand-file-name ".claude" root)))
                  "claude -c"
                "claude")))
    (if llm-dangerously-skip-permissions
        (concat base " --dangerously-skip-permissions")
      base)))

;;;###autoload
(defun llm (&optional user-root)
  "Open Claude CLI in a vterm buffer named *claude:project*.
Without prefix: reuse the existing buffer, or create one.
With \\[universal-argument]: new buffer, continue session if possible.
With \\[universal-argument] \\[universal-argument]: new buffer, fresh session."
  (interactive)
  (pcase-let* ((`(,label . ,root) (llm--project-label (or user-root default-directory)))
               (default-directory (or user-root root default-directory))
               (base (format "*claude:%s*" label))
               (prefix (prefix-numeric-value current-prefix-arg)))
    (cond
     ((= prefix 1)
      (let ((existing (get-buffer base)))
        (if (and existing (buffer-live-p existing))
            (pop-to-buffer existing)
          (let ((vterm-shell (llm--claude-shell-command root)))
            (vterm-other-window base)
            (llm--register-buffer (current-buffer))))))
     ((= prefix 4)
      (let ((vterm-shell (llm--claude-shell-command root))
            (name (generate-new-buffer-name base)))
        (vterm-other-window name)
        (llm--register-buffer (current-buffer))))
     ((>= prefix 16)
      (let ((vterm-shell "claude")
            (name (generate-new-buffer-name base)))
        (vterm-other-window name)
        (llm--register-buffer (current-buffer)))))))

;;;###autoload
(defun llm-vterm-here ()
  "Open or switch to a vterm buffer in the current window.
Without prefix: switch to the last *vterm:LABEL* buffer for the project,
or create one if none exist.
With prefix: always create a new vterm buffer."
  (interactive)
  (pcase-let* ((`(,label . ,_) (llm--project-label default-directory))
               (base (format "*vterm:%s*" label)))
    (if current-prefix-arg
        (vterm (generate-new-buffer-name base))
      (let ((vterm-bufs (cl-remove-if-not
                         (lambda (b) (string-prefix-p base (buffer-name b)))
                         (buffer-list))))
        (if vterm-bufs
            (switch-to-buffer (car vterm-bufs))
          (vterm base))))))

;;; Claude vterm status indicator

(defvar-local llm--status-timer nil
  "Debounce timer for status detection in claude vterm buffers.")

(defface llm-status-idle-face
  '((t :foreground "green3"))
  "Face for claude status when idle/waiting for user input.")

(defface llm-status-busy-face
  '((t :foreground "dark orange"))
  "Face for claude status when thinking/working.")

(defface llm-status-blocked-face
  '((t :foreground "red"))
  "Face for claude status when waiting for user approval.")

(defface llm-status-exited-face
  '((t :foreground "gray50"))
  "Face for claude status when process has exited.")

(defun llm-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a claude vterm buffer."
  (string-prefix-p "*claude:" (buffer-name (or buf (current-buffer)))))

(defun llm--detect-status (buf)
  "Update the status for BUF based on process state.
Uses `llm--status-from-process' to determine new status.
Only triggers a mode-line redraw when the status actually changes.
Drains the prompt queue on transitions to idle/busy."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((old-status (gethash buf llm--buffers))
            (new-status (llm--status-from-process buf)))
        (unless (eq old-status new-status)
          (puthash buf new-status llm--buffers)
          (force-mode-line-update)
          (when (and llm--prompt-queue
                     (memq new-status '(idle busy)))
            (llm--drain-queue)))))))

(defun llm--schedule-status-check ()
  "Schedule a debounced status check for the current claude buffer."
  (when (timerp llm--status-timer)
    (cancel-timer llm--status-timer))
  (setq llm--status-timer
        (run-with-timer 0.5 nil #'llm--detect-status (current-buffer))))

(defun llm--filter-advice (orig-fn process input)
  "After vterm processes output, update buffer status.
Uses `llm--status-from-output' to determine new status based on patterns.
Only redraws when the status actually changes to avoid vterm jitter."
  (funcall orig-fn process input)
  (when-let ((buf (process-buffer process)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (llm-buffer-p)
          (let* ((old-status (gethash buf llm--buffers))
                 (new-status (llm--status-from-output input old-status)))
            (when (and new-status (not (eq old-status new-status)))
              (puthash buf new-status llm--buffers)
              (force-mode-line-update)
              (when (and llm--prompt-queue
                         (memq new-status '(idle busy)))
                (llm--drain-queue)))
            (llm--schedule-status-check)))))))

(advice-add 'vterm--filter :around #'llm--filter-advice)

(defun llm--sentinel-advice (orig-fn process event)
  "Update claude status when the vterm process exits."
  (funcall orig-fn process event)
  (when (and (string-match-p "\\`\\(finished\\|exited\\|signal\\)" event)
             (not (process-live-p process)))
    (when-let ((buf (process-buffer process)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (when (llm-buffer-p)
            (when (timerp llm--status-timer)
              (cancel-timer llm--status-timer)
              (setq llm--status-timer nil))
            (puthash (current-buffer) 'exited llm--buffers)
            (force-mode-line-update)))))))

(advice-add 'vterm--sentinel :around #'llm--sentinel-advice)

(defun llm--mode-line-status ()
  "Return a mode-line string showing the current claude buffer status.
Returns empty string for non-llm buffers."
  (if (not (llm-buffer-p))
      ""
    (let ((status (gethash (current-buffer) llm--buffers)))
      (pcase status
        ('idle    (propertize " ● idle" 'face 'llm-status-idle-face))
        ('busy    (propertize " ◉ busy" 'face 'llm-status-busy-face))
        ('blocked (propertize " ⊘ blocked" 'face 'llm-status-blocked-face))
        ('exited  (propertize " ○ exited" 'face 'llm-status-exited-face))
        (_        (propertize " ● idle" 'face 'llm-status-idle-face))))))

;; Clean up stale timers and unregister dead buffers on re-eval.
(when (hash-table-p llm--buffers)
  (maphash (lambda (buf _status)
             (unless (buffer-live-p buf)
               (remhash buf llm--buffers)))
           llm--buffers))
(dolist (buf (buffer-list))
  (with-current-buffer buf
    (when (and (bound-and-true-p llm--status-timer)
               (timerp llm--status-timer))
      (cancel-timer llm--status-timer)
      (setq llm--status-timer nil))))

(defun llm--cleanup-buffer ()
  "Unregister the current buffer from the claude buffer list."
  (llm--unregister-buffer (current-buffer)))

(add-hook 'kill-buffer-hook #'llm--cleanup-buffer)

(let ((entry '(:eval (llm--mode-line-status))))
  (setq-default mode-line-misc-info
                (cons entry
                      (cl-remove entry (default-value 'mode-line-misc-info)
                                 :test #'equal))))

;;; Prompt Mode

(defun llm--prompt-capf ()
  "Completion-at-point for @file references in the prompt buffer.
Offers project-relative file paths when the point follows `@'."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (let* ((at-start (match-beginning 0))
               (start    (1+ at-start))
               (prefix   (buffer-substring-no-properties start end))
               (root     (or llm--prompt-project-root (llm--current-root))))
          (when (and root (eq (char-before (1+ at-start)) ?@))
            (ignore prefix)
            (list start end
                  (completion-table-dynamic
                   (lambda (_)
                     (when-let ((proj (project-current nil root)))
                       (mapcar (lambda (f) (file-relative-name f root))
                               (project-files proj)))))
                  :exclusive 'no
                  :annotation-function (lambda (_) " file"))))))))

(define-derived-mode llm-prompt-mode text-mode "LLM-Prompt"
  "Major mode for composing multi-line Claude prompts.
\\<llm-prompt-mode-map>\\[llm-prompt-send] to send, \\[llm-prompt-cancel] to cancel."
  (setq header-line-format " Claude  C-c C-c send | C-c C-k cancel")
  (add-hook 'completion-at-point-functions #'llm--prompt-capf nil t))

(define-key llm-prompt-mode-map (kbd "C-c C-c") #'llm-prompt-send)
(define-key llm-prompt-mode-map (kbd "C-c C-k") #'llm-prompt-cancel)
(define-key llm-prompt-mode-map (kbd "C-c C-m") #'llm--bubble-promote)

;;; Interactive Commands

(defvar llm--prompt-queue nil
  "LIFO queue of (BUFFER . PROMPT) entries waiting to be sent when claude is idle.")

(defun llm--drain-queue ()
  "Flush the next prompt from `llm--prompt-queue' if its buffer is ready.
Called from status transitions in `llm--filter-advice' /
`llm--detect-status'; does nothing if the target is busy/blocked."
  (while-let ((entry (car (last llm--prompt-queue)))
              (buf (car entry))
              ((not (buffer-live-p buf))))
    (setq llm--prompt-queue (butlast llm--prompt-queue)))
  (when-let ((entry (car (last llm--prompt-queue))))
    (let ((buf (car entry))
          (prompt (cdr entry)))
      (with-current-buffer buf
        (when (memq (gethash buf llm--buffers) '(idle busy))
          (setq llm--prompt-queue (butlast llm--prompt-queue))
          (vterm-insert prompt)
          (vterm-send-return)
          (let ((remaining (length llm--prompt-queue)))
            (if (zerop remaining)
                (message "Queued prompt sent to %s" (buffer-name buf))
              (message "Queued prompt sent to %s (%d still pending)"
                       (buffer-name buf) remaining))))))))

(defun llm--ensure-ignored (root)
  "Append `.project/' to ROOT's .gitignore if missing.
No-op unless `llm-persistence-strategy' is `project' and ROOT is a git repo."
  (when (and (eq llm-persistence-strategy 'project)
             root
             (file-directory-p (expand-file-name ".git" root)))
    (let* ((gitignore (expand-file-name ".gitignore" root))
           (existing (when (file-readable-p gitignore)
                       (with-temp-buffer
                         (insert-file-contents gitignore)
                         (buffer-string)))))
      (unless (and existing
                   (string-match-p (rx line-start
                                       (? "/")
                                       ".project/"
                                       (? line-end))
                                   existing))
        (with-temp-buffer
          (when existing (insert existing))
          (unless (or (null existing) (string-suffix-p "\n" existing))
            (insert "\n"))
          (insert ".project/\n")
          (write-region (point-min) (point-max) gitignore))))))

(defun llm--save-prompt (prompt root)
  "Save PROMPT under the persistence location for ROOT as a timestamped file."
  (let* ((r (or root (llm--current-root)))
         (dir (llm--persistence-dir r "prompts"))
         (file (expand-file-name
                (format "%s.txt" (format-time-string "%Y%m%d-%H%M%S"))
                dir)))
    (make-directory dir t)
    (llm--ensure-ignored r)
    (with-temp-file file (insert prompt))))

(defun llm--send-to-claude (prompt &optional root)
  "Switch to the claude vterm buffer and insert PROMPT.
If ROOT is provided, switch to the claude buffer for that project root.
If the session is busy or blocked, queue the prompt and insert it
once the session becomes idle."
  (llm--save-prompt prompt root)
  (llm root)
  (let ((status (gethash (current-buffer) llm--buffers)))
    (if (memq status '(nil idle busy))
        (progn (vterm-insert prompt)
               (vterm-send-return))
      (push (cons (current-buffer) prompt) llm--prompt-queue)
      (message "Claude is %s — prompt queued (%d pending), will send when idle"
               status (length llm--prompt-queue)))))

(defun llm--write-context-file (text)
  "Write TEXT to a temporary file and return its path."
  (let ((file (make-temp-file "llm-context-" nil ".txt")))
    (with-temp-file file (insert text))
    file))

;;; Prompt child-frame ("thinking bubble")

(defface llm-prompt-frame-face
  '((((background dark))
     :background "#2a2a3a" :foreground "#e6e6ee")
    (t :background "#fff8e6" :foreground "#1a1a1a"))
  "Face for the prompt child frame's default text and background."
  :group 'llm)

(defface llm-prompt-frame-border-face
  '((((background dark)) :background "#7aa2f7")
    (t :background "#5c7cfa"))
  "Face whose :background colors the prompt child frame's border."
  :group 'llm)

(defvar llm--prompt-frame nil
  "Currently visible prompt child frame, or nil.")

(defvar-local llm--prompt-face-cookie nil
  "Cookie from `face-remap-add-relative' used inside the prompt buffer.")

(defcustom llm-prompt-frame-size '(80 . 14)
  "Target (COLS . ROWS) of the prompt child frame."
  :type '(cons integer integer)
  :group 'llm)

(defcustom llm-prompt-bubble-steps 8
  "Number of animation frames from point to full prompt size."
  :type 'integer
  :group 'llm)

(defcustom llm-prompt-bubble-interval 0.018
  "Seconds between animation steps."
  :type 'number
  :group 'llm)

(defvar llm-prompt-frame-parameters
  '((minibuffer . nil)
    (undecorated . t)
    (internal-border-width . 2)
    (child-frame-border-width . 1)
    (left-fringe . 8) (right-fringe . 8)
    (vertical-scroll-bars . nil) (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0) (tool-bar-lines . 0) (tab-bar-lines . 0)
    (no-accept-focus . nil)
    (unsplittable . t)
    (no-other-frame . t)
    (cursor-type . box)
    (visibility . nil)))

(defun llm--prompt-anchor-xy ()
  "Return pixel (X . Y) at point in the selected window's frame."
  (let* ((edges (window-inside-pixel-edges))
         (posn (posn-at-point))
         (xy (and posn (posn-x-y posn))))
    (if xy
        (cons (+ (nth 0 edges) (car xy))
              (+ (nth 1 edges) (cdr xy) (default-line-height)))
      (cons (nth 0 edges) (nth 1 edges)))))

(defun llm--prompt-apply-styles (frame buf)
  "Apply `llm-prompt-frame-face' + border face to FRAME and BUF."
  (let ((bg  (face-attribute 'llm-prompt-frame-face :background nil 'default))
        (fg  (face-attribute 'llm-prompt-frame-face :foreground nil 'default))
        (bd  (face-attribute 'llm-prompt-frame-border-face :background nil 'default)))
    (when (stringp bg) (set-frame-parameter frame 'background-color bg))
    (when (stringp fg) (set-frame-parameter frame 'foreground-color fg))
    (dolist (face '(internal-border child-frame-border))
      (when (facep face)
        (set-face-background face (if (stringp bd) bd 'unspecified) frame)))
    (with-current-buffer buf
      (when llm--prompt-face-cookie
        (face-remap-remove-relative llm--prompt-face-cookie))
      (setq llm--prompt-face-cookie
            (face-remap-add-relative 'default 'llm-prompt-frame-face)))))

(defun llm--prompt-make-frame (buf anchor)
  "Create the prompt child frame showing BUF, anchored at ANCHOR (X . Y) pixels."
  (let* ((parent (selected-frame))
         (params (append `((parent-frame . ,parent)
                           (left . ,(car anchor))
                           (top  . ,(cdr anchor))
                           (width . 1) (height . 1))
                         llm-prompt-frame-parameters))
         (frame (make-frame params))
         (win (frame-selected-window frame)))
    (set-window-buffer win buf)
    (set-window-dedicated-p win t)
    (set-window-parameter win 'no-other-window t)
    (llm--prompt-apply-styles frame buf)
    (make-frame-visible frame)
    frame))

(defun llm--animate-prompt-frame (frame target-w target-h)
  "Grow FRAME from 1x1 to TARGET-W x TARGET-H over `llm-prompt-bubble-steps'."
  (let* ((i 0) (steps llm-prompt-bubble-steps) timer)
    (setq timer
          (run-with-timer
           0 llm-prompt-bubble-interval
           (lambda ()
             (cl-incf i)
             (cond
              ((not (frame-live-p frame))
               (cancel-timer timer))
              ((>= i steps)
               (set-frame-size frame target-w target-h)
               (select-frame-set-input-focus frame)
               (cancel-timer timer))
              (t
               (let ((k (/ (float i) steps)))
                 (set-frame-size frame
                                 (max 1 (round (* target-w k)))
                                 (max 1 (round (* target-h k))))))))))))

(defun llm--close-prompt-frame ()
  "Delete the prompt child frame if it's live."
  (when (and llm--prompt-frame (frame-live-p llm--prompt-frame))
    (delete-frame llm--prompt-frame t))
  (setq llm--prompt-frame nil))

;;; Bubble (inline-response) mode

(defcustom llm-bubble-frame-size '(100 . 24)
  "Target (COLS . ROWS) for the bubble.
Bigger than `llm-prompt-frame-size' because the bubble ends up
displaying Claude's reply, not just the prompt."
  :type '(cons integer integer)
  :group 'llm)

(defcustom llm-bubble-prompt-prefix ""
  "String prepended to the user's text when sending in bubble mode.
Empty by default — the bubble sends your prompt verbatim through
`claude -p', which works in any environment.

Set to \"/btw \" (trailing space) if you've defined a matching
custom slash command at `~/.claude/commands/btw.md' or at
`<project>/.claude/commands/btw.md'.  Any other string works too —
e.g. \"By the way, briefly: \" as a plain-text framing preamble."
  :type 'string
  :group 'llm)

(defface llm-bubble-header-face
  '((t :inherit header-line :slant italic))
  "Face for the bubble header line.")

(defface llm-bubble-user-face
  '((((background dark))  :foreground "#7aa2f7" :weight bold)
    (((background light)) :foreground "#5c7cfa" :weight bold))
  "Face for the \"▸\" turn marker in front of user messages.")

(defface llm-bubble-thinking-face
  '((t :inherit shadow :slant italic))
  "Face for the animated `Thinking…' indicator.")

(defvar-local llm--prompt-bubble nil
  "Non-nil when this prompt buffer is in bubble (inline-response) mode.")

(defvar-local llm--bubble-process nil
  "Async `claude -p' process for a bubble, if running.")

(defvar-local llm--bubble-last-prompt nil
  "The most recent user prompt sent in this bubble.
Used by `llm--bubble-promote' to forward it to the main claude session.")

(defvar-local llm--bubble-input-start nil
  "Marker at the start of the user's current input region.
Nil on the very first send (no conversation history yet); a live marker
once the first reply has settled and subsequent turns are being typed.")

(defvar-local llm--bubble-session-id nil
  "UUID pinning every turn of this bubble to the same claude session.
Generated lazily on bubble creation; used with `--session-id' on every
`claude -p' invocation and with `--resume' when promoting to a
full `*claude:PROJECT*' vterm.")

(defvar-local llm--bubble-dangerous nil
  "Buffer-local copy of `llm-dangerously-skip-permissions' at bubble creation.
Frozen at bubble open so toggling the transient mid-conversation
doesn't retroactively change the session's permission posture.")

(defvar-local llm--bubble-thinking-overlay nil
  "Overlay showing the animated `...' indicator while Claude is thinking.")

(defvar-local llm--bubble-thinking-timer nil
  "Buffer-local timer animating `llm--bubble-thinking-overlay'.")

(defvar-local llm--bubble-thinking-tick 0
  "Counter driving the thinking-dots animation.")

(defun llm--bubble-thinking-string (tick)
  "Return the animated dots string for TICK (1–3 dots)."
  (propertize (make-string (1+ (mod tick 3)) ?.)
              'face 'llm-bubble-thinking-face))

(defun llm--bubble-thinking-tick-fn (buf)
  "Tick BUF's thinking animation one frame forward."
  (when (and (buffer-live-p buf)
             (overlayp (buffer-local-value 'llm--bubble-thinking-overlay buf)))
    (with-current-buffer buf
      (cl-incf llm--bubble-thinking-tick)
      (overlay-put llm--bubble-thinking-overlay
                   'after-string
                   (llm--bubble-thinking-string llm--bubble-thinking-tick)))))

(defun llm--bubble-start-thinking (buf)
  "Begin the `thinking' animation in BUF at current `point-max'."
  (with-current-buffer buf
    (llm--bubble-stop-thinking buf)
    (let* ((pos (point-max))
           (ov  (make-overlay pos pos buf t nil)))
      (overlay-put ov 'after-string (llm--bubble-thinking-string 0))
      (setq-local llm--bubble-thinking-overlay ov)
      (setq-local llm--bubble-thinking-tick 0)
      (setq-local llm--bubble-thinking-timer
                  (run-with-timer 0.4 0.4
                                  #'llm--bubble-thinking-tick-fn buf)))))

(defun llm--bubble-stop-thinking (buf)
  "Cancel BUF's thinking animation and remove its indicator."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp llm--bubble-thinking-timer)
        (cancel-timer llm--bubble-thinking-timer))
      (setq-local llm--bubble-thinking-timer nil)
      (when (overlayp llm--bubble-thinking-overlay)
        (delete-overlay llm--bubble-thinking-overlay))
      (setq-local llm--bubble-thinking-overlay nil))))

(defun llm--generate-uuid ()
  "Return a v4-style UUID string."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior #x4000 (logand (random 65536) #x0fff))
          (logior #x8000 (logand (random 65536) #x3fff))
          (random 65536) (random 65536) (random 65536)))

(defun llm--bubble-command (prompt)
  "Build the `claude' argv for PROMPT on this bubble's pinned session.
Every turn uses `--session-id' with the same UUID, so claude treats all
popup turns as one conversation regardless of what else is happening in
the project directory. Prepends `llm-bubble-prompt-prefix' to PROMPT and,
if `llm--bubble-dangerous' is set for this bubble, passes
`--dangerously-skip-permissions'."
  (let ((text (concat llm-bubble-prompt-prefix prompt)))
    (append (list "claude" "--session-id" llm--bubble-session-id)
            (when llm--bubble-dangerous '("--dangerously-skip-permissions"))
            (list "-p" text))))

(defun llm--bubble-clean-chunk (chunk)
  "Strip CR, ANSI CSI sequences, and OSC sequences from CHUNK.
`ansi-color-apply' handles CSI (colors, cursor); OSC (e.g. title
changes like ESC ] ... BEL) and stray CR are removed by hand."
  (let* ((no-cr  (replace-regexp-in-string "\r" "" chunk))
         (no-osc (replace-regexp-in-string "\e\\][^\a]*\\(?:\a\\|\e\\\\\\)" "" no-cr)))
    (ansi-color-apply no-osc)))

(defun llm--bubble-filter (proc chunk)
  "Process filter: clean CHUNK and append to PROC's buffer.
On the first chunk, replaces the `thinking' indicator with the
claude turn marker."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (first-chunk (overlayp llm--bubble-thinking-overlay))
            (was-at-end (= (point) (point-max)))
            (cleaned (llm--bubble-clean-chunk chunk)))
        (when first-chunk
          (llm--bubble-stop-thinking (current-buffer))
          (save-excursion
            (goto-char (point-max))
            (insert (propertize "— " 'face 'llm-bubble-user-face))))
        (save-excursion
          (goto-char (point-max))
          (insert cleaned))
        (when (or first-chunk was-at-end)
          (goto-char (point-max)))))))

(defun llm--bubble-sentinel (proc _event)
  "Process sentinel: append a fresh input prompt and hand control back."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (llm--bubble-stop-thinking (current-buffer))
      (setq-local llm--bubble-process nil)
      (let ((status (process-status proc))
            (inhibit-read-only t))
        (pcase status
          ('exit
           (goto-char (point-max))
           (insert "\n\n" (propertize "— " 'face 'llm-bubble-user-face))
           (setq-local llm--bubble-input-start (copy-marker (point) nil))
           (setq header-line-format
                 (propertize
                  " Claude  C-c C-c send · C-c C-k close · C-c C-m →claude"
                  'face 'llm-bubble-header-face)))
          ('signal
           (setq header-line-format
                 (propertize " Claude  (cancelled — C-c C-k close)"
                             'face 'llm-bubble-header-face))))))))

(defun llm--bubble-promote ()
  "Close the bubble and open a new *claude:PROJECT* vterm
continuing the same session the popup has been driving.

Every popup turn runs with `--session-id <UUID>', so the conversation
is pinned to one specific claude session. Promote spawns a fresh
interactive claude with `--resume <UUID>' on the same UUID, loading
all prior turns regardless of what else is happening in the directory."
  (interactive)
  (unless llm--bubble-last-prompt
    (user-error "Nothing to promote yet — send a turn first"))
  (unless llm--bubble-session-id
    (user-error "No session id recorded for this bubble"))
  (when (process-live-p llm--bubble-process)
    (user-error "Claude is still responding — wait, or C-c C-k to cancel first"))
  (let* ((root      llm--prompt-project-root)
         (dir       (or root default-directory))
         (label     (car (llm--project-label dir)))
         (base      (format "*claude:%s*" label))
         (name      (generate-new-buffer-name base))
         (sid       llm--bubble-session-id)
         (dangerous llm--bubble-dangerous)
         (bubble    (current-buffer)))
    (llm--close-prompt-frame)
    (kill-buffer bubble)
    (let ((default-directory dir)
          (vterm-shell (format "claude --resume %s%s"
                               (shell-quote-argument sid)
                               (if dangerous
                                   " --dangerously-skip-permissions"
                                 ""))))
      (vterm-other-window name)
      (llm--register-buffer (current-buffer)))))

;;;###autoload
(defun llm-prompt-bubble ()
  "Open a bubble: throwaway prompt that streams the reply inline.
Thin wrapper around `llm-prompt' with the prefix-arg preset, so it's
directly bindable / transient-invokable without universal-argument.

The bubble runs `claude -p', which is non-interactive: any tool that
would normally ask you something (permission prompts, AskUserQuestion)
is auto-failed by the CLI before reaching us. If a turn needs that kind
of interaction, promote with C-c C-m to a `*claude:PROJECT*' vterm
that resumes the same session."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'llm-prompt)))

(defun llm-prompt-bubble-send ()
  "Spawn a new bubble turn (first send or post-response follow-up).
Refuses while a turn is already running."
  (if (process-live-p llm--bubble-process)
      (user-error "Claude is still responding — wait, or C-c C-k to cancel")
    (llm--bubble-spawn-turn)))

(defun llm--bubble-spawn-turn (&optional explicit-prompt)
  "Start a new claude turn (first send or follow-up after completion).
With EXPLICIT-PROMPT, use it as the prompt instead of reading the
buffer's input region, and skip echoing the user turn into the buffer
so only the response is rendered."
  (let* ((has-history (markerp llm--bubble-input-start))
         (prompt (or explicit-prompt
                     (string-trim
                      (if has-history
                          (buffer-substring-no-properties
                           llm--bubble-input-start (point-max))
                        (buffer-string)))))
         (root   llm--prompt-project-root)
         (default-directory (or root default-directory)))
    (when (string-empty-p prompt) (user-error "Empty prompt"))
    (setq-local llm--bubble-last-prompt prompt)
    (let ((inhibit-read-only t)
          (dash (propertize "— " 'face 'llm-bubble-user-face)))
      (cond
       (explicit-prompt
        (erase-buffer))
       (has-history
        (delete-region llm--bubble-input-start (point-max))
        (goto-char (point-max))
        (insert prompt "\n\n"))
       (t
        (erase-buffer)
        (insert dash prompt "\n\n")))
      (setq-local llm--bubble-input-start nil)
      (llm--bubble-start-thinking (current-buffer))
      (setq header-line-format
            (propertize " Claude  (running — C-c C-k cancel)"
                        'face 'llm-bubble-header-face)))
    (let* ((args (llm--bubble-command prompt))
           (process-environment
            (append '("NO_COLOR=1" "CLICOLOR=0" "TERM=dumb")
                    process-environment))
           (proc (apply #'start-process "llm-bubble" (current-buffer) args)))
      (setq-local llm--bubble-process proc)
      (set-process-filter   proc #'llm--bubble-filter)
      (set-process-sentinel proc #'llm--bubble-sentinel))))

;;;###autoload
(defun llm-prompt-send ()
  "Send the contents of the prompt buffer to Claude.
In bubble mode: run `claude -p' as a subprocess and stream the reply
into the same bubble. Otherwise: hand off to the project's claude
vterm session (queues if busy)."
  (interactive)
  (if llm--prompt-bubble
      (llm-prompt-bubble-send)
    (let* ((prompt (string-trim (buffer-string)))
           (ctx    llm--prompt-context-prefix)
           (root   llm--prompt-project-root)
           (buf    (current-buffer))
           (full   (if ctx (concat ctx prompt) prompt)))
      (when (string-empty-p prompt) (user-error "Empty prompt"))
      (llm--close-prompt-frame)
      (kill-buffer buf)
      (llm--send-to-claude full root))))

(defun llm-prompt-cancel ()
  "Cancel the prompt or response.
If a bubble subprocess is running, kill it and keep the bubble open.
Otherwise close the bubble and kill its buffer."
  (interactive)
  (cond
   ((and llm--prompt-bubble (process-live-p llm--bubble-process))
    (kill-process llm--bubble-process)
    (setq-local llm--bubble-process nil)
    (llm--bubble-stop-thinking (current-buffer))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\n[cancelled]\n"))
    (setq header-line-format
          (propertize " Claude bubble  (cancelled — q close)"
                      'face 'llm-bubble-header-face)))
   (t
    (let ((buf (current-buffer)))
      (llm--close-prompt-frame)
      (kill-buffer buf)
      (message "Prompt cancelled")))))

;;;###autoload
(defun llm-prompt (&optional arg)
  "Open a multi-line prompt buffer for Claude.
Pre-populates context based on the current state:
- Active region: inserts a file/region context prefix
- Otherwise: inserts a file+line context prefix

With \\[universal-argument] ARG: bubble mode.  Opens a fresh throwaway
bubble with no file-context prefix; on send, runs `claude -p <prompt>'
as a subprocess and streams the reply into the same bubble.
Nothing is saved to prompt history and the main claude vterm is
untouched."
  (interactive "P")
  (let* ((bubble (consp arg))
         (proj (project-current nil default-directory))
         (root (when proj (project-root proj)))
         (root (or root (llm--project-root default-directory)))
         (file-name (buffer-file-name))
         (prefix (unless bubble
                   (cond
                    ((use-region-p)
                     (let* ((start (region-beginning))
                            (end (region-end))
                            (context (buffer-substring-no-properties start end))
                            (file (if file-name
                                      file-name
                                    (llm--write-context-file context))))
                       (deactivate-mark)
                       (if file-name
                           (format "Read file %s lines %d-%d\n\n"
                                   file (line-number-at-pos start) (line-number-at-pos end))
                         (format "Context: %s\n\n" file))))
                    (file-name (format "%s:%d\n\n" file-name (line-number-at-pos (point)))))))
         (buf (if bubble
                  (generate-new-buffer "*llm-bubble*")
                (get-buffer-create "*llm-prompt*"))))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (setq-local llm--prompt-context-prefix prefix)
      (setq-local llm--prompt-project-root root)
      (setq-local llm--prompt-bubble bubble)
      (when bubble
        (setq-local llm--bubble-session-id (llm--generate-uuid))
        (setq-local llm--bubble-dangerous llm-dangerously-skip-permissions)
        (goto-char (point-max))
        (insert (propertize "— " 'face 'llm-bubble-user-face))
        (setq-local llm--bubble-input-start (copy-marker (point) nil))
        (setq header-line-format
              (propertize
               " Claude  C-c C-c send · C-c C-k close · C-c C-m →claude"
               'face 'llm-bubble-header-face))))
    (llm--close-prompt-frame)
    (if (display-graphic-p)
        (let* ((size (if bubble llm-bubble-frame-size llm-prompt-frame-size))
               (anchor (llm--prompt-anchor-xy))
               (frame (llm--prompt-make-frame buf anchor)))
          (setq llm--prompt-frame frame)
          (llm--animate-prompt-frame frame (car size) (cdr size)))
      (pop-to-buffer buf))))

;;; Prompt History

(defun llm--prompts-dir (&optional root)
  "Absolute path to the prompts directory for ROOT.
Honors `llm-persistence-strategy'."
  (llm--persistence-dir (or root (llm--current-root)) "prompts"))

(defun llm--prompt-history-files (&optional root)
  "Return ROOT's saved prompt files, newest first."
  (let ((dir (llm--prompts-dir root)))
    (when (file-directory-p dir)
      (sort (directory-files dir t "\\.txt\\'") #'string>))))

(defun llm--prompt-preview (file)
  "Return a one-line preview string for FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 200)
    (replace-regexp-in-string "[\n\t]+" " " (buffer-string))))

(defun llm--open-prompt-in-bubble (text root)
  "Show TEXT in the prompt bubble, tagged for ROOT."
  (let ((buf (get-buffer-create "*llm-prompt*")))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (insert text)
      (setq-local llm--prompt-project-root root))
    (llm--close-prompt-frame)
    (if (display-graphic-p)
        (let* ((anchor (llm--prompt-anchor-xy))
               (frame (llm--prompt-make-frame buf anchor)))
          (setq llm--prompt-frame frame)
          (llm--animate-prompt-frame frame
                                     (car llm-prompt-frame-size)
                                     (cdr llm-prompt-frame-size)))
      (pop-to-buffer buf))))

;;;###autoload
(defun llm-prompt-history ()
  "Browse saved prompts for the current project.
Picks a prompt via `completing-read' and opens it in the bubble
for editing and re-sending."
  (interactive)
  (let* ((root  (llm--current-root))
         (files (llm--prompt-history-files root)))
    (unless files (user-error "No saved prompts for this project"))
    (let* ((cands (mapcar (lambda (f)
                            (cons (format "%s  %s"
                                          (file-name-base f)
                                          (truncate-string-to-width
                                           (llm--prompt-preview f)
                                           80 nil nil "…"))
                                  f))
                          files))
           (choice (completing-read "Prompt: " (mapcar #'car cands) nil t))
           (file   (cdr (assoc choice cands)))
           (text   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
      (llm--open-prompt-in-bubble text root))))

;;;###autoload
(defun llm-prompt-resume ()
  "Re-open the most recent saved prompt for this project in the bubble."
  (interactive)
  (let* ((root  (llm--current-root))
         (files (llm--prompt-history-files root)))
    (unless files (user-error "No saved prompts for this project"))
    (let ((text (with-temp-buffer
                  (insert-file-contents (car files))
                  (buffer-string))))
      (llm--open-prompt-in-bubble text root))))

;;; Change Highlighting on Revert

(defface llm-change-highlight-face
  '((((background dark))  :background "#1a3a1a" :extend t)
    (((background light)) :background "#d4f4d4" :extend t))
  "Face applied to lines added or changed in the last auto-revert.")

(defvar llm--pre-revert-contents (make-hash-table :test 'equal)
  "Hash-table mapping absolute file paths to their buffer text captured
just before `auto-revert-mode' reverts them.")

(defvar-local llm--change-highlight-timer nil
  "Buffer-local idle timer that removes `llm-change-highlight' overlays.")

(defun llm-change-highlight-clear (&optional buf)
  "Remove all change-highlight overlays from BUF (default: current buffer).
Also cancels the auto-clear timer if one is pending."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when (timerp llm--change-highlight-timer)
      (cancel-timer llm--change-highlight-timer)
      (setq llm--change-highlight-timer nil))
    (remove-overlays (point-min) (point-max) 'category 'llm-change-highlight)))

(defun llm--before-revert-save ()
  "Hook: capture buffer text before `auto-revert-mode' reverts it."
  (when (buffer-file-name)
    (puthash (buffer-file-name)
             (buffer-substring-no-properties (point-min) (point-max))
             llm--pre-revert-contents)))

(defun llm--after-revert-highlight ()
  "Hook: highlight lines in the reverted buffer that differ from the snapshot."
  (when-let* ((file  (buffer-file-name))
              (old   (gethash file llm--pre-revert-contents)))
    (remhash file llm--pre-revert-contents)
    (let* ((new   (buffer-substring-no-properties (point-min) (point-max)))
           (lines (llm--diff-added-lines old new))
           (buf   (current-buffer)))
      (llm-change-highlight-clear buf)
      (save-excursion
        (dolist (lnum lines)
          (goto-char (point-min))
          (forward-line (1- lnum))
          (let ((ov (make-overlay (line-beginning-position)
                                  (min (point-max) (1+ (line-end-position))))))
            (overlay-put ov 'face     'llm-change-highlight-face)
            (overlay-put ov 'category 'llm-change-highlight)
            (overlay-put ov 'priority 10))))
      (setq llm--change-highlight-timer
            (run-with-timer 60 nil #'llm-change-highlight-clear buf)))))

(defun llm--diff-added-lines (old new)
  "Return a sorted list of 1-based line numbers added/changed in NEW vs OLD.
Returns nil immediately when OLD equals NEW (frequent auto-revert case
where the timer fires but nothing actually changed on disk)."
  (unless (string= old new)
    (let ((old-file (make-temp-file "llm-diff-a"))
          (new-file (make-temp-file "llm-diff-b"))
          lines)
      (unwind-protect
          (progn
            (with-temp-file old-file (insert old))
            (with-temp-file new-file (insert new))
            (with-temp-buffer
              (call-process "diff" nil t nil
                            "--new-line-format=%dn\n"
                            "--old-line-format="
                            "--unchanged-line-format="
                            old-file new-file)
              (goto-char (point-min))
              (while (re-search-forward "^\\([0-9]+\\)$" nil t)
                (push (string-to-number (match-string 1)) lines))))
        (ignore-errors (delete-file old-file))
        (ignore-errors (delete-file new-file)))
      (nreverse lines))))

(add-hook 'before-revert-hook #'llm--before-revert-save)
(add-hook 'after-revert-hook  #'llm--after-revert-highlight)

;;;###autoload
(defun llm-switch-buffer ()
  "Switch between claude buffers, showing status in the menu."
  (interactive)
  (let ((bufs (llm--get-buffers)))
    (unless bufs
      (user-error "No claude buffers"))
    (let ((entries (cl-loop for buffer in bufs
                            collect (let ((name (buffer-name buffer))
                                          (status (gethash buffer llm--buffers)))
                                      (cons (if status
                                                (format "%s [%s]" name status)
                                              name)
                                            buffer)))))
      (let* ((choice (completing-read "Claude buffer: "
                                      (mapcar #'car entries)
                                      nil t))
             (buf (cdr (assoc choice entries))))
        (pop-to-buffer buf)))))

(defun llm--buffer-visible-elsewhere-p (buf)
  "Non-nil if BUF is shown in any visible window other than the selected one."
  (cl-some (lambda (w) (not (eq w (selected-window))))
           (get-buffer-window-list buf nil 'visible)))

(defun llm--cycle-buffer (direction)
  "Switch current window to the next/previous claude buffer.
DIRECTION is +1 (forward) or -1 (backward). Buffers already visible
in another window are deprioritized (sorted to the back), so cycling
prefers ones not yet on screen."
  (let ((bufs (sort (llm--get-buffers)
                    (lambda (a b)
                      (let ((va (llm--buffer-visible-elsewhere-p a))
                            (vb (llm--buffer-visible-elsewhere-p b)))
                        (cond
                         ((and va (not vb)) nil)
                         ((and (not va) vb) t)
                         (t (string< (buffer-name a) (buffer-name b)))))))))
    (unless bufs (user-error "No claude buffers"))
    (let* ((pos (cl-position (current-buffer) bufs))
           (next (if pos
                     (nth (mod (+ pos direction) (length bufs)) bufs)
                   (car bufs))))
      (switch-to-buffer next))))

;;;###autoload
(defun llm-next-buffer ()
  "Switch current window to the next claude buffer."
  (interactive)
  (llm--cycle-buffer +1))

;;;###autoload
(defun llm-previous-buffer ()
  "Switch current window to the previous claude buffer."
  (interactive)
  (llm--cycle-buffer -1))

;;; FIXME/TODO Annotation System

(defvar llm--annotations (make-hash-table :test 'equal)
  "Hash table mapping (ROOT . KIND) to list of annotation entries.
KIND is a string like \"FIXME\" or \"TODO\".
Each entry is a plist (:file :line :text :time).")

(defun llm--annotation-file (root kind)
  "Return the persistence file path for KIND annotations in ROOT.
Honors `llm-persistence-strategy'."
  (llm--persistence-dir root (format "%s.el" (downcase kind))))

(defun llm--annotation-file-for-read (root kind)
  "Like `llm--annotation-file', but falls back to the legacy `.project/' path
if the primary path doesn't exist. Lets existing in-repo annotations stay
visible after the user switches `llm-persistence-strategy' to `user'."
  (let ((primary (llm--annotation-file root kind))
        (legacy  (llm--legacy-project-path root (format "%s.el" (downcase kind)))))
    (if (file-readable-p primary)
        primary
      (if (file-readable-p legacy) legacy primary))))

(defun llm--annotation-key (root kind)
  "Return the hash key for ROOT and KIND."
  (cons root kind))

(defun llm--annotation-load (root kind)
  "Load annotations of KIND for ROOT from disk.
Reads from `llm--annotation-file-for-read' so legacy `.project/' data
stays visible after switching `llm-persistence-strategy' to `user'."
  (let ((file (llm--annotation-file-for-read root kind)))
    (puthash (llm--annotation-key root kind)
             (when (file-readable-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (read (current-buffer))))
             llm--annotations)))

(defun llm--annotation-save (root kind)
  "Save annotations of KIND for ROOT to disk."
  (let ((file (llm--annotation-file root kind))
        (entries (gethash (llm--annotation-key root kind) llm--annotations)))
    (make-directory (file-name-directory file) t)
    (llm--ensure-ignored root)
    (with-temp-file file
      (pp entries (current-buffer)))))

(defun llm--annotation-alive-p (entry kind)
  "Return non-nil if ENTRY's KIND comment still exists in the file."
  (let ((file (plist-get entry :file))
        (text (plist-get entry :text)))
    (and (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (let ((needle (concat kind ": " (car (split-string text "\n")))))
             (search-forward needle nil t))))))

(defun llm--annotation-entries (root kind)
  "Return the list of live KIND annotations for ROOT.
Loads from disk if needed, then prunes entries whose comment
has been removed from the source file."
  (let ((key (llm--annotation-key root kind)))
    (unless (gethash key llm--annotations)
      (llm--annotation-load root kind))
    (let* ((entries (gethash key llm--annotations))
           (live (cl-remove-if-not (lambda (e) (llm--annotation-alive-p e kind)) entries)))
      (unless (= (length entries) (length live))
        (puthash key live llm--annotations)
        (llm--annotation-save root kind))
      live)))

(defun llm--annotation-comment (kind text)
  "Return a KIND comment for TEXT using the current mode's comment syntax."
  (let ((cs (string-trim-right (or comment-start "# ")))
        (ce (let ((e (or comment-end ""))) (if (string-empty-p e) "" (concat " " e)))))
    (mapconcat (lambda (line)
                 (concat cs " " kind "(llm): " line ce))
               (split-string text "\n")
               "\n")))

(defvar-local llm--annotation-kind nil
  "The annotation kind (\"FIXME\" or \"TODO\") for the current prompt buffer.")
(defvar-local llm--annotation-source-buf nil)
(defvar-local llm--annotation-source-file nil)
(defvar-local llm--annotation-source-line nil)

(defun llm--annotation-send ()
  "Insert the annotation comment at the source location and save."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (kind llm--annotation-kind)
        (root llm--prompt-project-root)
        (source-buf llm--annotation-source-buf)
        (source-file llm--annotation-source-file)
        (source-line llm--annotation-source-line))
    (when (string-empty-p text)
      (user-error "Empty %s text" kind))
    (kill-buffer (current-buffer))
    (when (buffer-live-p source-buf)
      (with-current-buffer source-buf
        (save-excursion
          (goto-char (point-min))
          (forward-line (1- source-line))
          (beginning-of-line)
          (open-line 1)
          (insert (llm--annotation-comment kind text))
          (indent-region (line-beginning-position) (line-end-position)))))
    (let* ((key (llm--annotation-key root kind))
           (entries (llm--annotation-entries root kind)))
      (push (list :file source-file :line source-line
                  :text text :time (format-time-string "%Y-%m-%d %H:%M"))
            entries)
      (puthash key entries llm--annotations))
    (llm--annotation-save root kind)
    (message "%s added at %s:%d" kind source-file source-line)))

(defun llm--add-annotation (kind)
  "Open a prompt buffer to compose a KIND annotation."
  (let* ((root (llm--current-root))
         (source-buf (current-buffer))
         (source-file (or (buffer-file-name) (buffer-name)))
         (source-line (line-number-at-pos (point)))
         (buf (get-buffer-create (format "*llm-%s*" (downcase kind)))))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (setq-local llm--prompt-project-root root)
      (setq-local llm--annotation-kind kind)
      (setq-local llm--annotation-source-buf source-buf)
      (setq-local llm--annotation-source-file source-file)
      (setq-local llm--annotation-source-line source-line)
      (setq header-line-format
            (format " %s  C-c C-c insert | C-c C-k cancel" kind))
      (use-local-map (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map text-mode-map)
                        (define-key map (kbd "C-c C-c") #'llm--annotation-send)
                        (define-key map (kbd "C-c C-k") #'llm-prompt-cancel)
                        map)))
    (pop-to-buffer buf)))

(defvar-local llm--annotation-list-kind nil
  "The annotation kind being displayed in the current list buffer.")

(defvar-local llm--annotation-list-root nil
  "The project root whose annotations are displayed in the current list buffer.")

(defun llm--annotation-list-refresh ()
  "Rebuild `tabulated-list-entries' from live annotations."
  (let ((entries (llm--annotation-entries llm--annotation-list-root
                                          llm--annotation-list-kind)))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (list e
                          (vector (file-relative-name (plist-get e :file)
                                                      llm--annotation-list-root)
                                  (number-to-string (plist-get e :line))
                                  (or (plist-get e :time) "")
                                  (truncate-string-to-width
                                   (replace-regexp-in-string "\n" " ⏎ "
                                                             (plist-get e :text))
                                   80 nil nil "…"))))
                  entries))
    (tabulated-list-print t)))

(defun llm-annotation-list-visit ()
  "Jump to the source location of the annotation at point."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (file  (plist-get entry :file))
         (line  (plist-get entry :line)))
    (unless entry (user-error "No annotation at point"))
    (unless (file-exists-p file) (user-error "File gone: %s" file))
    (find-file-other-window file)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun llm-annotation-list-delete ()
  "Delete the annotation at point from persistence.
Source-file comment is left untouched — remove it manually if desired."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (kind  llm--annotation-list-kind)
         (root  llm--annotation-list-root)
         (key   (llm--annotation-key root kind)))
    (unless entry (user-error "No annotation at point"))
    (puthash key
             (cl-remove entry (gethash key llm--annotations) :test #'equal)
             llm--annotations)
    (llm--annotation-save root kind)
    (llm--annotation-list-refresh)))

(defvar llm-annotation-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'llm-annotation-list-visit)
    (define-key map (kbd "o")   #'llm-annotation-list-visit)
    (define-key map (kbd "d")   #'llm-annotation-list-delete)
    (define-key map (kbd "x")   #'llm-annotation-list-delete)
    (define-key map (kbd "g")   #'llm--annotation-list-refresh)
    map)
  "Keymap for `llm-annotation-list-mode'.")

(define-derived-mode llm-annotation-list-mode tabulated-list-mode "LLM-Annotations"
  "Tabulated view of project annotations.
\\<llm-annotation-list-mode-map>\
\\[llm-annotation-list-visit] visit, \\[llm-annotation-list-delete] delete, \
\\[llm--annotation-list-refresh] refresh."
  (setq tabulated-list-format
        [("File" 40 t) ("Line" 6 t) ("Time" 17 t) ("Text" 0 nil)])
  (setq tabulated-list-sort-key '("File"))
  (tabulated-list-init-header))

(defun llm--list-annotations (kind)
  "Open a tabulated list of KIND annotations for the current project."
  (let* ((root (llm--current-root))
         (entries (llm--annotation-entries root kind)))
    (unless entries
      (user-error "No %ss in this project" kind))
    (let ((buf (get-buffer-create (format "*llm-%ss: %s*"
                                          (downcase kind)
                                          (file-name-nondirectory
                                           (directory-file-name root))))))
      (with-current-buffer buf
        (llm-annotation-list-mode)
        (setq llm--annotation-list-kind kind
              llm--annotation-list-root root)
        (llm--annotation-list-refresh))
      (pop-to-buffer buf))))

(defun llm--send-annotations (kind)
  "Send all KIND annotations for the current project to Claude."
  (let* ((root (llm--current-root))
         (entries (llm--annotation-entries root kind)))
    (unless entries
      (user-error "No %ss in this project" kind))
    (let ((prompt (mapconcat
                   (lambda (e)
                     (format "%s at %s:%d — %s"
                             kind
                             (plist-get e :file)
                             (plist-get e :line)
                             (plist-get e :text)))
                   entries "\n")))
      (llm--send-to-claude
       (format "Resolve the following %ss in this project:\n\n%s" kind prompt)))))

;;;###autoload
(defun llm-add-fixme ()
  "Add a FIXME annotation at point."
  (interactive)
  (llm--add-annotation "FIXME"))

;;;###autoload
(defun llm-add-todo ()
  "Add a TODO annotation at point."
  (interactive)
  (llm--add-annotation "TODO"))

;;;###autoload
(defun llm-list-fixmes ()
  "List all FIXMEs for the current project."
  (interactive)
  (llm--list-annotations "FIXME"))

;;;###autoload
(defun llm-list-todos ()
  "List all TODOs for the current project."
  (interactive)
  (llm--list-annotations "TODO"))

;;;###autoload
(defun llm-send-fixmes ()
  "Send all FIXMEs to Claude."
  (interactive)
  (llm--send-annotations "FIXME"))

;;;###autoload
(defun llm-send-todos ()
  "Send all TODOs to Claude."
  (interactive)
  (llm--send-annotations "TODO"))

;;;###autoload
(defun llm-grep-annotations ()
  "Grep all TODO/FIXME/HACK/XXX comments in the current project."
  (interactive)
  (let ((root (llm--current-root)))
    (grep-find (format "grep -rnE '(TODO|FIXME|HACK|XXX):?' %s --include='*.*' -I"
                       (shell-quote-argument (directory-file-name root))))))

;;; Transient Menu

(defun llm--menu-dangerous-p ()
  "Return non-nil if the menu's `-d' switch is active for this invocation."
  (member "--dangerously-skip-permissions" (transient-args 'llm-menu)))

(transient-define-suffix llm--menu-prompt-bubble ()
  "Launch the inline-conversation bubble; honors the menu's switches.
- `--btw' prepends the `/btw ' slash-command prefix to every turn.
- `--dangerously-skip-permissions' is captured into the bubble and
  passed to every `claude -p' turn as well as the promote vterm."
  :description "Prompt inline (conversation)"
  (interactive)
  (let ((llm-bubble-prompt-prefix
         (if (member "--btw" (transient-args 'llm-menu))
             "/btw "
           ""))
        (llm-dangerously-skip-permissions
         (or llm-dangerously-skip-permissions (llm--menu-dangerous-p))))
    (llm-prompt-bubble)))

(transient-define-suffix llm--menu-open-claude ()
  "Open the main *claude:PROJECT* vterm; honors the `-d' switch."
  :description "Open Claude in project"
  (interactive)
  (let ((llm-dangerously-skip-permissions
         (or llm-dangerously-skip-permissions (llm--menu-dangerous-p))))
    (call-interactively #'llm)))

(transient-define-prefix llm-menu ()
  "Claude CLI commands."
  ["Options"
   ("-b" "Prepend /btw slash-command to inline prompts" "--btw")
   ("-d" "Dangerously skip permission prompts"          "--dangerously-skip-permissions")]
  [["Session"
    ("c" llm--menu-open-claude)
    ("v" "Vterm in project"       llm-vterm-here)
    ("b" "Switch buffer"          llm-switch-buffer)
    ("p" "Prompt"                 llm-prompt)
    ("P" llm--menu-prompt-bubble)
    ("r" "Resume last prompt"     llm-prompt-resume)
    ("H" "Prompt history"         llm-prompt-history)
    ("?" "Describe at point"      llm-describe-at-point)]
   ["Annotations"
    ("f" "Add FIXME"          llm-add-fixme)
    ("t" "Add TODO"           llm-add-todo)
    ("F" "List FIXMEs"        llm-list-fixmes)
    ("T" "List TODOs"         llm-list-todos)
    ("S" "Send FIXMEs"        llm-send-fixmes)
    ("G" "Grep annotations"   llm-grep-annotations)]
   ["Highlights"
    ("h" "Clear revert highlights" llm-change-highlight-clear)]])

;;;###autoload
(defun llm-toggle-vterm-claude ()
  "Toggle current window between `*vterm:PROJECT*' and `*claude:PROJECT*'.
Switches to the counterpart of the current buffer, creating it in the
current window if missing.  When the current buffer is neither, jump
to the project's vterm first (reusing or spawning)."
  (interactive)
  (let ((name (buffer-name)))
    (if (string-match "\\`\\*\\(vterm\\|claude\\):\\(.*\\)\\*\\'" name)
        (let* ((kind   (match-string 1 name))
               (label  (match-string 2 name))
               (target (format (if (equal kind "vterm") "*claude:%s*" "*vterm:%s*")
                               label))
               (existing (get-buffer target)))
          (if (buffer-live-p existing)
              (switch-to-buffer existing)
            (pcase-let* ((`(,_ . ,root) (llm--project-label default-directory))
                         (default-directory (or root default-directory)))
              (if (equal kind "vterm")
                  (let ((vterm-shell (llm--claude-shell-command root)))
                    (vterm target)
                    (llm--register-buffer (current-buffer)))
                (vterm target)))))
      (let ((current-prefix-arg nil))
        (llm-vterm-here)))))

;;;###autoload
(defun llm-describe-at-point ()
  "Ask Claude to describe the symbol at point or the active region.
Spawns a bubble seeded with a prompt referencing the visiting file and
line(s), and auto-sends.  If the buffer isn't visiting a file, the
buffer name is used as context instead."
  (interactive)
  (let* ((region-p (use-region-p))
         (rb (and region-p (region-beginning)))
         (re (and region-p (region-end)))
         (thing (cond
                 (region-p (string-trim (buffer-substring-no-properties rb re)))
                 ((thing-at-point 'symbol t))
                 (t (user-error "No symbol at point and no active region"))))
         (file-name (buffer-file-name))
         (loc (cond
               (region-p (format "lines %d-%d"
                                 (line-number-at-pos rb)
                                 (line-number-at-pos re)))
               (t        (format "line %d" (line-number-at-pos (point))))))
         (where (if file-name
                    (format "%s (%s)" file-name loc)
                  (format "buffer %s (%s)" (buffer-name) loc)))
         (text (if (string-match-p "\n" thing)
                   (format "Describe the following snippet in the context of %s:\n\n```\n%s\n```"
                           where thing)
                 (format "Describe `%s` in the context of %s." thing where)))
         (root (llm--current-root))
         (buf (generate-new-buffer "*llm-bubble*")))
    (when region-p (deactivate-mark))
    (with-current-buffer buf
      (llm-prompt-mode)
      (erase-buffer)
      (setq-local llm--prompt-project-root root)
      (setq-local llm--prompt-bubble t)
      (setq-local llm--bubble-session-id (llm--generate-uuid))
      (setq-local llm--bubble-dangerous llm-dangerously-skip-permissions))
    (llm--close-prompt-frame)
    (if (display-graphic-p)
        (let* ((size llm-bubble-frame-size)
               (anchor (llm--prompt-anchor-xy))
               (frame (llm--prompt-make-frame buf anchor)))
          (setq llm--prompt-frame frame)
          (llm--animate-prompt-frame frame (car size) (cdr size)))
      (pop-to-buffer buf))
    (with-current-buffer buf
      (llm--bubble-spawn-turn text))))

;;; Keybindings

(global-set-key (kbd "C-x y e") #'llm-menu)
(global-set-key (kbd "C-S-j")   #'llm-next-buffer)
(global-set-key (kbd "C-S-k")   #'llm-previous-buffer)
(global-set-key (kbd "C-x C-x") #'llm-toggle-vterm-claude)

(provide 'mijn-llm)
;;; mijn-llm.el ends here
