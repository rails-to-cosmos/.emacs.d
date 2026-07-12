;;; mijn-llm.el --- Claude CLI integration for Emacs -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'subr-x)
(require 'project)
(require 'transient)
(require 'ansi-color)
(require 'vterm)

(defvar vterm-copy-mode)
(defvar vterm-mode-map)
(defvar vterm-copy-mode-hook)
(defvar vterm--process)


(use-package vterm
  :ensure nil)

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

(defconst llm-model-choices
  '("claude-opus-4-8"
    "claude-opus-4-7"
    "claude-opus-4-6"
    "claude-sonnet-4-6"
    "claude-haiku-4-5")
  "Claude models offered by `llm-menu' under the `-m' switch.
The CLI accepts these full names as well as aliases like \"opus\",
\"sonnet\", \"haiku\".  Edit this list to add new models as they ship.")

(defcustom llm-model nil
  "Model passed to claude as `--model'.
Nil means use claude's default (whatever its config picks).  A string
is passed through verbatim — full names like \"claude-opus-4-7\" or
aliases like \"opus\" both work.

Normally toggled per-invocation via the `-m' switch in `llm-menu'
rather than set directly.  Applies to the main `*claude:PROJECT*'
vterm and to the inline bubble (captured once at bubble creation)."
  :type '(choice (const :tag "Default (claude picks)" nil)
                 (string :tag "Model name"))
  :group 'llm)

;;; Model version parsing (for gating effort levels per model)

(defun llm--model-split (model)
  "Split MODEL into (FAMILY NUMS), without resolving aliases.
FAMILY is the family string (\"opus\", \"sonnet\", ...) or nil.  NUMS is
the version as up to two integers (major minor); it is empty for a bare
alias like \"opus\".  The optional `claude-' prefix and any trailing
date component (e.g. in \"haiku-4-5-20251001\") are ignored."
  (let* ((name (string-remove-prefix "claude-" (or model "")))
         (parts (split-string name "-" t)))
    (list (car parts)
          (seq-take
           (cl-loop for p in (cdr parts)
                    when (string-match-p "\\`[0-9]+\\'" p)
                    collect (string-to-number p))
           2))))

(defun llm--version< (a b)
  "Non-nil when version list A precedes B (lexicographic on integers).
A missing component sorts before a present one, so (4) precedes (4 8)."
  (cond ((null a) (and b t))
        ((null b) nil)
        ((= (car a) (car b)) (llm--version< (cdr a) (cdr b)))
        (t (< (car a) (car b)))))

(defun llm--version>= (a b)
  "Non-nil when version list A is at least B."
  (not (llm--version< a b)))

(defun llm--family-newest-version (family)
  "Newest version (a list like (4 8)) for FAMILY in `llm-model-choices'.
Nil when FAMILY appears in no listed model."
  (let (best)
    (dolist (m llm-model-choices best)
      (cl-destructuring-bind (fam nums) (llm--model-split m)
        (when (and (equal fam family) nums
                   (or (null best) (llm--version< best nums)))
          (setq best nums))))))

(defun llm--model-family+version (model)
  "Return (FAMILY . VERSION) for MODEL, resolving aliases; nil if unknown.
FAMILY is a string like \"opus\"; VERSION is an integer list like (4 8).
A bare alias (\"opus\") resolves to the newest version of that family in
`llm-model-choices'."
  (when (and model (not (string-empty-p model)))
    (cl-destructuring-bind (family nums) (llm--model-split model)
      (when family
        (cons family (or nums (llm--family-newest-version family)))))))

(defun llm--model-supports-ultracode-p (model)
  "Non-nil when MODEL can use the `ultracode' effort level.
Nil MODEL (claude's own default) is treated as eligible, since the CLI
default is a current-generation model.  Otherwise `ultracode' is offered
for opus 4.8 and newer, and for any Claude 5-family model."
  (or (null model)
      (let ((fv (llm--model-family+version model)))
        (and fv
             (let ((family (car fv))
                   (version (cdr fv)))
               (or (llm--version>= version '(5))
                   (and (equal family "opus")
                        (llm--version>= version '(4 8)))))))))

;;; Reasoning effort

(defconst llm-effort-choices
  '("default"
    "low"
    "medium"
    "high"
    "max")
  "Reasoning-effort levels offered by `llm-menu' for every model.
Levels available only for some models live in
`llm-effort-model-choices' and are appended by
`llm-effort-choices-for-model'.")

(defconst llm-effort-model-choices
  '(("ultracode" . llm--model-supports-ultracode-p))
  "Effort levels gated behind a model predicate.
Each entry is (EFFORT . PREDICATE); PREDICATE is called with the model
name (a string, or nil for claude's default) and returns non-nil when
EFFORT should be offered for that model.")

(defun llm-effort-choices-for-model (model)
  "Return the effort levels offered for MODEL.
The universal `llm-effort-choices' plus any `llm-effort-model-choices'
whose predicate passes for MODEL."
  (append llm-effort-choices
          (cl-loop for (effort . pred) in llm-effort-model-choices
                   when (funcall pred model)
                   collect effort)))

(defcustom llm-effort nil
  "Reasoning effort passed to claude as `--effort'.
Nil means use claude's default.  A string like \"low\", \"medium\",
or \"high\" is passed through verbatim.

Normally toggled per-invocation via the `-e' switch in `llm-menu'
rather than set directly."
  :type '(choice (const :tag "Default (claude picks)" nil)
                 (string :tag "Effort level"))
  :group 'llm)

(cl-defun llm--project-root (&optional (dir default-directory))
  "Find the nearest ancestor of DIR (inclusive) containing any marker
in `llm-project-root-markers'.  Falls back to DIR if none found.

Walks up directory-by-directory, asking \"does any marker exist
here?\" at each level.  This is intentionally different from looping
over markers: looping would let an early marker (e.g. `.git') in a far
ancestor win over a later marker (e.g. `CLAUDE.md') in a much closer
ancestor.

The home directory is never treated as a project root.  Markers there
are global config, not project markers — most importantly `~/.claude',
claude's own config dir, which matches the `.claude' marker.  Without
this guard every markerless directory under HOME would resolve to HOME
and share one `*claude:~*' buffer."
  (let ((home (expand-file-name "~/")))
    (or (when-let ((root (locate-dominating-file
                          dir
                          (lambda (parent)
                            (and (not (file-equal-p parent home))
                                 (cl-some (lambda (marker)
                                            (file-exists-p
                                             (expand-file-name marker parent)))
                                          llm-project-root-markers))))))
          (file-name-as-directory root))
        (file-name-as-directory dir))))

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

- `project': per-project, inside the repo at `.llm/'.
  Committable across machines; `.gitignore' auto-appended to avoid
  dirtying `git status'.
- `user': per-user, per-machine, at `~/.cache/mijn-llm/<repo-id>/'.
  Never touches the repo; each checkout starts empty."
  :type '(choice (const :tag "Per-project (.llm/ in repo)" project)
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
     (expand-file-name (concat ".llm/" subpath) root))))

;;; Claude vterm buffer management

(defvar llm--buffers (make-hash-table :test 'eq)
  "Registry of live claude vterm buffers (used as a set; keys only).")

(defun llm--register-buffer (buf)
  "Register BUF as a claude buffer."
  (puthash buf t llm--buffers))

(defun llm--unregister-buffer (buf)
  "Unregister BUF from the claude buffer registry."
  (remhash buf llm--buffers))

(defun llm--get-buffers ()
  "Return a list of all live claude buffers."
  (cl-remove-if-not #'buffer-live-p (hash-table-keys llm--buffers)))

(defun llm--project-label (directory)
  "Return (LABEL . ROOT) for DIRECTORY.
ROOT comes from `llm--project-root' — the single source of truth for
project roots in this package — and is never nil; LABEL is its final
path component."
  (let ((root (llm--project-root directory)))
    (cons (file-name-nondirectory (directory-file-name root))
          root)))

(defun llm--claude-session-dir (dir)
  "Return the `~/.claude/projects/<encoded>' path for DIR.
Claude encodes a project directory by replacing each `/' and `.'
with `-' (e.g. /home/u/.emacs.d → -home-u--emacs-d)."
  (let ((encoded (replace-regexp-in-string
                  "[/.]" "-"
                  (directory-file-name (expand-file-name dir)))))
    (expand-file-name encoded "~/.claude/projects/")))

(defun llm--claude-has-session-p (dir)
  "Return non-nil if DIR has at least one recorded claude session."
  (let ((sdir (llm--claude-session-dir dir)))
    (and (file-directory-p sdir)
         (directory-files sdir nil "\\.jsonl\\'" t))))

(defun llm--claude-shell-command (_root)
  "Return the claude shell command.
Uses `claude -c' (continue most recent session) when the current
directory has a recorded session, otherwise plain `claude'.
Appends `--model' when `llm-model' is set, `--effort' when `llm-effort'
is set, and `--dangerously-skip-permissions' when
`llm-dangerously-skip-permissions' is non-nil."
  (let* ((base (if (llm--claude-has-session-p default-directory)
                   "claude -c"
                 "claude"))
         (with-model (if llm-model
                         (concat base " --model "
                                 (shell-quote-argument llm-model))
                       base))
         (with-effort (if llm-effort
                          (concat with-model " --effort "
                                  (shell-quote-argument llm-effort))
                        with-model)))
    (if llm-dangerously-skip-permissions
        (concat with-effort " --dangerously-skip-permissions")
      with-effort)))

;;; Show last response in a side buffer
;;
;; Reads the current *claude:* vterm's session JSONL (newest .jsonl in the
;; buffer's project dir, via `llm--claude-session-dir') and renders Claude's
;; most recent assistant turn into a plain-text buffer in another window.
;; Nothing is written to disk.

(defcustom llm-response-render-function #'llm--render-response-plain
  "Function that renders an extracted Claude response for display.
Called with one argument, the raw response STRING (already joined from
the assistant turn's text blocks), in a fresh buffer that is current and
empty.  It should insert the display text and may set the major mode.

The default, `llm--render-response-plain', inserts the text verbatim in
`fundamental-mode'.  This indirection is the single seam for future
rendering: point it at e.g. an `llm--render-response-markdown' that turns
on `gfm-mode'/`markdown-mode' without touching the extraction or display
plumbing."
  :type 'function
  :group 'llm)

(defun llm--session-file (dir)
  "Return the newest session .jsonl for DIR, or nil if none.
DIR is resolved to its claude project dir via `llm--claude-session-dir';
files are ranked by modification time so the session the live claude is
actively appending wins (session filenames are random UUIDs, so name
order is meaningless)."
  (let ((sdir (llm--claude-session-dir dir)))
    (when (file-directory-p sdir)
      (car (sort (directory-files sdir t "\\.jsonl\\'" t)
                 (lambda (a b)
                   (time-less-p (file-attribute-modification-time
                                 (file-attributes b))
                                (file-attribute-modification-time
                                 (file-attributes a)))))))))

(defun llm--session-records (file)
  "Parse FILE (JSONL) into a list of alists, in file order.
Lines that fail to parse as a JSON object are skipped, so a partially
written trailing line (claude still flushing mid-write) can't error."
  (let (records)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position) (line-end-position))))
          (unless (string-blank-p line)
            (let ((obj (ignore-errors
                         (json-parse-string line
                                            :object-type 'alist
                                            :array-type 'list
                                            :null-object nil
                                            :false-object nil))))
              (when (and obj (listp obj)) (push obj records)))))
        (forward-line 1)))
    (nreverse records)))

(defun llm--genuine-user-prompt-p (record)
  "Return non-nil if RECORD is a genuine human prompt (the turn boundary).
A genuine prompt is a type==\"user\" line whose `message.content' is a
STRING (not an array), and which is NOT a tool result, meta/command
expansion, compact summary, or sidechain.  This string-only test is what
correctly skips the array-content lines that masquerade as user turns:
tool_result entries (content is a `tool_result' array), isMeta skill /
command expansions, and synthetic \"[Request interrupted by user]\"
markers — all of which can otherwise be mistaken for the boundary and
truncate the latest turn."
  (and (equal (alist-get 'type record) "user")
       (stringp (alist-get 'content (alist-get 'message record)))
       (not (assq 'toolUseResult record))
       (not (eq t (alist-get 'isMeta record)))
       (not (eq t (alist-get 'isCompactSummary record)))
       (not (eq t (alist-get 'isSidechain record)))))

(defun llm--extract-last-response (records)
  "Return Claude's latest assistant response text from RECORDS, or nil.
The latest turn is every assistant `text' block appearing after the last
genuine human prompt; blocks are joined with blank lines.  `thinking' and
`tool_use' blocks, sidechain assistant lines, and API-error lines are
intentionally dropped — this is the prose reply."
  (let* ((boundary (or (cl-position-if #'llm--genuine-user-prompt-p records
                                       :from-end t)
                       -1))
         (tail (nthcdr (1+ boundary) records))
         texts)
    (dolist (rec tail)
      (when (and (equal (alist-get 'type rec) "assistant")
                 (not (eq t (alist-get 'isSidechain rec)))
                 (not (eq t (alist-get 'isApiErrorMessage rec))))
        (let ((content (alist-get 'content (alist-get 'message rec))))
          (when (listp content)
            (dolist (blk content)
              (when (equal (alist-get 'type blk) "text")
                (let ((txt (alist-get 'text blk)))
                  (when (and (stringp txt) (not (string-blank-p txt)))
                    (push txt texts)))))))))
    (when texts
      (string-trim (string-join (nreverse texts) "\n\n")))))

(defun llm--render-response-plain (text)
  "Default renderer: insert TEXT verbatim as plain text.
Current buffer is fresh and current; leaves it in `fundamental-mode'."
  (fundamental-mode)
  (insert text))

;;;###autoload
(defun llm-show-last-response ()
  "Show the current claude session's latest response in another window.
Must be invoked from a `*claude:PROJECT*' vterm.  Locates that buffer's
session JSONL (newest .jsonl under its project dir) without writing
anything to disk, extracts the most recent assistant turn, and renders it
via `llm-response-render-function' into a reused `*claude-response:LABEL*'
buffer shown in another window, with point at the top."
  (interactive)
  (unless (llm-buffer-p)
    (user-error "Not a claude buffer"))
  (let* ((dir   default-directory)
         (label (car (llm--project-label dir)))
         (file  (llm--session-file dir)))
    (unless file
      (user-error "No claude session found for %s" label))
    (let ((response (llm--extract-last-response (llm--session-records file))))
      (unless response
        (user-error "No assistant response found in latest turn"))
      (let ((buf (get-buffer-create (format "*claude-response:%s*" label))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (when (bound-and-true-p view-mode) (view-mode -1))
            (erase-buffer)
            (funcall llm-response-render-function response)
            (goto-char (point-min))
            (set-buffer-modified-p nil))
          (view-mode 1))            ; q to bury; read-only with cheap nav
        (display-buffer buf '(display-buffer-pop-up-window
                              (inhibit-same-window . t)))))))

;;;###autoload
(defun llm (&optional user-root)
  "Open Claude CLI in a vterm buffer named *claude:project*.
Without prefix: reuse the existing buffer, or create one.
With \\[universal-argument]: new buffer, continue session if possible.
With \\[universal-argument] \\[universal-argument]: new buffer, fresh session."
  (interactive)
  (pcase-let* ((`(,label . ,root)
                (if user-root
                    (cons (file-name-nondirectory
                           (directory-file-name (expand-file-name user-root)))
                          user-root)
                  (llm--project-label default-directory)))
               (default-directory (or user-root root default-directory))
               (base (format "*claude:%s*" label))
               (prefix (prefix-numeric-value current-prefix-arg)))
    (cond ((= prefix 1)
           (let ((existing (get-buffer base)))
             (cond
              ((and existing (buffer-live-p existing)
                    (get-buffer-process existing))
               (pop-to-buffer existing))
              (t
               (when existing (kill-buffer existing))
               (let ((vterm-shell (llm--claude-shell-command root)))
                 (vterm-other-window base)
                 (llm--register-buffer (current-buffer)))))))
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

;;; Claude vterm buffer predicate

(defun llm-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a claude vterm buffer."
  (string-prefix-p "*claude:" (buffer-name (or buf (current-buffer)))))

;; Unregister dead buffers on re-eval.
(when (hash-table-p llm--buffers)
  (maphash (lambda (buf _status)
             (unless (buffer-live-p buf)
               (remhash buf llm--buffers)))
           llm--buffers))

(defun llm--cleanup-buffer ()
  "Unregister the current buffer from the claude buffer list."
  (llm--unregister-buffer (current-buffer)))

(add-hook 'kill-buffer-hook #'llm--cleanup-buffer)

;;; Prompt Mode

(defun llm--prompt-capf ()
  "Completion-at-point for @file references in the prompt buffer.
Offers project-relative file paths when the point follows `@'."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (let* ((at-start (match-beginning 0))
               (start    (1+ at-start))
               (root     (or llm--prompt-project-root (llm--current-root))))
          (when root
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

(defun llm--ensure-ignored (root)
  "Append `.llm/' to ROOT's .gitignore if missing.
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
                                       ".llm/"
                                       (? line-end))
                                   existing))
        (with-temp-buffer
          (when existing (insert existing))
          (unless (or (null existing) (string-suffix-p "\n" existing))
            (insert "\n"))
          (insert ".llm/\n")
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
If ROOT is provided, switch to the claude buffer for that project root."
  (llm--save-prompt prompt root)
  (llm root)
  (vterm-insert prompt)
  (vterm-send-return))

(defun llm--write-context-file (text)
  "Write TEXT to a temporary file and return its path."
  (let ((file (make-temp-file "llm-context-" nil ".txt")))
    (with-temp-file file (insert text))
    file))

(defun llm--present-prompt-buffer (buf)
  "Show BUF in a window for editing, like `org-capture'."
  (pop-to-buffer buf))

;;; Bubble (inline-response) mode

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

(defvar-local llm--bubble-model nil
  "Buffer-local copy of `llm-model' captured at bubble creation.
Frozen at bubble open so toggling the menu's `-m' switch mid-conversation
doesn't retroactively change which model the session uses.")

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
  "Begin the `thinking' animation in BUF at current `point-max'.
The timer cancels itself if BUF is killed, so a promoted/closed bubble
can never strand a repeating timer on a dead buffer."
  (with-current-buffer buf
    (llm--bubble-stop-thinking buf)
    (let* ((pos (point-max))
           (ov  (make-overlay pos pos buf t nil))
           timer)
      (overlay-put ov 'after-string (llm--bubble-thinking-string 0))
      (setq-local llm--bubble-thinking-overlay ov)
      (setq-local llm--bubble-thinking-tick 0)
      (setq timer (run-with-timer
                   0.4 0.4
                   (lambda ()
                     (if (buffer-live-p buf)
                         (llm--bubble-thinking-tick-fn buf)
                       (cancel-timer timer)))))
      (setq-local llm--bubble-thinking-timer timer))))

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
the project directory. Prepends `llm-bubble-prompt-prefix' to PROMPT;
passes `--model' from `llm--bubble-model' when set, and
`--dangerously-skip-permissions' when `llm--bubble-dangerous' is set."
  (let ((text (concat llm-bubble-prompt-prefix prompt)))
    (append (list "claude" "--session-id" llm--bubble-session-id)
            (when llm--bubble-model    (list "--model" llm--bubble-model))
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
         (model     llm--bubble-model)
         (bubble    (current-buffer)))
    (kill-buffer bubble)
    (let ((default-directory dir)
          (vterm-shell (format "claude --resume %s%s%s"
                               (shell-quote-argument sid)
                               (if model
                                   (format " --model %s"
                                           (shell-quote-argument model))
                                 "")
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
          (propertize " Claude bubble  (cancelled — C-c C-k close)"
                      'face 'llm-bubble-header-face)))
   (t
    (let ((buf (current-buffer)))
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
         (root (llm--project-root default-directory))
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
        (setq-local llm--bubble-model llm-model)
        (goto-char (point-max))
        (insert (propertize "— " 'face 'llm-bubble-user-face))
        (setq-local llm--bubble-input-start (copy-marker (point) nil))
        (setq header-line-format
              (propertize
               " Claude  C-c C-c send · C-c C-k close · C-c C-m →claude"
               'face 'llm-bubble-header-face))))
    (llm--present-prompt-buffer buf)))

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
    (llm--present-prompt-buffer buf)))

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
  "Switch to another claude buffer."
  (interactive)
  (let ((bufs (llm--get-buffers)))
    (unless bufs
      (user-error "No claude buffers"))
    (let ((entries (cl-loop for buffer in bufs
                            collect (cons (buffer-name buffer) buffer))))
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

(defun llm--annotation-key (root kind)
  "Return the hash key for ROOT and KIND."
  (cons root kind))

(defun llm--annotation-load (root kind)
  "Load annotations of KIND for ROOT from disk."
  (let ((file (llm--annotation-file root kind)))
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
  "Return non-nil if ENTRY's KIND comment still exists in the file.
The needle matches the `KIND(llm): ' prefix produced by
`llm--annotation-comment', not a bare `KIND: '."
  (let ((file (plist-get entry :file))
        (text (plist-get entry :text)))
    (and (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (let ((needle (concat kind "(llm): " (car (split-string text "\n")))))
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

;; The per-kind annotation commands (add/list/send for FIXME and TODO) are
;; generated from the generic helpers, so the three operations stay in sync
;; across kinds instead of being six hand-maintained wrappers.
(defmacro llm--define-annotation-commands (kind)
  "Define `llm-add/list/send' commands for annotation KIND (a string)."
  (let ((lc (downcase kind)))
    `(progn
       (defun ,(intern (format "llm-add-%s" lc)) ()
         ,(format "Add a %s annotation at point." kind)
         (interactive)
         (llm--add-annotation ,kind))
       (defun ,(intern (format "llm-list-%ss" lc)) ()
         ,(format "List all %ss for the current project." kind)
         (interactive)
         (llm--list-annotations ,kind))
       (defun ,(intern (format "llm-send-%ss" lc)) ()
         ,(format "Send all %ss to Claude." kind)
         (interactive)
         (llm--send-annotations ,kind)))))

(llm--define-annotation-commands "FIXME")
(llm--define-annotation-commands "TODO")

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

(defun llm--menu-use-cwd-p ()
  "Return non-nil if the menu's `-c' switch is active."
  (member "-c" (transient-args 'llm-menu)))

(defun llm--menu-flag (prefix)
  "Return the menu argument value for PREFIX (e.g. \"--model=\"), or nil.
\"default\" is treated as nil so that no flag is passed."
  (let ((val (cl-some (lambda (a)
                        (and (stringp a)
                             (string-prefix-p prefix a)
                             (substring a (length prefix))))
                      (transient-args 'llm-menu))))
    (if (equal val "default") nil val)))

(defun llm--menu-current-model ()
  "The model in effect for `llm-menu': the `-m' override, else `llm-model'.
Guarded so it is safe to call while the transient is live (e.g. from an
infix `:choices' function)."
  (or (ignore-errors (llm--menu-flag "--model="))
      llm-model))

(defun llm--menu-effort ()
  "The effort selected in `llm-menu', filtered by the current model.
Returns nil when no effort is set, or when the set effort is gated to a
model the current selection does not satisfy (e.g. \"ultracode\" with a
non-opus-4.8 model), so an unsupported level is never passed to claude."
  (let ((effort (llm--menu-flag "--effort=")))
    (and effort
         (member effort (llm-effort-choices-for-model (llm--menu-current-model)))
         effort)))

;;;###autoload
(defun llm-set-default-model (model)
  "Set MODEL as the default for new claude sessions and persist it.
Empty input clears the default (claude will pick).  The value is saved
via `customize-save-variable', so it survives Emacs restarts.

Per-invocation overrides via the menu's `-m' switch are unaffected."
  (interactive
   (list (completing-read
          (format "Default model (current: %s, empty = claude picks): "
                  (or llm-model "none"))
          llm-model-choices nil nil nil nil llm-model)))
  (let ((value (if (or (string-empty-p (or model ""))
                       (equal model "default"))
                   nil model)))
    (customize-save-variable 'llm-model value)
    (message "Default model %s"
             (if value (format "set to %s (saved)" value)
               "cleared (claude picks)"))))

(transient-define-suffix llm--menu-prompt-bubble ()
  "Launch the inline-conversation bubble; honors the menu's switches."
  :description "Prompt inline (conversation)"
  (interactive)
  (let ((llm-bubble-prompt-prefix
         (if (member "--btw" (transient-args 'llm-menu))
             "/btw "
           ""))
        (llm-dangerously-skip-permissions
         (or llm-dangerously-skip-permissions (llm--menu-dangerous-p)))
        (llm-model (or (llm--menu-flag "--model=") llm-model))
        (default-directory (if (llm--menu-use-cwd-p)
                               default-directory
                             (or (llm--project-root) default-directory))))
    (llm-prompt-bubble)))

(transient-define-suffix llm--menu-open-claude ()
  "Open the main *claude:PROJECT* vterm; honors the menu's switches."
  :description "Open Claude in project"
  (interactive)
  (let ((llm-dangerously-skip-permissions
         (or llm-dangerously-skip-permissions (llm--menu-dangerous-p)))
        (llm-model (or (llm--menu-flag "--model=") llm-model))
        (llm-effort (or (llm--menu-effort) llm-effort))
        (root (when (llm--menu-use-cwd-p) default-directory))
        (current-prefix-arg nil))
    (llm root)))

(defun llm--menu-model-description ()
  "Description for the model switch showing the current default."
  (format "Model [%s]" (or llm-model "default")))

(defun llm--menu-effort-description ()
  "Description for the effort switch showing the current default."
  (format "Effort [%s]" (or llm-effort "default")))

(transient-define-prefix llm-menu ()
  "Claude CLI commands."
  ["Options"
   ("-b" "Prepend /btw slash-command to inline prompts" "--btw")
   ("-c" "Use current directory (not project root)"     "-c")
   ("-d" "Dangerously skip permission prompts"          "--dangerously-skip-permissions")
   ("-m" llm--menu-model-description                    "--model="
    :choices (lambda () llm-model-choices))
   ("-e" llm--menu-effort-description                   "--effort="
    :choices (lambda ()
               (llm-effort-choices-for-model (llm--menu-current-model))))]
  [["Session"
    ("c" llm--menu-open-claude)
    ("v" "Vterm in project"       llm-vterm-here)
    ("b" "Switch buffer"          llm-switch-buffer)
    ("p" "Prompt"                 llm-prompt)
    ("P" llm--menu-prompt-bubble)
    ("r" "Resume last prompt"     llm-prompt-resume)
    ("H" "Prompt history"         llm-prompt-history)
    ("M" "Set default model"      llm-set-default-model)
    ("?" "Describe at point"      llm-describe-at-point)
    ("R" "Show last response"     llm-show-last-response)]
   ["Annotations"
    ("f" "Add FIXME"          llm-add-fixme)
    ("t" "Add TODO"           llm-add-todo)
    ("F" "List FIXMEs"        llm-list-fixmes)
    ("T" "List TODOs"         llm-list-todos)
    ("S" "Send FIXMEs"        llm-send-fixmes)
    ("D" "Send TODOs"         llm-send-todos)
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
      (setq-local llm--bubble-dangerous llm-dangerously-skip-permissions)
      (setq-local llm--bubble-model llm-model))
    (llm--present-prompt-buffer buf)
    (with-current-buffer buf
      (llm--bubble-spawn-turn text))))

;;; Vterm display fixups

(defun llm--vterm-display-fixups ()
  "Neutralize global display settings that corrupt vterm's character grid.
This config sets `line-spacing' globally (see `mijn-ui'); the extra
pixels between rows break the vertical box-drawing borders of TUIs like
interactive `claude', so it is zeroed buffer-locally here.  Symbol
prettification (from `global-prettify-symbols-mode') has no place in a
terminal grid and is likewise disabled.

The terminal dimensions are computed during `vterm-mode' init, before
this hook fires, so they reflect the old `line-spacing'.  A deferred
resize corrects the mismatch once the buffer is displayed."
  (setq-local line-spacing 0)
  (when (bound-and-true-p prettify-symbols-mode)
    (prettify-symbols-mode -1))
  (let ((buf (current-buffer)))
    (run-with-timer 0 nil
      (lambda ()
        (when (and (buffer-live-p buf)
                   (get-buffer-window buf))
          (window--adjust-process-windows))))))

;;; Vterm copy helper (for TUIs that redraw and stomp on selections)

(defvar-local llm--vterm-copy-resume nil
  "Non-nil when leaving `vterm-copy-mode' should resume a process this
buffer suspended via `llm-vterm-copy'.")

(defun llm--vterm-copy-resume-on-exit ()
  "Send `fg' when copy-mode is disabled in a buffer flagged for resume."
  (when (and llm--vterm-copy-resume (not vterm-copy-mode))
    (process-send-string vterm--process "fg\n")
    (setq-local llm--vterm-copy-resume nil)))

(defun llm-vterm-copy ()
  "Suspend the foreground vterm process and enter `vterm-copy-mode'.
Resumes the process automatically when copy-mode is exited (q / C-c C-t).

Useful for copying from TUIs (e.g. interactive `claude') that
continuously redraw and overwrite selections.  No-op outside vterm."
  (interactive)
  ;; (unless (derived-mode-p 'vterm-mode)
  ;;   (user-error "Not a vterm buffer"))
  ;; (process-send-string vterm--process "\C-z")
  ;; (setq-local llm--vterm-copy-resume t)
  ;; (vterm-copy-mode 1)
  )

;;; Vterm auto-freeze: scroll up to read history, type or q to resume
;;
;; vterm forces buffer point to the terminal cursor (bottom) on every redraw
;; (vterm-module.c term_redraw -> adjust_topline, unconditional), so a
;; streaming TUI like `claude' makes scrollback unreadable.  There is NO
;; public "disable follow" knob; the only sanctioned freeze is
;; `vterm-copy-mode', which sends XOFF (`<stop>' -> tcflow TCOOFF) so the
;; child PAUSES and no further output arrives to trigger the redraw.  We do
;; NOT pin window geometry around the filter (that caused the torn frames
;; that were removed); we only OBSERVE point in `post-command-hook' and
;; toggle the documented `vterm-copy-mode'.  Exiting it sends XON and snaps
;; point back to the live cursor, resuming following.
;;
;; `window-scroll-functions' is deliberately NOT used: it runs during
;; redisplay, where toggling a minor mode / writing to the pty is unsafe.
;; Every user scroll (C-v, M-v) and point move is a command, so
;; `post-command-hook' catches them without redisplay re-entry.

(defvar vterm--term)
(declare-function vterm-copy-mode "vterm" (&optional arg))
(declare-function vterm-reset-cursor-point "vterm" ())

(defcustom llm-vterm-autofreeze-buffers #'llm-buffer-p
  "Predicate deciding whether auto-freeze is active in a vterm buffer.
Called with no args in the vterm buffer; non-nil enables the behavior.
Defaults to claude buffers only, so plain `*vterm:…*' shells are untouched."
  :type 'function
  :group 'llm)

(defvar-local llm--vterm-autofreeze-armed nil
  "Guard so our own copy-mode toggles don't re-trigger the observer.")

(defvar-local llm--vterm-autofreeze-active nil
  "Non-nil when copy-mode in this buffer was entered BY auto-freeze.
As opposed to the manual `llm-vterm-copy' / `C-c C-y' workflow or a bare
`vterm-copy-mode'.  Scopes the resume overlay map and the header-line so
manual copy-mode keeps vanilla behavior.")

(defun llm--vterm-autofreeze-p ()
  "Non-nil if auto-freeze should manage the current buffer."
  (and (derived-mode-p 'vterm-mode)
       (boundp 'vterm--term) vterm--term
       (ignore-errors (funcall llm-vterm-autofreeze-buffers))))

(defun llm--vterm-at-bottom-p ()
  "Non-nil if point in the selected window is on the buffer's last line.
That last line is the live prompt/cursor row vterm keeps pinned, so being
there means we are following; being above it means the user scrolled up."
  (>= (point)
      (save-excursion (goto-char (point-max))
                      (line-beginning-position))))

(defun llm--vterm-freeze ()
  "Enter copy-mode to freeze the view (XOFF pauses claude).  Idempotent.
Sets `llm--vterm-autofreeze-active' so only our managed freeze gets the
resume overlay map + header-line, and leaves `llm--vterm-copy-resume' nil
so exiting sends only XON, never the `fg' of the C-c C-y suspend path."
  ;; (unless (or (bound-and-true-p vterm-copy-mode)
  ;;             llm--vterm-autofreeze-armed)
  ;;   (let ((llm--vterm-autofreeze-armed t))
  ;;     (setq llm--vterm-autofreeze-active t)
  ;;     (vterm-copy-mode 1)
  ;;     (setq header-line-format
  ;;           " VTerm frozen — scroll to read · q quits · any key resumes")))
  )

(defun llm--vterm-resume ()
  "Exit copy-mode and snap point to the live cursor so output follows again.
Safe to call when not frozen (no-op)."
  (when (bound-and-true-p vterm-copy-mode)
    (let ((llm--vterm-autofreeze-armed t))
      (vterm-copy-mode -1)        ; sends XON, restores vterm-mode-map
      (vterm-reset-cursor-point)  ; point -> live terminal cursor (bottom)
      (dolist (w (get-buffer-window-list (current-buffer) nil t))
        (set-window-point w (point))))))

(defun llm--vterm-autofreeze-post-command ()
  "`post-command-hook': freeze when point has moved up off the live line.
Only fires from the command loop — never from vterm's redraw timer — so
plain streaming with no keypress does NOT freeze; the user must actively
scroll or move point up.  Skips mid-toggle calls via the armed guard."
  (when (and (not llm--vterm-autofreeze-armed)
             (not (bound-and-true-p vterm-copy-mode))
             (llm--vterm-autofreeze-p)
             (not (llm--vterm-at-bottom-p)))
    (llm--vterm-freeze)))

(defun llm--vterm-resume-and-resend ()
  "Thaw, then replay the triggering key into the live terminal so it is
not lost.  Bound to self-inserting keys while auto-frozen."
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (llm--vterm-resume)
    (setq unread-command-events
          (append (listify-key-sequence keys) unread-command-events))))

(defun llm--vterm-resume-only ()
  "Thaw without resending the key (viewer-style quit on `q')."
  (interactive)
  (llm--vterm-resume))

(defvar llm--vterm-autofreeze-resume-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'llm--vterm-resume-and-resend)
    (dolist (k '("SPC" "DEL" "TAB" "RET" "<return>"))
      (define-key map (kbd k) #'llm--vterm-resume-and-resend))
    (define-key map (kbd "q") #'llm--vterm-resume-only)
    map)
  "Overlay keymap active while a claude vterm is auto-frozen.
Layered above `vterm-copy-mode-map' so it adds resume keys without
overriding copy-mode's bindings.  Note: this rebinds RET to
resume-and-send-newline; drop the RET/<return> entries if you prefer
RET to copy the line (`vterm-copy-mode-done').
Deliberately does NOT bind C-d (EOF) or C-c C-c (SIGINT), so a stray key
while reading can never end or interrupt claude.")

(defun llm--vterm-autofreeze-copy-hook ()
  "Run on `vterm-copy-mode-hook' (fires on BOTH enable and disable).
On enable of an auto-freeze-initiated copy-mode, install the resume
overlay map.  On disable, clear our header-line and flag.  Manual
`vterm-copy-mode' / `C-c C-y' sessions (active flag nil) stay vanilla."
  (when (llm--vterm-autofreeze-p)
    (cond
     ((and (bound-and-true-p vterm-copy-mode)
           llm--vterm-autofreeze-active)
      (set-transient-map llm--vterm-autofreeze-resume-map
                         (lambda () (bound-and-true-p vterm-copy-mode))))
     ((not (bound-and-true-p vterm-copy-mode))
      (when llm--vterm-autofreeze-active
        (setq llm--vterm-autofreeze-active nil)
        (setq header-line-format nil))))))

(defun llm--vterm-autofreeze-setup ()
  "Arm the auto-freeze observer buffer-locally for a managed vterm buffer."
  (when (llm--vterm-autofreeze-p)
    (add-hook 'post-command-hook
              #'llm--vterm-autofreeze-post-command nil t)))

(with-eval-after-load 'vterm
  (add-hook 'vterm-mode-hook #'llm--vterm-display-fixups)
  ;; (add-hook 'vterm-mode-hook #'llm--vterm-autofreeze-setup)
  ;; (add-hook 'vterm-copy-mode-hook #'llm--vterm-copy-resume-on-exit)
  ;; (add-hook 'vterm-copy-mode-hook #'llm--vterm-autofreeze-copy-hook)
  ;; (define-key vterm-mode-map (kbd "C-c C-y") #'llm-vterm-copy)
  (define-key vterm-mode-map (kbd "C-c C-r") #'llm-show-last-response))

;;; Keybindings

(global-set-key (kbd "C-x y e") #'llm-menu)
(global-set-key (kbd "C-S-j")   #'llm-next-buffer)
(global-set-key (kbd "C-S-k")   #'llm-previous-buffer)
(global-set-key (kbd "C-x C-x") #'llm-toggle-vterm-claude)

(provide 'mijn-llm)
;;; mijn-llm.el ends here
