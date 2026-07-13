;;; agnostic-llm.el --- Agentic CLI integration -*- lexical-binding: t; -*-

;; Copyright (c) 2026 Dmitry Akatov
;; Author: Dmitry Akatov <dmitry.akatov@protonmail.com>
;; URL: https://github.com/rails-to-cosmos/agnostic-llm
;; Package-Version: 20260713.1033
;; Package-Revision: a5d9d4451502
;; Package-Requires: ((emacs "28.1") (transient "0.4.0") (vterm "0.0.2"))
;; Keywords: convenience, tools

;;; Commentary:
;;
;; Drive the `claude' CLI (https://docs.anthropic.com/en/docs/claude-cli)
;; from Emacs.  Each project gets a dedicated `*claude:PROJECT*' vterm; a
;; multi-line prompt buffer with @file completion feeds it, and a throwaway
;; "bubble" streams a `claude -p' reply inline without disturbing the main
;; session.  `agnostic-llm-show-last-response' renders the latest assistant
;; turn from the session JSONL into a read-only buffer.
;;
;; A transient menu (`agnostic-llm-menu') gathers the commands and exposes
;; per-invocation switches for model, reasoning effort, and
;; `--dangerously-skip-permissions'.  A FIXME/TODO annotation system records
;; notes at point, persists them (per-project under `.agnostic-llm/' or
;; per-user under `~/.cache/agnostic-llm/'), lists them, and can hand them
;; back to Claude to resolve.
;;
;; Requires the `claude' CLI on PATH.  Binds no global keys itself — bind the
;; entry points (`agnostic-llm', `agnostic-llm-menu', `agnostic-llm-prompt',
;; ...) from your own configuration.

;;; Code:

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

;;; Customization

(defgroup agnostic-llm nil
  "Claude CLI integration for Emacs."
  :group 'tools
  :prefix "agnostic-llm-")

;;; Project Root Detection

(defcustom agnostic-llm-project-root-markers '(".git" ".claude" "CLAUDE.md")
  "Files/dirs that indicate a project root."
  :type '(repeat string)
  :group 'agnostic-llm)

(defcustom agnostic-llm-dangerously-skip-permissions nil
  "When non-nil, pass `--dangerously-skip-permissions' to claude.
Applies to the main `*claude:PROJECT*' vterm and to the inline
bubble (captured once at bubble creation as buffer-local state).

Normally toggled per-invocation via the `-d' switch in `agnostic-llm-menu'
rather than set directly.  Beware: with this flag Claude skips all
tool-use confirmation prompts."
  :type 'boolean
  :group 'agnostic-llm)

(defcustom agnostic-llm-models
  '(("claude-fable-5"   . (:efforts ("default" "low" "medium" "high" "max" "ultracode")))
    ("claude-sonnet-5"  . (:efforts ("default" "low" "medium" "high" "max" "ultracode")))
    ("claude-opus-4-8"  . (:efforts ("default" "low" "medium" "high" "max" "ultracode")))
    ("claude-opus-4-7")
    ("claude-opus-4-6")
    ("claude-sonnet-4-6")
    ("claude-haiku-4-5"))
  "Claude models known to `agnostic-llm-menu', newest first.
Each entry is (NAME . PLIST).  NAME is a full model name; the CLI also
accepts aliases like \"opus\", \"sonnet\", \"haiku\".  PLIST currently
supports one key, `:efforts' — the full list of reasoning-effort levels
that model offers.  When absent, the model uses the baseline
`agnostic-llm-effort-choices'.  A full override (rather than an extras
list) lets a model also lack a baseline level.  Remaining plist keys are
reserved for future per-model properties.

Order matters: entries are newest-first, and the first entry stands in
for claude's own default model (used when `agnostic-llm-model' is nil).

This is the place to teach the package about new models and their
effort levels as they ship."
  :type '(alist :key-type (string :tag "Model")
                :value-type (plist :options ((:efforts (repeat string)))))
  :group 'agnostic-llm)

(defun agnostic-llm-model-choices ()
  "Return the model names offered by `agnostic-llm-menu' under the `-m' switch.
The names come from `agnostic-llm-models', in table order."
  (mapcar #'car agnostic-llm-models))

(defcustom agnostic-llm-model nil
  "Model passed to claude as `--model'.
Nil means use claude's default (whatever its config picks).  A string
is passed through verbatim — full names like \"claude-opus-4-7\" or
aliases like \"opus\" both work.

Normally toggled per-invocation via the `-m' switch in `agnostic-llm-menu'
rather than set directly.  Applies to the main `*claude:PROJECT*'
vterm and to the inline bubble (captured once at bubble creation)."
  :type '(choice (const :tag "Default (claude picks)" nil)
                 (string :tag "Model name"))
  :group 'agnostic-llm)

;;; Model resolution (name -> `agnostic-llm-models' entry)
;;
;; A model name may be a full id ("claude-opus-4-8"), a date-suffixed id
;; ("claude-haiku-4-5-20251001"), or a bare family alias ("opus").
;; `agnostic-llm--model-split' breaks a name into (FAMILY NUMS), and
;; `agnostic-llm--version<' orders version lists so a bare alias resolves
;; to its family's newest entry.  `agnostic-llm--model-spec' ties these
;; together, returning the declarative `agnostic-llm-models' entry that
;; governs a model's effort levels.

(defun agnostic-llm--model-split (model)
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

(defun agnostic-llm--version< (a b)
  "Non-nil when version list A precedes B (lexicographic on integers).
A missing component sorts before a present one, so (4) precedes (4 8)."
  (cond ((null a) (and b t))
        ((null b) nil)
        ((= (car a) (car b)) (agnostic-llm--version< (cdr a) (cdr b)))
        (t (< (car a) (car b)))))

(defun agnostic-llm--model-spec (model)
  "Return the `agnostic-llm-models' entry for MODEL, or nil when unknown.
Nil MODEL resolves to the first entry (claude's default model).  A full
name matches by `assoc'; a date-suffixed id (e.g.
\"claude-haiku-4-5-20251001\") matches the entry with the same family and
version; a bare alias (e.g. \"opus\") matches that family's newest entry
ordered by `agnostic-llm--version<'."
  (if (null model)
      (car agnostic-llm-models)
    (or (assoc model agnostic-llm-models)
        (cl-destructuring-bind (family nums) (agnostic-llm--model-split model)
          (cond
           ((null family) nil)
           (nums
            (seq-find (lambda (entry)
                        (equal (agnostic-llm--model-split (car entry))
                               (list family nums)))
                      agnostic-llm-models))
           (t
            (let (best best-nums)
              (dolist (entry agnostic-llm-models best)
                (cl-destructuring-bind (fam enums)
                    (agnostic-llm--model-split (car entry))
                  (when (and (equal fam family) enums
                             (or (null best-nums)
                                 (agnostic-llm--version< best-nums enums)))
                    (setq best entry best-nums enums)))))))))))

;;; Reasoning effort

(defconst agnostic-llm-effort-choices
  '("default"
    "low"
    "medium"
    "high"
    "max")
  "Baseline reasoning-effort levels offered for every model.
A model may widen or narrow this set through its `:efforts' entry in
`agnostic-llm-models'; `agnostic-llm-effort-choices-for-model' returns the
levels actually offered for a given model.")

(defun agnostic-llm-effort-choices-for-model (model)
  "Return the reasoning-effort levels offered for MODEL.
Uses the `:efforts' list from MODEL's `agnostic-llm-models' entry, falling
back to the baseline `agnostic-llm-effort-choices' when MODEL is unknown
or declares no efforts."
  (or (plist-get (cdr (agnostic-llm--model-spec model)) :efforts)
      agnostic-llm-effort-choices))

(defcustom agnostic-llm-effort nil
  "Reasoning effort passed to claude as `--effort'.
Nil means use claude's default.  A string like \"low\", \"medium\",
or \"high\" is passed through verbatim.

Normally toggled per-invocation via the `-e' switch in `agnostic-llm-menu'
rather than set directly."
  :type '(choice (const :tag "Default (claude picks)" nil)
                 (string :tag "Effort level"))
  :group 'agnostic-llm)

(cl-defun agnostic-llm--project-root (&optional (dir default-directory))
  "Return the project root at or above DIR.
This is the nearest ancestor of DIR (inclusive) containing any marker in
`agnostic-llm-project-root-markers'.  Falls back to DIR if none found.

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
                                          agnostic-llm-project-root-markers))))))
          (file-name-as-directory root))
        (file-name-as-directory dir))))

(defvar-local agnostic-llm--prompt-project-root nil
  "Project root captured when the prompt buffer was opened.")

(defvar-local agnostic-llm--prompt-context-prefix nil
  "Auto-generated file/region context header.
Prepended to the user's prompt at send-time but kept out of the
visible buffer so the composition area stays clean.")

(defun agnostic-llm--current-root ()
  "Get the project root for the current context."
  (or agnostic-llm--prompt-project-root
      (agnostic-llm--project-root)))

;;; Persistence Location

(defcustom agnostic-llm-persistence-strategy 'project
  "Where to store prompt history and annotations.

- `project': per-project, inside the repo at `.agnostic-llm/'.
  Committable across machines; `.gitignore' auto-appended to avoid
  dirtying `git status'.
- `user': per-user, per-machine, at `~/.cache/agnostic-llm/<repo-id>/'.
  Never touches the repo; each checkout starts empty."
  :type '(choice (const :tag "Per-project (.agnostic-llm/ in repo)" project)
                 (const :tag "Per-user (~/.cache/agnostic-llm/)"    user))
  :group 'agnostic-llm)

(defcustom agnostic-llm-user-cache-dir
  (expand-file-name "agnostic-llm"
                    (or (getenv "XDG_CACHE_HOME")
                        (expand-file-name ".cache"
                                          (or (getenv "HOME") "~"))))
  "Root directory for per-user persistence.
Used when `agnostic-llm-persistence-strategy' is `user'."
  :type 'directory
  :group 'agnostic-llm)

(defun agnostic-llm--project-cache-subdir (root)
  "Human-readable directory name for ROOT under `agnostic-llm-user-cache-dir'.
Uses the abbreviated absolute path with `/' replaced by `!'."
  (let ((abbrev (abbreviate-file-name (directory-file-name (expand-file-name root)))))
    (replace-regexp-in-string "/" "!" abbrev)))

(defun agnostic-llm--persistence-dir (root subpath)
  "Return the absolute path of SUBPATH under ROOT.
Honors `agnostic-llm-persistence-strategy'.  SUBPATH is relative (e.g.
\"prompts\" or \"fixme.el\")."
  (pcase agnostic-llm-persistence-strategy
    ('user
     (expand-file-name
      subpath
      (expand-file-name (agnostic-llm--project-cache-subdir root)
                        agnostic-llm-user-cache-dir)))
    (_
     (expand-file-name (concat ".agnostic-llm/" subpath) root))))

;;; Claude vterm buffer management

(defvar agnostic-llm--buffers (make-hash-table :test 'eq)
  "Registry of live claude vterm buffers (used as a set; keys only).")

(defun agnostic-llm--register-buffer (buf)
  "Register BUF as a claude buffer."
  (puthash buf t agnostic-llm--buffers))

(defun agnostic-llm--unregister-buffer (buf)
  "Unregister BUF from the claude buffer registry."
  (remhash buf agnostic-llm--buffers))

(defun agnostic-llm--get-buffers ()
  "Return a list of all live claude buffers."
  (cl-remove-if-not #'buffer-live-p (hash-table-keys agnostic-llm--buffers)))

(defun agnostic-llm--project-label (directory)
  "Return (LABEL . ROOT) for DIRECTORY.
ROOT comes from `agnostic-llm--project-root' — the single source of truth for
project roots in this package — and is never nil; LABEL is its final
path component."
  (let ((root (agnostic-llm--project-root directory)))
    (cons (file-name-nondirectory (directory-file-name root))
          root)))

(defun agnostic-llm--claude-session-dir (dir)
  "Return the `~/.claude/projects/<encoded>' path for DIR.
Claude encodes a project directory by replacing each `/' and `.'
with `-' (e.g. /home/u/.config → -home-u--config)."
  (let ((encoded (replace-regexp-in-string
                  "[/.]" "-"
                  (directory-file-name (expand-file-name dir)))))
    (expand-file-name encoded "~/.claude/projects/")))

(defun agnostic-llm--claude-has-session-p (dir)
  "Return non-nil if DIR has at least one recorded claude session."
  (let ((sdir (agnostic-llm--claude-session-dir dir)))
    (and (file-directory-p sdir)
         (directory-files sdir nil "\\.jsonl\\'" t))))

(defun agnostic-llm--claude-shell-command (_root)
  "Return the claude shell command.
Uses `claude -c' (continue most recent session) when the current
directory has a recorded session, otherwise plain `claude'.
Appends `--model' when `agnostic-llm-model' is set, `--effort' when
`agnostic-llm-effort' is set, and `--dangerously-skip-permissions' when
`agnostic-llm-dangerously-skip-permissions' is non-nil."
  (let* ((base (if (agnostic-llm--claude-has-session-p default-directory)
                   "claude -c"
                 "claude"))
         (with-model (if agnostic-llm-model
                         (concat base " --model "
                                 (shell-quote-argument agnostic-llm-model))
                       base))
         (with-effort (if agnostic-llm-effort
                          (concat with-model " --effort "
                                  (shell-quote-argument agnostic-llm-effort))
                        with-model)))
    (if agnostic-llm-dangerously-skip-permissions
        (concat with-effort " --dangerously-skip-permissions")
      with-effort)))

;;; Show last response in a side buffer
;;
;; Reads the current *claude:* vterm's session JSONL (newest .jsonl in the
;; buffer's project dir, via `agnostic-llm--claude-session-dir') and renders Claude's
;; most recent assistant turn into a plain-text buffer in another window.
;; Nothing is written to disk.

(defcustom agnostic-llm-response-render-function #'agnostic-llm--render-response-plain
  "Function that renders an extracted Claude response for display.
Called with one argument, the raw response STRING (already joined from
the assistant turn's text blocks), in a fresh buffer that is current and
empty.  It should insert the display text and may set the major mode.

The default, `agnostic-llm--render-response-plain', inserts the text verbatim in
`fundamental-mode'.  This indirection is the single seam for future
rendering: point it at e.g. an `agnostic-llm--render-response-markdown'
that turns on `gfm-mode'/`markdown-mode' without touching the extraction
or display plumbing."
  :type 'function
  :group 'agnostic-llm)

(defun agnostic-llm--session-file (dir)
  "Return the newest session .jsonl for DIR, or nil if none.
DIR is resolved to its claude project dir via
`agnostic-llm--claude-session-dir'; files are ranked by modification time
so the session the live claude is actively appending wins (session
filenames are random UUIDs, so name order is meaningless)."
  (let ((sdir (agnostic-llm--claude-session-dir dir)))
    (when (file-directory-p sdir)
      (car (sort (directory-files sdir t "\\.jsonl\\'" t)
                 (lambda (a b)
                   (time-less-p (file-attribute-modification-time
                                 (file-attributes b))
                                (file-attribute-modification-time
                                 (file-attributes a)))))))))

(defun agnostic-llm--session-records (file)
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

(defun agnostic-llm--genuine-user-prompt-p (record)
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

(defun agnostic-llm--extract-last-response (records)
  "Return Claude's latest assistant response text from RECORDS, or nil.
The latest turn is every assistant `text' block appearing after the last
genuine human prompt; blocks are joined with blank lines.  `thinking' and
`tool_use' blocks, sidechain assistant lines, and API-error lines are
intentionally dropped — this is the prose reply."
  (let* ((boundary (or (cl-position-if #'agnostic-llm--genuine-user-prompt-p records
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

(defun agnostic-llm--render-response-plain (text)
  "Default renderer: insert TEXT verbatim as plain text.
Current buffer is fresh and current; leaves it in `fundamental-mode'."
  (fundamental-mode)
  (insert text))

;;;###autoload
(defun agnostic-llm-show-last-response ()
  "Show the current claude session's latest response in another window.
Must be invoked from a `*claude:PROJECT*' vterm.  Locates that buffer's
session JSONL (newest .jsonl under its project dir) without writing
anything to disk, extracts the most recent assistant turn, and renders it
via `agnostic-llm-response-render-function' into a reused
`*claude-response:LABEL*' buffer shown in another window, with point at
the top."
  (interactive)
  (unless (agnostic-llm-buffer-p)
    (user-error "Not a claude buffer"))
  (let* ((dir   default-directory)
         (label (car (agnostic-llm--project-label dir)))
         (file  (agnostic-llm--session-file dir)))
    (unless file
      (user-error "No claude session found for %s" label))
    (let ((response (agnostic-llm--extract-last-response (agnostic-llm--session-records file))))
      (unless response
        (user-error "No assistant response found in latest turn"))
      (let ((buf (get-buffer-create (format "*claude-response:%s*" label))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (when (bound-and-true-p view-mode) (view-mode -1))
            (erase-buffer)
            (funcall agnostic-llm-response-render-function response)
            (goto-char (point-min))
            (set-buffer-modified-p nil))
          (view-mode 1))            ; q to bury; read-only with cheap nav
        (display-buffer buf '(display-buffer-pop-up-window
                              (inhibit-same-window . t)))))))

;;;###autoload
(defun agnostic-llm (&optional user-root)
  "Open Claude CLI in a vterm buffer named *claude:project*.
With non-nil USER-ROOT, target that directory instead of the current
project.
Without prefix: reuse the existing buffer, or create one.
With \\[universal-argument]: new buffer, continue session if possible.
With \\[universal-argument] \\[universal-argument]: new buffer, fresh session."
  (interactive)
  (pcase-let* ((`(,label . ,root)
                (if user-root
                    (cons (file-name-nondirectory
                           (directory-file-name (expand-file-name user-root)))
                          user-root)
                  (agnostic-llm--project-label default-directory)))
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
               (let ((vterm-shell (agnostic-llm--claude-shell-command root)))
                 (vterm-other-window base)
                 (agnostic-llm--register-buffer (current-buffer)))))))
          ((= prefix 4)
           (let ((vterm-shell (agnostic-llm--claude-shell-command root))
                 (name (generate-new-buffer-name base)))
             (vterm-other-window name)
             (agnostic-llm--register-buffer (current-buffer))))
          ((>= prefix 16)
           (let ((vterm-shell "claude")
                 (name (generate-new-buffer-name base)))
             (vterm-other-window name)
             (agnostic-llm--register-buffer (current-buffer)))))))

;;;###autoload
(defun agnostic-llm-vterm-here ()
  "Open or switch to a vterm buffer in the current window.
Without prefix: switch to the last *vterm:LABEL* buffer for the project,
or create one if none exist.
With prefix: always create a new vterm buffer."
  (interactive)
  (pcase-let* ((`(,label . ,_) (agnostic-llm--project-label default-directory))
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

(defun agnostic-llm-buffer-p (&optional buf)
  "Return non-nil if BUF (default: current buffer) is a claude vterm buffer."
  (string-prefix-p "*claude:" (buffer-name (or buf (current-buffer)))))

;; Unregister dead buffers on re-eval.
(when (hash-table-p agnostic-llm--buffers)
  (maphash (lambda (buf _status)
             (unless (buffer-live-p buf)
               (remhash buf agnostic-llm--buffers)))
           agnostic-llm--buffers))

(defun agnostic-llm--cleanup-buffer ()
  "Unregister the current buffer from the claude buffer list."
  (agnostic-llm--unregister-buffer (current-buffer)))

(add-hook 'kill-buffer-hook #'agnostic-llm--cleanup-buffer)

;;; Prompt Mode

(defun agnostic-llm--prompt-capf ()
  "Completion-at-point for @file references in the prompt buffer.
Offers project-relative file paths when the point follows `@'."
  (save-excursion
    (let ((end (point)))
      (when (re-search-backward "@\\([^ \t\n]*\\)" (line-beginning-position) t)
        (let* ((at-start (match-beginning 0))
               (start    (1+ at-start))
               (root     (or agnostic-llm--prompt-project-root (agnostic-llm--current-root))))
          (when root
            (list start end
                  (completion-table-dynamic
                   (lambda (_)
                     (when-let ((proj (project-current nil root)))
                       (mapcar (lambda (f) (file-relative-name f root))
                               (project-files proj)))))
                  :exclusive 'no
                  :annotation-function (lambda (_) " file"))))))))

(define-derived-mode agnostic-llm-prompt-mode text-mode "Agnostic-LLM"
  "Major mode for composing multi-line Claude prompts.
\\<agnostic-llm-prompt-mode-map>\\[agnostic-llm-prompt-send] to send, \\[agnostic-llm-prompt-cancel] to cancel."
  (setq header-line-format " Claude  C-c C-c send | C-c C-k cancel")
  (add-hook 'completion-at-point-functions #'agnostic-llm--prompt-capf nil t))

(define-key agnostic-llm-prompt-mode-map (kbd "C-c C-c") #'agnostic-llm-prompt-send)
(define-key agnostic-llm-prompt-mode-map (kbd "C-c C-k") #'agnostic-llm-prompt-cancel)
(define-key agnostic-llm-prompt-mode-map (kbd "C-c C-m") #'agnostic-llm--bubble-promote)

;;; Interactive Commands

(defun agnostic-llm--ensure-ignored (root)
  "Append `.agnostic-llm/' to ROOT's .gitignore if missing.
No-op unless `agnostic-llm-persistence-strategy' is `project' and ROOT
is a git repo."
  (when (and (eq agnostic-llm-persistence-strategy 'project)
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
                                       ".agnostic-llm/"
                                       (? line-end))
                                   existing))
        (with-temp-buffer
          (when existing (insert existing))
          (unless (or (null existing) (string-suffix-p "\n" existing))
            (insert "\n"))
          (insert ".agnostic-llm/\n")
          (write-region (point-min) (point-max) gitignore))))))

(defun agnostic-llm--save-prompt (prompt root)
  "Save PROMPT under the persistence location for ROOT as a timestamped file."
  (let* ((r (or root (agnostic-llm--current-root)))
         (dir (agnostic-llm--persistence-dir r "prompts"))
         (file (expand-file-name
                (format "%s.txt" (format-time-string "%Y%m%d-%H%M%S"))
                dir)))
    (make-directory dir t)
    (agnostic-llm--ensure-ignored r)
    (with-temp-file file (insert prompt))))

(defun agnostic-llm--send-to-claude (prompt &optional root)
  "Switch to the claude vterm buffer and insert PROMPT.
If ROOT is provided, switch to the claude buffer for that project root."
  (agnostic-llm--save-prompt prompt root)
  (agnostic-llm root)
  (vterm-insert prompt)
  (vterm-send-return))

(defun agnostic-llm--write-context-file (text)
  "Write TEXT to a temporary file and return its path."
  (let ((file (make-temp-file "agnostic-llm-context-" nil ".txt")))
    (with-temp-file file (insert text))
    file))

(defun agnostic-llm--present-prompt-buffer (buf)
  "Show BUF in a window for editing, like `org-capture'."
  (pop-to-buffer buf))

;;; Bubble (inline-response) mode

(defcustom agnostic-llm-bubble-prompt-prefix ""
  "String prepended to the user's text when sending in bubble mode.
Empty by default — the bubble sends your prompt verbatim through
`claude -p', which works in any environment.

Set to \"/btw \" (trailing space) if you've defined a matching
custom slash command at `~/.claude/commands/btw.md' or at
`<project>/.claude/commands/btw.md'.  Any other string works too —
e.g. \"By the way, briefly: \" as a plain-text framing preamble."
  :type 'string
  :group 'agnostic-llm)

(defface agnostic-llm-bubble-header-face
  '((t :inherit header-line :slant italic))
  "Face for the bubble header line.")

(defface agnostic-llm-bubble-user-face
  '((((background dark))  :foreground "#7aa2f7" :weight bold)
    (((background light)) :foreground "#5c7cfa" :weight bold))
  "Face for the \"▸\" turn marker in front of user messages.")

(defface agnostic-llm-bubble-thinking-face
  '((t :inherit shadow :slant italic))
  "Face for the animated `Thinking…' indicator.")

(defvar-local agnostic-llm--prompt-bubble nil
  "Non-nil when this prompt buffer is in bubble (inline-response) mode.")

(defvar-local agnostic-llm--bubble-process nil
  "Async `claude -p' process for a bubble, if running.")

(defvar-local agnostic-llm--bubble-last-prompt nil
  "The most recent user prompt sent in this bubble.
Used by `agnostic-llm--bubble-promote' to forward it to the main claude session.")

(defvar-local agnostic-llm--bubble-input-start nil
  "Marker at the start of the user's current input region.
Nil on the very first send (no conversation history yet); a live marker
once the first reply has settled and subsequent turns are being typed.")

(defvar-local agnostic-llm--bubble-session-id nil
  "UUID pinning every turn of this bubble to the same claude session.
Generated lazily on bubble creation; used with `--session-id' on every
`claude -p' invocation and with `--resume' when promoting to a
full `*claude:PROJECT*' vterm.")

(defvar-local agnostic-llm--bubble-model nil
  "Buffer-local copy of `agnostic-llm-model' captured at bubble creation.
Frozen at bubble open so toggling the menu's `-m' switch mid-conversation
doesn't retroactively change which model the session uses.")

(defvar-local agnostic-llm--bubble-dangerous nil
  "Buffer-local copy of `agnostic-llm-dangerously-skip-permissions'.
Captured at bubble creation.  Frozen at bubble open so toggling the
transient mid-conversation doesn't retroactively change the session's
permission posture.")

(defvar-local agnostic-llm--bubble-thinking-overlay nil
  "Overlay showing the animated `...' indicator while Claude is thinking.")

(defvar-local agnostic-llm--bubble-thinking-timer nil
  "Buffer-local timer animating `agnostic-llm--bubble-thinking-overlay'.")

(defvar-local agnostic-llm--bubble-thinking-tick 0
  "Counter driving the thinking-dots animation.")

(defun agnostic-llm--bubble-thinking-string (tick)
  "Return the animated dots string for TICK (1–3 dots)."
  (propertize (make-string (1+ (mod tick 3)) ?.)
              'face 'agnostic-llm-bubble-thinking-face))

(defun agnostic-llm--bubble-thinking-tick-fn (buf)
  "Tick BUF's thinking animation one frame forward."
  (when (and (buffer-live-p buf)
             (overlayp (buffer-local-value 'agnostic-llm--bubble-thinking-overlay buf)))
    (with-current-buffer buf
      (cl-incf agnostic-llm--bubble-thinking-tick)
      (overlay-put agnostic-llm--bubble-thinking-overlay
                   'after-string
                   (agnostic-llm--bubble-thinking-string agnostic-llm--bubble-thinking-tick)))))

(defun agnostic-llm--bubble-start-thinking (buf)
  "Begin the `thinking' animation in BUF at current `point-max'.
The timer cancels itself if BUF is killed, so a promoted/closed bubble
can never strand a repeating timer on a dead buffer."
  (with-current-buffer buf
    (agnostic-llm--bubble-stop-thinking buf)
    (let* ((pos (point-max))
           (ov  (make-overlay pos pos buf t nil))
           timer)
      (overlay-put ov 'after-string (agnostic-llm--bubble-thinking-string 0))
      (setq-local agnostic-llm--bubble-thinking-overlay ov)
      (setq-local agnostic-llm--bubble-thinking-tick 0)
      (setq timer (run-with-timer
                   0.4 0.4
                   (lambda ()
                     (if (buffer-live-p buf)
                         (agnostic-llm--bubble-thinking-tick-fn buf)
                       (cancel-timer timer)))))
      (setq-local agnostic-llm--bubble-thinking-timer timer))))

(defun agnostic-llm--bubble-stop-thinking (buf)
  "Cancel BUF's thinking animation and remove its indicator."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (when (timerp agnostic-llm--bubble-thinking-timer)
        (cancel-timer agnostic-llm--bubble-thinking-timer))
      (setq-local agnostic-llm--bubble-thinking-timer nil)
      (when (overlayp agnostic-llm--bubble-thinking-overlay)
        (delete-overlay agnostic-llm--bubble-thinking-overlay))
      (setq-local agnostic-llm--bubble-thinking-overlay nil))))

(defun agnostic-llm--generate-uuid ()
  "Return a v4-style UUID string."
  (format "%04x%04x-%04x-%04x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536)
          (logior #x4000 (logand (random 65536) #x0fff))
          (logior #x8000 (logand (random 65536) #x3fff))
          (random 65536) (random 65536) (random 65536)))

(defun agnostic-llm--bubble-command (prompt)
  "Build the `claude' argv for PROMPT on this bubble's pinned session.
Every turn uses `--session-id' with the same UUID, so claude treats all
popup turns as one conversation regardless of what else is happening in
the project directory.  Prepends `agnostic-llm-bubble-prompt-prefix' to PROMPT;
passes `--model' from `agnostic-llm--bubble-model' when set, and
`--dangerously-skip-permissions' when `agnostic-llm--bubble-dangerous' is set."
  (let ((text (concat agnostic-llm-bubble-prompt-prefix prompt)))
    (append (list "claude" "--session-id" agnostic-llm--bubble-session-id)
            (when agnostic-llm--bubble-model    (list "--model" agnostic-llm--bubble-model))
            (when agnostic-llm--bubble-dangerous '("--dangerously-skip-permissions"))
            (list "-p" text))))

(defun agnostic-llm--bubble-clean-chunk (chunk)
  "Strip CR, ANSI CSI sequences, and OSC sequences from CHUNK.
`ansi-color-apply' handles CSI (colors, cursor); OSC (e.g. title
changes like ESC ] ... BEL) and stray CR are removed by hand."
  (let* ((no-cr  (replace-regexp-in-string "\r" "" chunk))
         (no-osc (replace-regexp-in-string "\e\\][^\a]*\\(?:\a\\|\e\\\\\\)" "" no-cr)))
    (ansi-color-apply no-osc)))

(defun agnostic-llm--bubble-filter (proc chunk)
  "Process filter: clean CHUNK and append to PROC's buffer.
On the first chunk, replaces the `thinking' indicator with the
claude turn marker."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (first-chunk (overlayp agnostic-llm--bubble-thinking-overlay))
            (was-at-end (= (point) (point-max)))
            (cleaned (agnostic-llm--bubble-clean-chunk chunk)))
        (when first-chunk
          (agnostic-llm--bubble-stop-thinking (current-buffer))
          (save-excursion
            (goto-char (point-max))
            (insert (propertize "— " 'face 'agnostic-llm-bubble-user-face))))
        (save-excursion
          (goto-char (point-max))
          (insert cleaned))
        (when (or first-chunk was-at-end)
          (goto-char (point-max)))))))

(defun agnostic-llm--bubble-sentinel (proc _event)
  "Process sentinel for PROC: append a fresh input prompt and hand control back."
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (agnostic-llm--bubble-stop-thinking (current-buffer))
      (setq-local agnostic-llm--bubble-process nil)
      (let ((status (process-status proc))
            (inhibit-read-only t))
        (pcase status
          ('exit
           (goto-char (point-max))
           (insert "\n\n" (propertize "— " 'face 'agnostic-llm-bubble-user-face))
           (setq-local agnostic-llm--bubble-input-start (copy-marker (point) nil))
           (setq header-line-format
                 (propertize
                  " Claude  C-c C-c send · C-c C-k close · C-c C-m →claude"
                  'face 'agnostic-llm-bubble-header-face)))
          ('signal
           (setq header-line-format
                 (propertize " Claude  (cancelled — C-c C-k close)"
                             'face 'agnostic-llm-bubble-header-face))))))))

(defun agnostic-llm--bubble-promote ()
  "Close the bubble and open a `*claude:PROJECT*' vterm on the same session.
The new vterm continues the same session the popup has been driving.

Every popup turn runs with `--session-id <UUID>', so the conversation
is pinned to one specific claude session.  Promote spawns a fresh
interactive claude with `--resume <UUID>' on the same UUID, loading
all prior turns regardless of what else is happening in the directory."
  (interactive)
  (unless agnostic-llm--bubble-last-prompt
    (user-error "Nothing to promote yet — send a turn first"))
  (unless agnostic-llm--bubble-session-id
    (user-error "No session id recorded for this bubble"))
  (when (process-live-p agnostic-llm--bubble-process)
    (user-error "Claude is still responding — wait, or C-c C-k to cancel first"))
  (let* ((root      agnostic-llm--prompt-project-root)
         (dir       (or root default-directory))
         (label     (car (agnostic-llm--project-label dir)))
         (base      (format "*claude:%s*" label))
         (name      (generate-new-buffer-name base))
         (sid       agnostic-llm--bubble-session-id)
         (dangerous agnostic-llm--bubble-dangerous)
         (model     agnostic-llm--bubble-model)
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
      (agnostic-llm--register-buffer (current-buffer)))))

;;;###autoload
(defun agnostic-llm-prompt-bubble ()
  "Open a bubble: throwaway prompt that streams the reply inline.
Thin wrapper around `agnostic-llm-prompt' with the prefix argument
preset, so it is directly bindable and transient-invokable without
`universal-argument'.

The bubble runs `claude -p', which is non-interactive: any tool that
would normally ask you something (permission prompts, AskUserQuestion)
is auto-failed by the CLI before reaching us.  If a turn needs that kind
of interaction, promote with \\<agnostic-llm-prompt-mode-map>\\[agnostic-llm--bubble-promote] to a `*claude:PROJECT*' vterm
that resumes the same session."
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'agnostic-llm-prompt)))

(defun agnostic-llm-prompt-bubble-send ()
  "Spawn a new bubble turn (first send or post-response follow-up).
Refuses while a turn is already running."
  (if (process-live-p agnostic-llm--bubble-process)
      (user-error "Claude is still responding — wait, or C-c C-k to cancel")
    (agnostic-llm--bubble-spawn-turn)))

(defun agnostic-llm--bubble-spawn-turn (&optional explicit-prompt)
  "Start a new claude turn (first send or follow-up after completion).
With EXPLICIT-PROMPT, use it as the prompt instead of reading the
buffer's input region, and skip echoing the user turn into the buffer
so only the response is rendered."
  (let* ((has-history (markerp agnostic-llm--bubble-input-start))
         (prompt (or explicit-prompt
                     (string-trim
                      (if has-history
                          (buffer-substring-no-properties
                           agnostic-llm--bubble-input-start (point-max))
                        (buffer-string)))))
         (root   agnostic-llm--prompt-project-root)
         (default-directory (or root default-directory)))
    (when (string-empty-p prompt) (user-error "Empty prompt"))
    (setq-local agnostic-llm--bubble-last-prompt prompt)
    (let ((inhibit-read-only t)
          (dash (propertize "— " 'face 'agnostic-llm-bubble-user-face)))
      (cond
       (explicit-prompt
        (erase-buffer))
       (has-history
        (delete-region agnostic-llm--bubble-input-start (point-max))
        (goto-char (point-max))
        (insert prompt "\n\n"))
       (t
        (erase-buffer)
        (insert dash prompt "\n\n")))
      (setq-local agnostic-llm--bubble-input-start nil)
      (agnostic-llm--bubble-start-thinking (current-buffer))
      (setq header-line-format
            (propertize " Claude  (running — C-c C-k cancel)"
                        'face 'agnostic-llm-bubble-header-face)))
    (let* ((args (agnostic-llm--bubble-command prompt))
           (process-environment
            (append '("NO_COLOR=1" "CLICOLOR=0" "TERM=dumb")
                    process-environment))
           (proc (apply #'start-process "agnostic-llm-bubble" (current-buffer) args)))
      (setq-local agnostic-llm--bubble-process proc)
      (set-process-filter   proc #'agnostic-llm--bubble-filter)
      (set-process-sentinel proc #'agnostic-llm--bubble-sentinel))))

;;;###autoload
(defun agnostic-llm-prompt-send ()
  "Send the contents of the prompt buffer to Claude.
In bubble mode: run `claude -p' as a subprocess and stream the reply
into the same bubble.  Otherwise: hand off to the project's claude
vterm session (queues if busy)."
  (interactive)
  (if agnostic-llm--prompt-bubble
      (agnostic-llm-prompt-bubble-send)
    (let* ((prompt (string-trim (buffer-string)))
           (ctx    agnostic-llm--prompt-context-prefix)
           (root   agnostic-llm--prompt-project-root)
           (buf    (current-buffer))
           (full   (if ctx (concat ctx prompt) prompt)))
      (when (string-empty-p prompt) (user-error "Empty prompt"))
      (kill-buffer buf)
      (agnostic-llm--send-to-claude full root))))

(defun agnostic-llm-prompt-cancel ()
  "Cancel the prompt or response.
If a bubble subprocess is running, kill it and keep the bubble open.
Otherwise close the bubble and kill its buffer."
  (interactive)
  (cond
   ((and agnostic-llm--prompt-bubble (process-live-p agnostic-llm--bubble-process))
    (kill-process agnostic-llm--bubble-process)
    (setq-local agnostic-llm--bubble-process nil)
    (agnostic-llm--bubble-stop-thinking (current-buffer))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert "\n\n[cancelled]\n"))
    (setq header-line-format
          (propertize " Claude bubble  (cancelled — C-c C-k close)"
                      'face 'agnostic-llm-bubble-header-face)))
   (t
    (let ((buf (current-buffer)))
      (kill-buffer buf)
      (message "Prompt cancelled")))))

;;;###autoload
(defun agnostic-llm-prompt (&optional arg)
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
         (root (agnostic-llm--project-root default-directory))
         (file-name (buffer-file-name))
         (prefix (unless bubble
                   (cond
                    ((use-region-p)
                     (let* ((start (region-beginning))
                            (end (region-end))
                            (context (buffer-substring-no-properties start end))
                            (file (if file-name
                                      file-name
                                    (agnostic-llm--write-context-file context))))
                       (deactivate-mark)
                       (if file-name
                           (format "Read file %s lines %d-%d\n\n"
                                   file (line-number-at-pos start) (line-number-at-pos end))
                         (format "Context: %s\n\n" file))))
                    (file-name (format "%s:%d\n\n" file-name (line-number-at-pos (point)))))))
         (buf (if bubble
                  (generate-new-buffer "*agnostic-llm-bubble*")
                (get-buffer-create "*agnostic-llm-prompt*"))))
    (with-current-buffer buf
      (agnostic-llm-prompt-mode)
      (erase-buffer)
      (setq-local agnostic-llm--prompt-context-prefix prefix)
      (setq-local agnostic-llm--prompt-project-root root)
      (setq-local agnostic-llm--prompt-bubble bubble)
      (when bubble
        (setq-local agnostic-llm--bubble-session-id (agnostic-llm--generate-uuid))
        (setq-local agnostic-llm--bubble-dangerous agnostic-llm-dangerously-skip-permissions)
        (setq-local agnostic-llm--bubble-model agnostic-llm-model)
        (goto-char (point-max))
        (insert (propertize "— " 'face 'agnostic-llm-bubble-user-face))
        (setq-local agnostic-llm--bubble-input-start (copy-marker (point) nil))
        (setq header-line-format
              (propertize
               " Claude  C-c C-c send · C-c C-k close · C-c C-m →claude"
               'face 'agnostic-llm-bubble-header-face))))
    (agnostic-llm--present-prompt-buffer buf)))

;;; Prompt History

(defun agnostic-llm--prompts-dir (&optional root)
  "Return the absolute path to ROOT's prompt directory.
Honors `agnostic-llm-persistence-strategy'."
  (agnostic-llm--persistence-dir (or root (agnostic-llm--current-root)) "prompts"))

(defun agnostic-llm--prompt-history-files (&optional root)
  "Return ROOT's saved prompt files, newest first."
  (let ((dir (agnostic-llm--prompts-dir root)))
    (when (file-directory-p dir)
      (sort (directory-files dir t "\\.txt\\'") #'string>))))

(defun agnostic-llm--prompt-preview (file)
  "Return a one-line preview string for FILE."
  (with-temp-buffer
    (insert-file-contents file nil 0 200)
    (replace-regexp-in-string "[\n\t]+" " " (buffer-string))))

(defun agnostic-llm--open-prompt-in-bubble (text root)
  "Show TEXT in the prompt bubble, tagged for ROOT."
  (let ((buf (get-buffer-create "*agnostic-llm-prompt*")))
    (with-current-buffer buf
      (agnostic-llm-prompt-mode)
      (erase-buffer)
      (insert text)
      (setq-local agnostic-llm--prompt-project-root root))
    (agnostic-llm--present-prompt-buffer buf)))

;;;###autoload
(defun agnostic-llm-prompt-history ()
  "Browse this project's saved prompt files.
Picks a prompt via `completing-read' and opens it in the bubble
for editing and re-sending."
  (interactive)
  (let* ((root  (agnostic-llm--current-root))
         (files (agnostic-llm--prompt-history-files root)))
    (unless files (user-error "No saved prompts for this project"))
    (let* ((cands (mapcar (lambda (f)
                            (cons (format "%s  %s"
                                          (file-name-base f)
                                          (truncate-string-to-width
                                           (agnostic-llm--prompt-preview f)
                                           80 nil nil "…"))
                                  f))
                          files))
           (choice (completing-read "Prompt: " (mapcar #'car cands) nil t))
           (file   (cdr (assoc choice cands)))
           (text   (with-temp-buffer
                     (insert-file-contents file)
                     (buffer-string))))
      (agnostic-llm--open-prompt-in-bubble text root))))

;;;###autoload
(defun agnostic-llm-prompt-resume ()
  "Re-open the most recent saved prompt for this project in the bubble."
  (interactive)
  (let* ((root  (agnostic-llm--current-root))
         (files (agnostic-llm--prompt-history-files root)))
    (unless files (user-error "No saved prompts for this project"))
    (let ((text (with-temp-buffer
                  (insert-file-contents (car files))
                  (buffer-string))))
      (agnostic-llm--open-prompt-in-bubble text root))))

;;; Change Highlighting on Revert

(defface agnostic-llm-change-highlight-face
  '((((background dark))  :background "#1a3a1a" :extend t)
    (((background light)) :background "#d4f4d4" :extend t))
  "Face applied to lines added or changed in the last auto-revert.")

(defvar agnostic-llm--pre-revert-contents (make-hash-table :test 'equal)
  "Hash-table of absolute file path to pre-revert buffer text.
Each entry is captured just before `auto-revert-mode' reverts the file.")

(defvar-local agnostic-llm--change-highlight-timer nil
  "Buffer-local idle timer that removes `agnostic-llm-change-highlight' overlays.")

(defun agnostic-llm-change-highlight-clear (&optional buf)
  "Remove all change-highlight overlays from BUF (default: current buffer).
Also cancels the auto-clear timer if one is pending."
  (interactive)
  (with-current-buffer (or buf (current-buffer))
    (when (timerp agnostic-llm--change-highlight-timer)
      (cancel-timer agnostic-llm--change-highlight-timer)
      (setq agnostic-llm--change-highlight-timer nil))
    (remove-overlays (point-min) (point-max) 'category 'agnostic-llm-change-highlight)))

(defun agnostic-llm--before-revert-save ()
  "Hook: capture buffer text before `auto-revert-mode' reverts it."
  (when (buffer-file-name)
    (puthash (buffer-file-name)
             (buffer-substring-no-properties (point-min) (point-max))
             agnostic-llm--pre-revert-contents)))

(defun agnostic-llm--after-revert-highlight ()
  "Hook: highlight lines in the reverted buffer that differ from the snapshot."
  (when-let* ((file  (buffer-file-name))
              (old   (gethash file agnostic-llm--pre-revert-contents)))
    (remhash file agnostic-llm--pre-revert-contents)
    (let* ((new   (buffer-substring-no-properties (point-min) (point-max)))
           (lines (agnostic-llm--diff-added-lines old new))
           (buf   (current-buffer)))
      (agnostic-llm-change-highlight-clear buf)
      (save-excursion
        (dolist (lnum lines)
          (goto-char (point-min))
          (forward-line (1- lnum))
          (let ((ov (make-overlay (line-beginning-position)
                                  (min (point-max) (1+ (line-end-position))))))
            (overlay-put ov 'face     'agnostic-llm-change-highlight-face)
            (overlay-put ov 'category 'agnostic-llm-change-highlight)
            (overlay-put ov 'priority 10))))
      (setq agnostic-llm--change-highlight-timer
            (run-with-timer 60 nil #'agnostic-llm-change-highlight-clear buf)))))

(defun agnostic-llm--diff-added-lines (old new)
  "Return a sorted list of 1-based line numbers added/changed in NEW vs OLD.
Returns nil immediately when OLD equals NEW (frequent auto-revert case
where the timer fires but nothing actually changed on disk)."
  (unless (string= old new)
    (let ((old-file (make-temp-file "agnostic-llm-diff-a"))
          (new-file (make-temp-file "agnostic-llm-diff-b"))
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

(add-hook 'before-revert-hook #'agnostic-llm--before-revert-save)
(add-hook 'after-revert-hook  #'agnostic-llm--after-revert-highlight)

;;;###autoload
(defun agnostic-llm-switch-buffer ()
  "Switch to another claude buffer."
  (interactive)
  (let ((bufs (agnostic-llm--get-buffers)))
    (unless bufs
      (user-error "No claude buffers"))
    (let ((entries (cl-loop for buffer in bufs
                            collect (cons (buffer-name buffer) buffer))))
      (let* ((choice (completing-read "Claude buffer: "
                                      (mapcar #'car entries)
                                      nil t))
             (buf (cdr (assoc choice entries))))
        (pop-to-buffer buf)))))

(defun agnostic-llm--buffer-visible-elsewhere-p (buf)
  "Non-nil if BUF is shown in any visible window other than the selected one."
  (cl-some (lambda (w) (not (eq w (selected-window))))
           (get-buffer-window-list buf nil 'visible)))

(defun agnostic-llm--cycle-buffer (direction)
  "Switch current window to the next/previous claude buffer.
DIRECTION is +1 (forward) or -1 (backward).  Buffers already visible
in another window are deprioritized (sorted to the back), so cycling
prefers ones not yet on screen."
  (let ((bufs (sort (agnostic-llm--get-buffers)
                    (lambda (a b)
                      (let ((va (agnostic-llm--buffer-visible-elsewhere-p a))
                            (vb (agnostic-llm--buffer-visible-elsewhere-p b)))
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
(defun agnostic-llm-next-buffer ()
  "Switch current window to the next claude buffer."
  (interactive)
  (agnostic-llm--cycle-buffer +1))

;;;###autoload
(defun agnostic-llm-previous-buffer ()
  "Switch current window to the previous claude buffer."
  (interactive)
  (agnostic-llm--cycle-buffer -1))

;;; FIXME/TODO Annotation System

(defvar agnostic-llm--annotations (make-hash-table :test 'equal)
  "Hash table mapping (ROOT . KIND) to list of annotation entries.
KIND is a string like \"FIXME\" or \"TODO\".
Each entry is a plist (:file :line :text :time).")

(defun agnostic-llm--annotation-file (root kind)
  "Return the persistence file path for KIND annotations in ROOT.
Honors `agnostic-llm-persistence-strategy'."
  (agnostic-llm--persistence-dir root (format "%s.el" (downcase kind))))

(defun agnostic-llm--annotation-key (root kind)
  "Return the hash key for ROOT and KIND."
  (cons root kind))

(defun agnostic-llm--annotation-load (root kind)
  "Load annotations of KIND for ROOT from disk."
  (let ((file (agnostic-llm--annotation-file root kind)))
    (puthash (agnostic-llm--annotation-key root kind)
             (when (file-readable-p file)
               (with-temp-buffer
                 (insert-file-contents file)
                 (read (current-buffer))))
             agnostic-llm--annotations)))

(defun agnostic-llm--annotation-save (root kind)
  "Save annotations of KIND for ROOT to disk."
  (let ((file (agnostic-llm--annotation-file root kind))
        (entries (gethash (agnostic-llm--annotation-key root kind) agnostic-llm--annotations)))
    (make-directory (file-name-directory file) t)
    (agnostic-llm--ensure-ignored root)
    (with-temp-file file
      (pp entries (current-buffer)))))

(defun agnostic-llm--annotation-alive-p (entry kind)
  "Return non-nil if ENTRY's KIND comment still exists in the file.
The needle matches the `KIND(agnostic-llm): ' prefix produced by
`agnostic-llm--annotation-comment', not a bare `KIND: '."
  (let ((file (plist-get entry :file))
        (text (plist-get entry :text)))
    (and (file-readable-p file)
         (with-temp-buffer
           (insert-file-contents file)
           (let ((needle (concat kind "(agnostic-llm): " (car (split-string text "\n")))))
             (search-forward needle nil t))))))

(defun agnostic-llm--annotation-entries (root kind)
  "Return the list of live KIND annotations for ROOT.
Loads from disk if needed, then prunes entries whose comment
has been removed from the source file."
  (let ((key (agnostic-llm--annotation-key root kind)))
    (unless (gethash key agnostic-llm--annotations)
      (agnostic-llm--annotation-load root kind))
    (let* ((entries (gethash key agnostic-llm--annotations))
           (live (cl-remove-if-not (lambda (e) (agnostic-llm--annotation-alive-p e kind)) entries)))
      (unless (= (length entries) (length live))
        (puthash key live agnostic-llm--annotations)
        (agnostic-llm--annotation-save root kind))
      live)))

(defun agnostic-llm--annotation-comment (kind text)
  "Return a KIND comment for TEXT using the current mode's comment syntax."
  (let ((cs (string-trim-right (or comment-start "# ")))
        (ce (let ((e (or comment-end ""))) (if (string-empty-p e) "" (concat " " e)))))
    (mapconcat (lambda (line)
                 (concat cs " " kind "(agnostic-llm): " line ce))
               (split-string text "\n")
               "\n")))

(defvar-local agnostic-llm--annotation-kind nil
  "The annotation kind (\"FIXME\" or \"TODO\") for the current prompt buffer.")
(defvar-local agnostic-llm--annotation-source-buf nil)
(defvar-local agnostic-llm--annotation-source-file nil)
(defvar-local agnostic-llm--annotation-source-line nil)

(defun agnostic-llm--annotation-send ()
  "Insert the annotation comment at the source location and save."
  (interactive)
  (let ((text (string-trim (buffer-string)))
        (kind agnostic-llm--annotation-kind)
        (root agnostic-llm--prompt-project-root)
        (source-buf agnostic-llm--annotation-source-buf)
        (source-file agnostic-llm--annotation-source-file)
        (source-line agnostic-llm--annotation-source-line))
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
          (insert (agnostic-llm--annotation-comment kind text))
          (indent-region (line-beginning-position) (line-end-position)))))
    (let* ((key (agnostic-llm--annotation-key root kind))
           (entries (agnostic-llm--annotation-entries root kind)))
      (push (list :file source-file :line source-line
                  :text text :time (format-time-string "%Y-%m-%d %H:%M"))
            entries)
      (puthash key entries agnostic-llm--annotations))
    (agnostic-llm--annotation-save root kind)
    (message "%s added at %s:%d" kind source-file source-line)))

(defun agnostic-llm--add-annotation (kind)
  "Open a prompt buffer to compose a KIND annotation."
  (let* ((root (agnostic-llm--current-root))
         (source-buf (current-buffer))
         (source-file (or (buffer-file-name) (buffer-name)))
         (source-line (line-number-at-pos (point)))
         (buf (get-buffer-create (format "*agnostic-llm-%s*" (downcase kind)))))
    (with-current-buffer buf
      (agnostic-llm-prompt-mode)
      (erase-buffer)
      (setq-local agnostic-llm--prompt-project-root root)
      (setq-local agnostic-llm--annotation-kind kind)
      (setq-local agnostic-llm--annotation-source-buf source-buf)
      (setq-local agnostic-llm--annotation-source-file source-file)
      (setq-local agnostic-llm--annotation-source-line source-line)
      (setq header-line-format
            (format " %s  C-c C-c insert | C-c C-k cancel" kind))
      (use-local-map (let ((map (make-sparse-keymap)))
                        (set-keymap-parent map text-mode-map)
                        (define-key map (kbd "C-c C-c") #'agnostic-llm--annotation-send)
                        (define-key map (kbd "C-c C-k") #'agnostic-llm-prompt-cancel)
                        map)))
    (pop-to-buffer buf)))

(defvar-local agnostic-llm--annotation-list-kind nil
  "The annotation kind being displayed in the current list buffer.")

(defvar-local agnostic-llm--annotation-list-root nil
  "The project root whose annotations are displayed in the current list buffer.")

(defun agnostic-llm--annotation-list-refresh ()
  "Rebuild `tabulated-list-entries' from live annotations."
  (let ((entries (agnostic-llm--annotation-entries agnostic-llm--annotation-list-root
                                          agnostic-llm--annotation-list-kind)))
    (setq tabulated-list-entries
          (mapcar (lambda (e)
                    (list e
                          (vector (file-relative-name (plist-get e :file)
                                                      agnostic-llm--annotation-list-root)
                                  (number-to-string (plist-get e :line))
                                  (or (plist-get e :time) "")
                                  (truncate-string-to-width
                                   (replace-regexp-in-string "\n" " ⏎ "
                                                             (plist-get e :text))
                                   80 nil nil "…"))))
                  entries))
    (tabulated-list-print t)))

(defun agnostic-llm-annotation-list-visit ()
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

(defun agnostic-llm-annotation-list-delete ()
  "Delete the annotation at point from persistence.
Source-file comment is left untouched — remove it manually if desired."
  (interactive)
  (let* ((entry (tabulated-list-get-id))
         (kind  agnostic-llm--annotation-list-kind)
         (root  agnostic-llm--annotation-list-root)
         (key   (agnostic-llm--annotation-key root kind)))
    (unless entry (user-error "No annotation at point"))
    (puthash key
             (cl-remove entry (gethash key agnostic-llm--annotations) :test #'equal)
             agnostic-llm--annotations)
    (agnostic-llm--annotation-save root kind)
    (agnostic-llm--annotation-list-refresh)))

(defvar agnostic-llm-annotation-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agnostic-llm-annotation-list-visit)
    (define-key map (kbd "o")   #'agnostic-llm-annotation-list-visit)
    (define-key map (kbd "d")   #'agnostic-llm-annotation-list-delete)
    (define-key map (kbd "x")   #'agnostic-llm-annotation-list-delete)
    (define-key map (kbd "g")   #'agnostic-llm--annotation-list-refresh)
    map)
  "Keymap for `agnostic-llm-annotation-list-mode'.")

(define-derived-mode agnostic-llm-annotation-list-mode tabulated-list-mode "Agnostic-LLM List"
  "Tabulated view of project annotations.
\\<agnostic-llm-annotation-list-mode-map>\
\\[agnostic-llm-annotation-list-visit] visit, \\[agnostic-llm-annotation-list-delete] delete, \
\\[agnostic-llm--annotation-list-refresh] refresh."
  (setq tabulated-list-format
        [("File" 40 t) ("Line" 6 t) ("Time" 17 t) ("Text" 0 nil)])
  (setq tabulated-list-sort-key '("File"))
  (tabulated-list-init-header))

(defun agnostic-llm--list-annotations (kind)
  "Open a tabulated list of KIND annotations for the current project."
  (let* ((root (agnostic-llm--current-root))
         (entries (agnostic-llm--annotation-entries root kind)))
    (unless entries
      (user-error "No %ss in this project" kind))
    (let ((buf (get-buffer-create (format "*agnostic-llm-%ss: %s*"
                                          (downcase kind)
                                          (file-name-nondirectory
                                           (directory-file-name root))))))
      (with-current-buffer buf
        (agnostic-llm-annotation-list-mode)
        (setq agnostic-llm--annotation-list-kind kind
              agnostic-llm--annotation-list-root root)
        (agnostic-llm--annotation-list-refresh))
      (pop-to-buffer buf))))

(defun agnostic-llm--send-annotations (kind)
  "Send all KIND annotations for the current project to Claude."
  (let* ((root (agnostic-llm--current-root))
         (entries (agnostic-llm--annotation-entries root kind)))
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
      (agnostic-llm--send-to-claude
       (format "Resolve the following %ss in this project:\n\n%s" kind prompt)))))

;; The per-kind annotation commands (add/list/send for FIXME and TODO) are
;; generated from the generic helpers, so the three operations stay in sync
;; across kinds instead of being six hand-maintained wrappers.
(defmacro agnostic-llm--define-annotation-commands (kind)
  "Define `agnostic-llm-add/list/send' commands for annotation KIND (a string)."
  (let ((lc (downcase kind)))
    `(progn
       (defun ,(intern (format "agnostic-llm-add-%s" lc)) ()
         ,(format "Add a %s annotation at point." kind)
         (interactive)
         (agnostic-llm--add-annotation ,kind))
       (defun ,(intern (format "agnostic-llm-list-%ss" lc)) ()
         ,(format "List all %ss for the current project." kind)
         (interactive)
         (agnostic-llm--list-annotations ,kind))
       (defun ,(intern (format "agnostic-llm-send-%ss" lc)) ()
         ,(format "Send all %ss to Claude." kind)
         (interactive)
         (agnostic-llm--send-annotations ,kind)))))

(agnostic-llm--define-annotation-commands "FIXME")
(agnostic-llm--define-annotation-commands "TODO")

;;;###autoload
(defun agnostic-llm-grep-annotations ()
  "Grep all TODO/FIXME/HACK/XXX comments in the current project."
  (interactive)
  (let ((root (agnostic-llm--current-root)))
    (grep-find (format "grep -rnE '(TODO|FIXME|HACK|XXX):?' %s --include='*.*' -I"
                       (shell-quote-argument (directory-file-name root))))))

;;; Transient Menu

(defun agnostic-llm--menu-dangerous-p ()
  "Return non-nil if the menu's `-d' switch is active for this invocation."
  (member "--dangerously-skip-permissions" (transient-args 'agnostic-llm-menu)))

(defun agnostic-llm--menu-use-cwd-p ()
  "Return non-nil if the menu's `-c' switch is active."
  (member "-c" (transient-args 'agnostic-llm-menu)))

(defun agnostic-llm--menu-flag (prefix)
  "Return the menu argument value for PREFIX (e.g. \"--model=\"), or nil.
\"default\" is treated as nil so that no flag is passed."
  (let ((val (cl-some (lambda (a)
                        (and (stringp a)
                             (string-prefix-p prefix a)
                             (substring a (length prefix))))
                      (transient-args 'agnostic-llm-menu))))
    (if (equal val "default") nil val)))

(defun agnostic-llm--menu-current-model ()
  "Return the model in effect for `agnostic-llm-menu'.
That is the `-m' override, else `agnostic-llm-model'.  Guarded so it is
safe to call while the transient is live (e.g. from an infix `:choices'
function)."
  (or (ignore-errors (agnostic-llm--menu-flag "--model="))
      agnostic-llm-model))

(defun agnostic-llm--menu-effort ()
  "The effort selected in `agnostic-llm-menu', filtered by the current model.
Returns nil when no effort is set, or when the set effort is gated to a
model the current selection does not satisfy (e.g. \"ultracode\" with a
non-opus-4.8 model), so an unsupported level is never passed to claude."
  (let ((effort (agnostic-llm--menu-flag "--effort=")))
    (and effort
         (member effort (agnostic-llm-effort-choices-for-model (agnostic-llm--menu-current-model)))
         effort)))

;;;###autoload
(defun agnostic-llm-set-default-model (model)
  "Set MODEL as the default for new claude sessions and persist it.
Empty input clears the default (claude will pick).  The value is saved
via `customize-save-variable', so it survives Emacs restarts.

Per-invocation overrides via the menu's `-m' switch are unaffected."
  (interactive
   (list (completing-read
          (format "Default model (current: %s, empty = claude picks): "
                  (or agnostic-llm-model "none"))
          (agnostic-llm-model-choices) nil nil nil nil agnostic-llm-model)))
  (let ((value (if (or (string-empty-p (or model ""))
                       (equal model "default"))
                   nil model)))
    (customize-save-variable 'agnostic-llm-model value)
    (message "Default model %s"
             (if value (format "set to %s (saved)" value)
               "cleared (claude picks)"))))

(transient-define-suffix agnostic-llm--menu-prompt-bubble ()
  "Launch the inline-conversation bubble; honors the menu's switches."
  :description "Prompt inline (conversation)"
  (interactive)
  (let ((agnostic-llm-bubble-prompt-prefix
         (if (member "--btw" (transient-args 'agnostic-llm-menu))
             "/btw "
           ""))
        (agnostic-llm-dangerously-skip-permissions
         (or agnostic-llm-dangerously-skip-permissions (agnostic-llm--menu-dangerous-p)))
        (agnostic-llm-model (or (agnostic-llm--menu-flag "--model=") agnostic-llm-model))
        (default-directory (if (agnostic-llm--menu-use-cwd-p)
                               default-directory
                             (or (agnostic-llm--project-root) default-directory))))
    (agnostic-llm-prompt-bubble)))

(transient-define-suffix agnostic-llm--menu-open-claude ()
  "Open the main *claude:PROJECT* vterm; honors the menu's switches."
  :description "Open Claude in project"
  (interactive)
  (let ((agnostic-llm-dangerously-skip-permissions
         (or agnostic-llm-dangerously-skip-permissions (agnostic-llm--menu-dangerous-p)))
        (agnostic-llm-model (or (agnostic-llm--menu-flag "--model=") agnostic-llm-model))
        (agnostic-llm-effort (or (agnostic-llm--menu-effort) agnostic-llm-effort))
        (root (when (agnostic-llm--menu-use-cwd-p) default-directory))
        (current-prefix-arg nil))
    (agnostic-llm root)))

(defun agnostic-llm--menu-model-description ()
  "Description for the model switch showing the current default."
  (format "Model [%s]" (or agnostic-llm-model "default")))

(defun agnostic-llm--menu-effort-description ()
  "Description for the effort switch showing the current default."
  (format "Effort [%s]" (or agnostic-llm-effort "default")))

(transient-define-prefix agnostic-llm-menu ()
  "Claude CLI commands."
  ["Options"
   ("-b" "Prepend /btw slash-command to inline prompts" "--btw")
   ("-c" "Use current directory (not project root)"     "-c")
   ("-d" "Dangerously skip permission prompts"          "--dangerously-skip-permissions")
   ("-m" agnostic-llm--menu-model-description                    "--model="
    :choices (lambda () (agnostic-llm-model-choices)))
   ("-e" agnostic-llm--menu-effort-description                   "--effort="
    :choices (lambda ()
               (agnostic-llm-effort-choices-for-model (agnostic-llm--menu-current-model))))]
  [["Session"
    ("c" agnostic-llm--menu-open-claude)
    ("v" "Vterm in project"       agnostic-llm-vterm-here)
    ("b" "Switch buffer"          agnostic-llm-switch-buffer)
    ("p" "Prompt"                 agnostic-llm-prompt)
    ("P" agnostic-llm--menu-prompt-bubble)
    ("r" "Resume last prompt"     agnostic-llm-prompt-resume)
    ("H" "Prompt history"         agnostic-llm-prompt-history)
    ("M" "Set default model"      agnostic-llm-set-default-model)
    ("?" "Describe at point"      agnostic-llm-describe-at-point)
    ("R" "Show last response"     agnostic-llm-show-last-response)]
   ["Annotations"
    ("f" "Add FIXME"          agnostic-llm-add-fixme)
    ("t" "Add TODO"           agnostic-llm-add-todo)
    ("F" "List FIXMEs"        agnostic-llm-list-fixmes)
    ("T" "List TODOs"         agnostic-llm-list-todos)
    ("S" "Send FIXMEs"        agnostic-llm-send-fixmes)
    ("D" "Send TODOs"         agnostic-llm-send-todos)
    ("G" "Grep annotations"   agnostic-llm-grep-annotations)]
   ["Highlights"
    ("h" "Clear revert highlights" agnostic-llm-change-highlight-clear)]])

;;;###autoload
(defun agnostic-llm-toggle-vterm-claude ()
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
            (pcase-let* ((`(,_ . ,root) (agnostic-llm--project-label default-directory))
                         (default-directory (or root default-directory)))
              (if (equal kind "vterm")
                  (let ((vterm-shell (agnostic-llm--claude-shell-command root)))
                    (vterm target)
                    (agnostic-llm--register-buffer (current-buffer)))
                (vterm target)))))
      (let ((current-prefix-arg nil))
        (agnostic-llm-vterm-here)))))

;;;###autoload
(defun agnostic-llm-describe-at-point ()
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
         (root (agnostic-llm--current-root))
         (buf (generate-new-buffer "*agnostic-llm-bubble*")))
    (when region-p (deactivate-mark))
    (with-current-buffer buf
      (agnostic-llm-prompt-mode)
      (erase-buffer)
      (setq-local agnostic-llm--prompt-project-root root)
      (setq-local agnostic-llm--prompt-bubble t)
      (setq-local agnostic-llm--bubble-session-id (agnostic-llm--generate-uuid))
      (setq-local agnostic-llm--bubble-dangerous agnostic-llm-dangerously-skip-permissions)
      (setq-local agnostic-llm--bubble-model agnostic-llm-model))
    (agnostic-llm--present-prompt-buffer buf)
    (with-current-buffer buf
      (agnostic-llm--bubble-spawn-turn text))))

;;; Vterm display fixups

(defun agnostic-llm--vterm-display-fixups ()
  "Neutralize global display settings that corrupt vterm's character grid.
Some Emacs configs set `line-spacing' globally; the extra
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

(defvar-local agnostic-llm--vterm-copy-resume nil
  "Non-nil when exiting `vterm-copy-mode' should resume a suspended process.
Set in a buffer whose foreground process `agnostic-llm-vterm-copy'
suspended.")

(defun agnostic-llm--vterm-copy-resume-on-exit ()
  "Send `fg' when copy-mode is disabled in a buffer flagged for resume."
  (when (and agnostic-llm--vterm-copy-resume (not vterm-copy-mode))
    (process-send-string vterm--process "fg\n")
    (setq-local agnostic-llm--vterm-copy-resume nil)))

(defun agnostic-llm-vterm-copy ()
  "Suspend the foreground vterm process and enter `vterm-copy-mode'.
Resumes the process automatically when copy-mode is exited.

Useful for copying from TUIs (e.g. interactive `claude') that
continuously redraw and overwrite selections.  No-op outside vterm."
  (interactive)
  ;; (unless (derived-mode-p 'vterm-mode)
  ;;   (user-error "Not a vterm buffer"))
  ;; (process-send-string vterm--process "\C-z")
  ;; (setq-local agnostic-llm--vterm-copy-resume t)
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

(defcustom agnostic-llm-vterm-autofreeze-buffers #'agnostic-llm-buffer-p
  "Predicate deciding whether auto-freeze is active in a vterm buffer.
Called with no args in the vterm buffer; non-nil enables the behavior.
Defaults to claude buffers only, so plain `*vterm:…*' shells are untouched."
  :type 'function
  :group 'agnostic-llm)

(defvar-local agnostic-llm--vterm-autofreeze-armed nil
  "Guard so our own copy-mode toggles don't re-trigger the observer.")

(defvar-local agnostic-llm--vterm-autofreeze-active nil
  "Non-nil when copy-mode in this buffer was entered BY auto-freeze.
As opposed to the manual `agnostic-llm-vterm-copy' workflow or a bare
`vterm-copy-mode'.  Scopes the resume overlay map and the header-line so
manual copy-mode keeps vanilla behavior.")

(defun agnostic-llm--vterm-autofreeze-p ()
  "Non-nil if auto-freeze should manage the current buffer."
  (and (derived-mode-p 'vterm-mode)
       (boundp 'vterm--term) vterm--term
       (ignore-errors (funcall agnostic-llm-vterm-autofreeze-buffers))))

(defun agnostic-llm--vterm-at-bottom-p ()
  "Non-nil if point in the selected window is on the buffer's last line.
That last line is the live prompt/cursor row vterm keeps pinned, so being
there means we are following; being above it means the user scrolled up."
  (>= (point)
      (save-excursion (goto-char (point-max))
                      (line-beginning-position))))

(defun agnostic-llm--vterm-freeze ()
  "Enter copy-mode to freeze the view (XOFF pauses claude).  Idempotent.
Sets `agnostic-llm--vterm-autofreeze-active' so only our managed freeze gets the
resume overlay map + header-line, and leaves `agnostic-llm--vterm-copy-resume' nil
so exiting sends only XON, never the `fg' of the manual suspend path."
  ;; (unless (or (bound-and-true-p vterm-copy-mode)
  ;;             agnostic-llm--vterm-autofreeze-armed)
  ;;   (let ((agnostic-llm--vterm-autofreeze-armed t))
  ;;     (setq agnostic-llm--vterm-autofreeze-active t)
  ;;     (vterm-copy-mode 1)
  ;;     (setq header-line-format
  ;;           " VTerm frozen — scroll to read · q quits · any key resumes")))
  )

(defun agnostic-llm--vterm-resume ()
  "Exit copy-mode and snap point to the live cursor so output follows again.
Safe to call when not frozen (no-op)."
  (when (bound-and-true-p vterm-copy-mode)
    (let ((agnostic-llm--vterm-autofreeze-armed t))
      (vterm-copy-mode -1)        ; sends XON, restores vterm-mode-map
      (vterm-reset-cursor-point)  ; point -> live terminal cursor (bottom)
      (dolist (w (get-buffer-window-list (current-buffer) nil t))
        (set-window-point w (point))))))

(defun agnostic-llm--vterm-autofreeze-post-command ()
  "`post-command-hook': freeze when point has moved up off the live line.
Only fires from the command loop — never from vterm's redraw timer — so
plain streaming with no keypress does NOT freeze; the user must actively
scroll or move point up.  Skips mid-toggle calls via the armed guard."
  (when (and (not agnostic-llm--vterm-autofreeze-armed)
             (not (bound-and-true-p vterm-copy-mode))
             (agnostic-llm--vterm-autofreeze-p)
             (not (agnostic-llm--vterm-at-bottom-p)))
    (agnostic-llm--vterm-freeze)))

(defun agnostic-llm--vterm-resume-and-resend ()
  "Thaw, then replay the triggering key into the live terminal.
The key is not lost.  Bound to self-inserting keys while auto-frozen."
  (interactive)
  (let ((keys (this-command-keys-vector)))
    (agnostic-llm--vterm-resume)
    (setq unread-command-events
          (append (listify-key-sequence keys) unread-command-events))))

(defun agnostic-llm--vterm-resume-only ()
  "Thaw without resending the key (viewer-style quit on `q')."
  (interactive)
  (agnostic-llm--vterm-resume))

(defvar agnostic-llm--vterm-autofreeze-resume-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap self-insert-command] #'agnostic-llm--vterm-resume-and-resend)
    (dolist (k '("SPC" "DEL" "TAB" "RET" "<return>"))
      (define-key map (kbd k) #'agnostic-llm--vterm-resume-and-resend))
    (define-key map (kbd "q") #'agnostic-llm--vterm-resume-only)
    map)
  "Overlay keymap active while a claude vterm is auto-frozen.
Layered above `vterm-copy-mode-map' so it adds resume keys without
overriding copy-mode's bindings.  Note: this rebinds RET to
resume-and-send-newline; drop the RET/<return> entries if you prefer
RET to copy the line (`vterm-copy-mode-done').
Deliberately binds neither EOF nor SIGINT, so a stray key while reading
can never end or interrupt claude.")

(defun agnostic-llm--vterm-autofreeze-copy-hook ()
  "Run on `vterm-copy-mode-hook' (fires on BOTH enable and disable).
On enable of an auto-freeze-initiated copy-mode, install the resume
overlay map.  On disable, clear our header-line and flag.  Manual
`vterm-copy-mode' sessions (active flag nil) stay vanilla."
  (when (agnostic-llm--vterm-autofreeze-p)
    (cond
     ((and (bound-and-true-p vterm-copy-mode)
           agnostic-llm--vterm-autofreeze-active)
      (set-transient-map agnostic-llm--vterm-autofreeze-resume-map
                         (lambda () (bound-and-true-p vterm-copy-mode))))
     ((not (bound-and-true-p vterm-copy-mode))
      (when agnostic-llm--vterm-autofreeze-active
        (setq agnostic-llm--vterm-autofreeze-active nil)
        (setq header-line-format nil))))))

(defun agnostic-llm--vterm-autofreeze-setup ()
  "Arm the auto-freeze observer buffer-locally for a managed vterm buffer."
  (when (agnostic-llm--vterm-autofreeze-p)
    (add-hook 'post-command-hook
              #'agnostic-llm--vterm-autofreeze-post-command nil t)))

(add-hook 'vterm-mode-hook #'agnostic-llm--vterm-display-fixups)

(provide 'agnostic-llm)
;;; agnostic-llm.el ends here
