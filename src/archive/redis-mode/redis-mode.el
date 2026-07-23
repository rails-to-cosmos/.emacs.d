;;; redis-mode.el --- Redis client for Emacs -*- lexical-binding: t; -*-

;;; Commentary:
;; A small Redis client backed by a native Rust module.  Two front-ends:
;;
;;   M-x redis-connect URL       opens a REPL buffer
;;   M-x redis-browse-keys URL   opens a SCAN-driven key browser
;;
;; The REPL is a plain text buffer (not comint): type a command, RET sends.
;; The browser pages through keys with `g' (refresh) / `m' (more) and shows
;; a value preview at point.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(declare-function redis-mod-exec       "redis-mod" (url cmd))
(declare-function redis-mod-scan       "redis-mod" (url cursor pattern count))
(declare-function redis-mod-type-of    "redis-mod" (url key))
(declare-function redis-mod-preview    "redis-mod" (url key max-items))
(declare-function redis-mod-ping       "redis-mod" (url))
(declare-function redis-mod-disconnect "redis-mod" (url))

;;; Native module loading

(defvar redis--native-module
  (expand-file-name
   (format "src/redis-mode/rust/target/release/libredis_mod.%s"
           (if (eq system-type 'darwin) "dylib" "so"))
   user-emacs-directory)
  "Path to the compiled Rust dynamic module.")

(defvar redis--native-loaded nil
  "Non-nil if the native redis-mod module has been successfully loaded.")

(defun redis--ensure-native ()
  "Load the native module, signalling a clear error if unavailable."
  (unless redis--native-loaded
    (unless (fboundp 'module-load)
      (user-error "Emacs was built without --with-modules; rebuild Emacs"))
    (unless (file-exists-p redis--native-module)
      (user-error
       "Native module not found at %s.  Build with:\n  cd %s && cargo build --release"
       redis--native-module
       (expand-file-name "src/redis-mode/rust" user-emacs-directory)))
    (module-load redis--native-module)
    (setq redis--native-loaded t)))

;;; Configuration

(defcustom redis-default-url "redis://127.0.0.1:6379/0"
  "Default URL prefilled when prompting in `redis-connect'."
  :type 'string
  :group 'redis)

(defcustom redis-scan-page-size 100
  "How many keys to ask for per SCAN call in the browser."
  :type 'integer
  :group 'redis)

(defcustom redis-preview-max-items 16
  "Maximum elements shown in a value preview (lists/sets/hashes/zsets)."
  :type 'integer
  :group 'redis)

(defface redis-prompt-face
  '((((background dark))  :foreground "#7aa2f7" :weight bold)
    (((background light)) :foreground "#5c7cfa" :weight bold))
  "Face for the REPL prompt.")

(defface redis-error-face
  '((t :inherit error))
  "Face for error responses in the REPL.")

(defface redis-key-face
  '((t :inherit font-lock-string-face))
  "Face for keys in the browser.")

(defface redis-type-face
  '((t :inherit font-lock-keyword-face))
  "Face for type tags in the browser.")

;;; URL helpers

(defun redis--url-label (url)
  "Return a short host:port/db label for URL."
  (if (string-match
       (rx "redis://"
           (? (group (+ (not (any "@/")))) "@")
           (group (+? (not (any ":/"))))
           (? ":" (group (+ digit)))
           (? "/" (group (+ digit))))
       url)
      (format "%s:%s/%s"
              (match-string 2 url)
              (or (match-string 3 url) "6379")
              (or (match-string 4 url) "0"))
    url))

(defun redis--read-url ()
  "Prompt for a URL, defaulting to `redis-default-url'."
  (read-string (format "Redis URL (default %s): " redis-default-url)
               nil nil redis-default-url))

;;; REPL

(defvar-local redis--url nil
  "Connection URL for this buffer.")

(defvar-local redis--input-start nil
  "Marker at the start of the user's pending input (after the prompt).")

(defvar-local redis--history nil
  "List of past commands sent in this buffer (most recent first).")

(defvar-local redis--history-pos nil
  "Index into `redis--history' while walking with M-p / M-n.")

(defvar redis-repl-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'redis-repl-send)
    (define-key map (kbd "M-p")   #'redis-repl-history-prev)
    (define-key map (kbd "M-n")   #'redis-repl-history-next)
    (define-key map (kbd "C-c C-c") #'redis-repl-clear)
    (define-key map (kbd "C-c C-k") #'redis-disconnect)
    (define-key map (kbd "C-c C-b") #'redis-browse-keys)
    map)
  "Keymap for `redis-repl-mode'.")

(define-derived-mode redis-repl-mode fundamental-mode "Redis-REPL"
  "Major mode for the Redis REPL buffer."
  (setq-local truncate-lines nil)
  (setq-local comment-start "# "))

(defun redis--insert-prompt ()
  "Insert a fresh prompt at point-max and park the input marker."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((start (point)))
      (insert (format "%s> " (redis--url-label redis--url)))
      (add-text-properties start (point)
                           '(face redis-prompt-face read-only t rear-nonsticky t)))
    (setq-local redis--input-start (copy-marker (point) nil))))

;;;###autoload
(defun redis-connect (url)
  "Connect to URL (a `redis://…' URL) and pop a REPL buffer."
  (interactive (list (redis--read-url)))
  (redis--ensure-native)
  ;; Probe before opening the buffer so the user gets a clean error if the
  ;; URL is unreachable.
  (redis-mod-ping url)
  (let* ((label (redis--url-label url))
         (buf (get-buffer-create (format "*redis:%s*" label))))
    (with-current-buffer buf
      (unless (derived-mode-p 'redis-repl-mode)
        (redis-repl-mode)
        (setq-local redis--url url)
        (let ((inhibit-read-only t))
          (insert (propertize
                   (format ";; Connected to %s.  RET to send. C-c C-b: browse keys.\n"
                           label)
                   'face 'font-lock-comment-face
                   'read-only t 'rear-nonsticky t))
          (redis--insert-prompt))))
    (pop-to-buffer buf)))

(defun redis--current-input ()
  "Return the trimmed input the user has typed after the prompt."
  (string-trim
   (buffer-substring-no-properties redis--input-start (point-max))))

(defun redis-repl-send ()
  "Send the current line to Redis and append the formatted reply."
  (interactive)
  (unless (markerp redis--input-start)
    (user-error "No active prompt"))
  (let ((input (redis--current-input)))
    (cond
     ((string-empty-p input)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n"))
      (redis--insert-prompt))
     (t
      (push input redis--history)
      (setq-local redis--history-pos nil)
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert "\n"))
      (let ((output
             (condition-case err
                 (redis-mod-exec redis--url input)
               (error
                (propertize (format "(error) %s" (error-message-string err))
                            'face 'redis-error-face)))))
        (let ((inhibit-read-only t))
          (insert output)
          (unless (bolp) (insert "\n"))))
      (redis--insert-prompt)))))

(defun redis-repl-clear ()
  "Erase the REPL buffer and start over."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (redis--insert-prompt))

(defun redis--history-replace (text)
  "Replace the current input region with TEXT."
  (let ((inhibit-read-only t))
    (delete-region redis--input-start (point-max))
    (goto-char (point-max))
    (insert text)))

(defun redis-repl-history-prev ()
  "Step back through `redis--history'."
  (interactive)
  (when redis--history
    (setq-local redis--history-pos
                (min (1- (length redis--history))
                     (1+ (or redis--history-pos -1))))
    (redis--history-replace (nth redis--history-pos redis--history))))

(defun redis-repl-history-next ()
  "Step forward through `redis--history'."
  (interactive)
  (cond
   ((null redis--history-pos)
    (redis--history-replace ""))
   ((<= redis--history-pos 0)
    (setq-local redis--history-pos nil)
    (redis--history-replace ""))
   (t
    (cl-decf redis--history-pos)
    (redis--history-replace (nth redis--history-pos redis--history)))))

;;;###autoload
(defun redis-disconnect ()
  "Drop the cached connection for the current REPL/browser buffer."
  (interactive)
  (when redis--url
    (redis-mod-disconnect redis--url)
    (message "Disconnected %s" (redis--url-label redis--url))))

;;; Key browser

(defvar-local redis--scan-cursor "0"
  "Current SCAN cursor (\"0\" before the first call; \"0\" after the last).")

(defvar-local redis--scan-pattern "*"
  "Current MATCH pattern.")

(defvar-local redis--scan-keys nil
  "Keys collected so far in this browser buffer.")

(defvar redis-browser-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "n" #'next-line)
    (define-key map "p" #'previous-line)
    (define-key map "g" #'redis-browser-refresh)
    (define-key map "m" #'redis-browser-more)
    (define-key map "/" #'redis-browser-set-pattern)
    (define-key map "v" #'redis-browser-view)
    (define-key map (kbd "RET") #'redis-browser-view)
    (define-key map "q" #'quit-window)
    (define-key map "?" #'redis-browser-help)
    map)
  "Keymap for `redis-browser-mode'.")

(define-derived-mode redis-browser-mode fundamental-mode "Redis-Keys"
  "Major mode for browsing Redis keys."
  (setq buffer-read-only t)
  (setq-local truncate-lines t))

(defun redis--browser-redraw ()
  "Render `redis--scan-keys' into the current buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (insert (propertize
             (format "Redis %s   pattern: %s   shown: %d   cursor: %s\n"
                     (redis--url-label redis--url)
                     redis--scan-pattern
                     (length redis--scan-keys)
                     (if (string= redis--scan-cursor "0") "done" redis--scan-cursor))
             'face 'header-line))
    (insert (propertize " g refresh · m more · / pattern · v/RET view · q quit\n\n"
                        'face 'font-lock-comment-face))
    (dolist (entry redis--scan-keys)
      (pcase-let ((`(,type . ,key) entry))
        (insert (propertize (format "%-7s" type) 'face 'redis-type-face)
                "  "
                (propertize key 'face 'redis-key-face)
                "\n")))
    (goto-char (point-min))
    (forward-line (max 2 (1- line)))))

(defun redis--browser-fetch (reset)
  "Fetch one SCAN page; if RESET, start from cursor 0 and clear keys."
  (when reset
    (setq-local redis--scan-cursor "0"
                redis--scan-keys nil))
  (let* ((result (redis-mod-scan redis--url
                                 redis--scan-cursor
                                 redis--scan-pattern
                                 redis-scan-page-size))
         (next (car result))
         (keys (cdr result)))
    (setq-local redis--scan-cursor next)
    (dolist (k keys)
      (let ((type (condition-case _err
                      (redis-mod-type-of redis--url k)
                    (error "?"))))
        (push (cons type k) redis--scan-keys)))
    (setq-local redis--scan-keys (nreverse redis--scan-keys))
    (redis--browser-redraw)))

;;;###autoload
(defun redis-browse-keys (&optional url)
  "Open a key browser for URL (defaults to a prompt)."
  (interactive)
  (redis--ensure-native)
  (let* ((url (or url (redis--read-url)))
         (label (redis--url-label url))
         (buf (get-buffer-create (format "*redis-keys:%s*" label))))
    (with-current-buffer buf
      (redis-browser-mode)
      (setq-local redis--url url)
      (setq-local redis--scan-pattern "*")
      (redis--browser-fetch t))
    (pop-to-buffer buf)))

(defun redis-browser-refresh ()
  "Restart SCAN from cursor 0."
  (interactive)
  (redis--browser-fetch t))

(defun redis-browser-more ()
  "Fetch one more SCAN page (no-op once cursor is back to 0)."
  (interactive)
  (if (string= redis--scan-cursor "0")
      (message "No more keys")
    (redis--browser-fetch nil)))

(defun redis-browser-set-pattern (pattern)
  "Set MATCH PATTERN and refresh."
  (interactive (list (read-string "MATCH pattern: " redis--scan-pattern)))
  (setq-local redis--scan-pattern (if (string-empty-p pattern) "*" pattern))
  (redis--browser-fetch t))

(defun redis--key-at-point ()
  "Return the key on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "[^[:space:]]+\\s-+\\(.+\\)$")
      (match-string-no-properties 1))))

(defun redis-browser-view ()
  "Open a value-preview buffer for the key on the current line."
  (interactive)
  (let ((key (redis--key-at-point)))
    (unless key (user-error "No key on this line"))
    (let* ((url redis--url)
           (label (redis--url-label url))
           (buf (get-buffer-create (format "*redis-value:%s:%s*" label key))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (special-mode)
          (setq-local redis--url url)
          (insert (propertize (format "%s @ %s\n\n" key label)
                              'face 'header-line))
          (insert (redis-mod-preview url key redis-preview-max-items))))
      (pop-to-buffer buf))))

(defun redis-browser-help ()
  "Show browser keybindings."
  (interactive)
  (message "n/p line · g refresh · m more · / pattern · v/RET view value · q quit"))

(provide 'redis-mode)
;;; redis-mode.el ends here
