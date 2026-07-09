;;; table-view-native.el --- Native (Rust) data backend for table-view -*- lexical-binding: t; -*-

;; Author: Dmitry Akatov <akatovda@gmail.com>
;; URL: https://github.com/rails-to-cosmos/table-view
;; Package-Version: 20260708.836
;; Package-Revision: d89d3b543593
;; Package-Requires: ((emacs "28.1") (table-view "0.1.0"))
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Optional accelerator: a Rust subprocess (`tvx') owns the table data model
;; (sort, filter, window) so Emacs stays a thin view past ~20k rows.  It speaks
;; JSON-RPC over stdio via the built-in `jsonrpc.el' and plugs into the existing
;; `table-view' paged mode as a `page-fn' -- no core rewrite.
;;
;; Distribution is compile-on-install: the Rust source ships under native/ and
;; is built with cargo via `M-x table-view-native-compile'.  When the binary is
;; absent or unbuildable, table-view runs the pure-elisp path and warns.
;;
;; Phase 1: initialize / open / window / close (discrete paged windows).  The
;; live-update layer ($/delta, `table-view-apply-delta') lands later.

;;; Code:

(require 'cl-lib)
(require 'jsonrpc)
(require 'table-view)

(defconst table-view-native-protocol 1
  "Wire-protocol integer this Emacs side speaks.
Must equal the binary's `open' reply :protocol and the N in
\"tvx X.Y.Z protocol N\" from `tvx --version'.")

;;; Customization

(defgroup table-view-native nil
  "Native backend for `table-view'." :group 'table-view :prefix "table-view-native-")

(defcustom table-view-native-enabled t
  "When nil, always use the pure-elisp path (no native backend)." :type 'boolean)
(defcustom table-view-native-program nil
  "Explicit path to the `tvx' binary, or nil to auto-resolve." :type '(choice (const nil) file))
(defcustom table-view-native-cargo-program "cargo"
  "Cargo executable used to build the backend." :type 'string)
(defcustom table-view-native-auto-compile 'prompt
  "Whether first native use may build the binary: prompt, t (silent), nil (never)."
  :type '(choice (const prompt) (const t) (const nil)))
(defcustom table-view-native-warn t
  "When non-nil, warn the first time the pure-elisp fallback is used." :type 'boolean)

;;; Paths / resolution

(defvar table-view-native--source-dir
  (let ((d (or (and load-file-name (file-name-directory load-file-name))
               (ignore-errors (file-name-directory (locate-library "table-view-native"))))))
    (and d (expand-file-name "native/tvx/" d)))
  "Directory of the bundled Rust crate (native/tvx/).")

(defvar table-view-native--cache-dir (locate-user-emacs-file "table-view/tvx/")
  "User-writable directory holding the compiled binary.")

(defun table-view-native--exe () (if (eq system-type 'windows-nt) "tvx.exe" "tvx"))

(defun table-view-native--cached-binary ()
  "Path of the cached compiled binary (name embeds the protocol version)."
  (expand-file-name (format "tvx-p%d%s" table-view-native-protocol
                            (if (eq system-type 'windows-nt) ".exe" ""))
                    table-view-native--cache-dir))

(defun table-view-native--validate (prog)
  "Return PROG when it runs and reports the expected protocol, else nil."
  (when (and prog (file-executable-p prog))
    (ignore-errors
      (with-temp-buffer
        (when (and (eq 0 (call-process prog nil t nil "--version"))
                   (string-match "protocol \\([0-9]+\\)" (buffer-string))
                   (= (string-to-number (match-string 1 (buffer-string)))
                      table-view-native-protocol))
          prog)))))

(defun table-view-native--resolve ()
  "Return a validated backend path, or nil.  Order: custom, cache, PATH, cargo bin."
  (seq-some #'table-view-native--validate
            (list (and table-view-native-program (executable-find table-view-native-program))
                  (let ((c (table-view-native--cached-binary))) (and (file-executable-p c) c))
                  (executable-find (table-view-native--exe))
                  (let ((c (expand-file-name (format "~/.cargo/bin/%s" (table-view-native--exe)))))
                    (and (file-executable-p c) c)))))

;;; Fallback + warning

(defvar table-view-native--warned (make-hash-table)
  "Reasons already warned about this session, for dedupe.")

(defun table-view-native--warn-text (reason detail)
  (concat
   (pcase reason
     ('no-cargo "native backend unavailable: cargo not found.  Install Rust (https://rustup.rs), then M-x table-view-native-compile.")
     ('no-binary "native backend not built.  M-x table-view-native-compile to enable it.")
     ('building "building the native backend; using the pure-elisp path until it finishes.")
     ('build-failed "native build failed -- see *tvx-compile*.  Using the pure-elisp path.")
     ('version-mismatch (format "native binary speaks protocol %s, need %d.  Rebuild with M-x table-view-native-compile."
                                detail table-view-native-protocol))
     ('unsupported-source "the native backend is required for this data source.")
     ('recommend-build (format "this %s-row table would be much faster with the native backend.  M-x table-view-native-compile to build it (needs cargo)." detail))
     (_ "using the pure-elisp path (slow past ~20k rows)."))
   "  Silence: (setq table-view-native-warn nil)."))

(defun table-view-native--fallback (reason &optional detail)
  "Note that the pure-elisp path is in use because of REASON; warn once."
  (when (and table-view-native-warn (not (gethash reason table-view-native--warned)))
    (puthash reason t table-view-native--warned)
    (display-warning 'table-view (table-view-native--warn-text reason detail) :warning))
  nil)

;;; Build (compile-on-install)

(defvar table-view-native--build-in-progress nil)

(defun table-view-native--cargo ()
  (or (executable-find table-view-native-cargo-program)
      (let ((c (expand-file-name "~/.cargo/bin/cargo"))) (and (file-executable-p c) c))))

(defvar table-view-native--build-callbacks nil
  "Callbacks awaiting the in-progress build; each is called with the path or nil.")

(defun table-view-native-compile (&optional force callback)
  "Build the native backend with cargo and cache the binary.
With FORCE (interactively, or non-nil), rebuild even when a valid binary
exists.  CALLBACK, if given, runs with the resulting path (or nil) when the
build finishes; callbacks queued while a build is already running all fire."
  (interactive (list t nil))
  (let ((have (and (not force) (table-view-native--resolve))))
    (cond
     (have (when callback (funcall callback have)) have)
     ((not (table-view-native--cargo))
      (table-view-native--fallback 'no-cargo) (when callback (funcall callback nil)) nil)
     ((not table-view-native--source-dir)
      (when callback (funcall callback nil)) nil)
     (t
      (when callback (push callback table-view-native--build-callbacks))
      (if table-view-native--build-in-progress
          nil                               ; join the running build; callback queued
        (setq table-view-native--build-in-progress t)
        (make-directory table-view-native--cache-dir t)
        (let* ((manifest (expand-file-name "Cargo.toml" table-view-native--source-dir))
               (buf (get-buffer-create "*tvx-compile*")))
          (with-current-buffer buf (erase-buffer))
          (make-process
           :name "tvx-compile" :buffer buf :noquery t
           :command (list (table-view-native--cargo) "build" "--release" "--manifest-path" manifest)
           :sentinel
           (lambda (proc _event)
             (when (memq (process-status proc) '(exit signal))
               (setq table-view-native--build-in-progress nil)
               (let* ((ok (eq 0 (process-exit-status proc)))
                      (built (expand-file-name (format "target/release/%s" (table-view-native--exe))
                                               table-view-native--source-dir))
                      (dest (table-view-native--cached-binary))
                      (result (if (and ok (file-executable-p built))
                                  (progn (copy-file built dest t) (set-file-modes dest #o755)
                                         (let ((valid (table-view-native--validate dest)))
                                           (unless valid (table-view-native--fallback 'version-mismatch))
                                           valid))
                                (table-view-native--fallback 'build-failed)
                                nil))
                      (cbs (nreverse table-view-native--build-callbacks)))
                 (setq table-view-native--build-callbacks nil)
                 (dolist (cb cbs) (funcall cb result))))))
          (message "Building the table-view native backend in *tvx-compile*...")
          nil))))))

(defun table-view-native--ensure ()
  "Return a validated backend path if ready, else nil.
Does not prompt or build -- `table-view-native-display' owns the build
decision; this is the best-effort resolver for the connection/respawn path."
  (and table-view-native-enabled (table-view-native--resolve)))

;;; Connection

(defvar table-view-native--connection nil "Shared jsonrpc connection, or nil.")
(defvar table-view-native--handles (make-hash-table) "Handle -> buffer, for respawn.")

(defvar-local table-view-native--source nil "This buffer's backend source plist.")
(defvar-local table-view-native--conn-handle nil "Cons (CONNECTION . HANDLE) for this buffer.")
(defvar-local table-view-native--rev 0 "Server rev this buffer is consistent at.")
(defvar-local table-view-native--gen 0 "Subscription generation this buffer is bound to.")

(defun table-view-native--close (conn handle)
  "Close HANDLE on CONN (when live) and drop it from the handle registry.
Idempotent; safe to call for an already-dead connection."
  (when handle
    (when (and conn (jsonrpc-connection-p conn) (jsonrpc-running-p conn))
      (ignore-errors (jsonrpc-notify conn 'close (list :handle handle))))
    (remhash handle table-view-native--handles)))

(defun table-view-native--on-kill ()
  "Close this buffer's backend handle when the buffer is killed."
  (when table-view-native--conn-handle
    (table-view-native--close (car table-view-native--conn-handle)
                              (cdr table-view-native--conn-handle))))

(defun table-view-native--make-connection (prog)
  (let ((conn (make-instance 'jsonrpc-process-connection
                :name "tvx"
                :notification-dispatcher #'table-view-native--dispatch
                :process (make-process :name "tvx" :command (list prog)
                                       :coding 'utf-8-emacs-unix :connection-type 'pipe
                                       :stderr (get-buffer-create "*tvx stderr*") :noquery t))))
    (jsonrpc-request conn 'initialize
                     (list :clientInfo (list :name "table-view" :version "0.1.0")
                           :protocol (list :min 1 :max table-view-native-protocol)
                           :wire ["json"] :capabilities (list :delta :json-false)))
    conn))

(defun table-view-native--ensure-connection ()
  "Return a live connection, starting one if needed and possible; else nil."
  (or (and table-view-native--connection
           (jsonrpc-running-p table-view-native--connection)
           table-view-native--connection)
      (when-let ((prog (table-view-native--ensure)))
        (setq table-view-native--connection (table-view-native--make-connection prog)))))

(defun table-view-native--dispatch (_conn method params)
  "Route a server notification.  Handles $/delta (a subscribed window changed).
A delta is applied only when it both continues this client's rev (:baseRev
equals the local rev) and targets the current subscription (:gen equals the
local gen); otherwise -- an out-of-order/missed delta, or one computed for a
newer window whose fetch reply was lost -- the window is refetched, which
re-subscribes and re-bases, so the view self-heals instead of corrupting."
  (when (eq method '$/delta)
    (let ((buf (gethash (plist-get params :handle) table-view-native--handles)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (if (and (eql (plist-get params :baseRev) table-view-native--rev)
                   (eql (plist-get params :gen) table-view-native--gen))
              (progn
                (setq table-view--total (plist-get params :matched))  ; before the render
                (table-view-apply-delta buf (table-view-native--delta-ops (plist-get params :ops)))
                (setq table-view-native--rev (plist-get params :rev)
                      table-view--has-next
                      (and table-view--page-size
                           (< (+ (or table-view--offset 0) (length table-view--rows))
                              (or table-view--total 0)))))
            (table-view--refetch-current)))))))

;;; Row conversion (jsonrpc plist <-> table-view alist)

(defun table-view-native--row (r)
  "Convert a reply row plist R to table-view's ((id . ID) (cells . ALIST)) shape."
  (list (cons 'id (plist-get r :id))
        (cons 'cells (cl-loop for (k v) on (plist-get r :cells) by #'cddr
                              collect (cons (intern (substring (symbol-name k) 1)) v)))))

(defun table-view-native--row->wire (row)
  "Convert a table-view ROW alist to the backend's (:id ID :cells PLIST) shape."
  (list :id (alist-get 'id row)
        :cells (cl-loop for (k . v) in (alist-get 'cells row)
                        append (list (intern (concat ":" (symbol-name k))) v))))

(defun table-view-native--delta-ops (ops)
  "Convert $/delta OPS (a vector of plists) to `table-view-apply-delta' ops."
  (mapcar (lambda (op)
            (if (plist-member op :row)
                (plist-put (copy-sequence op) :row (table-view-native--row (plist-get op :row)))
              op))
          (append ops nil)))

;;; The page-fn closure

(defun table-view-native--columns (spec)
  "Backend column schema from SPEC.
A vector of (:key K :type T); `value-fn' columns are excluded (Emacs-side only)."
  (vconcat
   (delq nil
         (mapcar (lambda (col)
                   (unless (alist-get 'value-fn col)   ; value-fn cols are Emacs-side only
                     (list :key (alist-get 'key col)
                           :type (pcase (alist-get 'type col)
                                   ("number" "number") ("badge" "badge") (_ "text")))))
                 (table-view--columns spec)))))

(defun table-view-native--open (conn)
  "Open the current buffer's source on CONN; return the new handle.
Registers the handle and re-bases rev.  Called on first display and again,
transparently, after a respawn re-opens the source on a fresh connection."
  (let* ((open (jsonrpc-request
                conn 'open
                (list :source table-view-native--source
                      :columns (table-view-native--columns table-view--spec)
                      :pageSize (or table-view--page-size 50)
                      :protocol table-view-native-protocol)))
         (handle (plist-get open :handle)))
    ;; Drop the previous handle (dead after a respawn) so its registry entry and
    ;; backend table do not leak.
    (when table-view-native--conn-handle
      (table-view-native--close (car table-view-native--conn-handle)
                                (cdr table-view-native--conn-handle)))
    (setq table-view-native--conn-handle (cons conn handle)
          table-view-native--rev (or (plist-get open :rev) 0))
    (puthash handle (current-buffer) table-view-native--handles)
    handle))

(defun table-view-native--live-handle (buf conn conn0 handle0)
  "The handle for BUF valid on CONN, re-opening the source if CONN is new.
This is the respawn seam: a crashed backend yields a fresh CONN, and the
next window re-opens and re-subscribes without the caller noticing.  CONN0
and HANDLE0 are the open captured at display time -- used for the very first
fetch, which fires from `table-view-display' before the buffer-local handle
is stored."
  (with-current-buffer buf
    (cond ((eq (car table-view-native--conn-handle) conn) (cdr table-view-native--conn-handle))
          ((eq conn0 conn) handle0)
          (t (table-view-native--open conn)))))

(defun table-view-native--page-fn (buffer conn0 handle0)
  "Return a `page-fn' closure fetching subscribed windows for BUFFER.
The closure resolves the live connection and handle at call time, so it
survives a backend respawn; CONN0/HANDLE0 bootstrap the first fetch."
  (lambda (req)
    (let* ((buf (plist-get req :buffer))
           (conn (table-view-native--ensure-connection)))
      (ignore buffer)
      (if (not conn)
          (table-view-page-error buf "native backend unavailable")
        (jsonrpc-async-request
         conn 'window
         (list :handle (table-view-native--live-handle buf conn conn0 handle0)
               :offset (or (plist-get req :offset) 0)
               :limit (plist-get req :limit)
               :sort (vconcat (mapcar (lambda (ka) (vector (car ka) (and (cdr ka) t)))
                                      (plist-get req :sort)))
               :filter (or (plist-get req :filter) "")
               :subscribe t)
         :success-fn
         (lambda (reply)
           (with-current-buffer buf
             (setq table-view-native--rev (plist-get reply :rev)
                   table-view-native--gen (plist-get reply :gen)))
           (table-view-set-page
            buf (mapcar #'table-view-native--row (append (plist-get reply :rows) nil))
            :total (plist-get reply :matched)
            :offset (plist-get reply :offset)
            :has-next (eq (plist-get reply :hasNext) t)))
         :error-fn
         (lambda (err) (table-view-page-error buf (plist-get err :message)))
         :timeout-fn
         (lambda () (table-view-page-error buf "native backend timeout")))))))

;;; Public entry

(defun table-view-native--wire-source (source)
  "SOURCE with a `rows' payload converted from table-view rows to wire shape.
Other source kinds (e.g. \"gen\", \"file\") pass through untouched."
  (if (equal (plist-get source :kind) "rows")
      (plist-put (copy-sequence source) :rows
                 (vconcat (mapcar #'table-view-native--row->wire
                                  (append (plist-get source :rows) nil))))
    source))

(defun table-view-native--display-now (buffer source spec handlers)
  "Open SOURCE on the backend and display SPEC in BUFFER (the native path)."
  (if-let ((conn (table-view-native--ensure-connection)))
      (let* ((buf (get-buffer-create buffer))
             ;; Capture any prior handle before `table-view-mode' wipes the
             ;; buffer-locals, so re-displaying can close it (no leak).
             (prior (buffer-local-value 'table-view-native--conn-handle buf))
             (wire (table-view-native--wire-source source))
             (pg (alist-get 'pagination spec))
             (page-size (or (alist-get 'page-size pg) 50))
             (open (jsonrpc-request
                    conn 'open
                    (list :source wire
                          :columns (table-view-native--columns (table-view--own-spec spec))
                          :pageSize page-size
                          :protocol table-view-native-protocol)))
             (handle (plist-get open :handle))
             ;; force a paged buffer even if the spec omitted `pagination'
             (spec (if pg spec (append spec (list (cons 'pagination
                                                        (list (cons 'page-size page-size)
                                                              (cons 'strategy 'offset))))))))
        (table-view-display buffer spec handlers nil
                            (table-view-native--page-fn buf conn handle))
        (with-current-buffer buf
          (setq-local table-view-native--source wire  ; wire-ready, for respawn re-open
                      table-view-native--conn-handle (cons conn handle)
                      table-view-native--rev (or (plist-get open :rev) 0))
          (puthash handle buf table-view-native--handles)
          (add-hook 'kill-buffer-hook #'table-view-native--on-kill nil t))
        (when prior (table-view-native--close (car prior) (cdr prior)))
        buf)
    (table-view-native--display-fallback buffer source spec handlers)))

(defun table-view-native--display-fallback (buffer source spec handlers)
  "Render SOURCE in pure elisp: inline rows directly, other kinds need the backend."
  (if (equal (plist-get source :kind) "rows")
      (progn
        (table-view-display buffer spec handlers)
        (table-view-set-rows buffer (append (plist-get source :rows) nil))
        (get-buffer buffer))
    (table-view-native--fallback 'unsupported-source)
    (table-view-display buffer spec handlers)))

(defun table-view-native--render-status (buf spec head sub)
  "Show a HEAD/SUB status placeholder for SPEC's title in BUF (before the table)."
  (with-current-buffer buf
    (let ((inhibit-read-only t))
      (fundamental-mode)
      (erase-buffer)
      (insert (propertize (or (alist-get 'title spec) "Table") 'face '(:weight bold :height 1.1)) "\n\n")
      (insert (propertize head 'face 'warning) "\n")
      (insert (propertize sub 'face 'shadow) "\n"))
    (setq buffer-read-only t)
    (goto-char (point-min))))

(defun table-view-native--display-deferred (buffer source spec handlers)
  "Show a build placeholder in BUFFER, build the backend, then load it or show
the error.  The compile runs asynchronously (Emacs stays responsive) with live
output in *tvx-compile*; the real table replaces the placeholder on success."
  (let ((buf (get-buffer-create buffer)))
    (table-view-native--render-status
     buf spec "Building native backend (tvx)…"
     "Compiling with cargo — live output in *tvx-compile*.  The table loads when it finishes.")
    (display-buffer buf)
    (display-buffer (get-buffer-create "*tvx-compile*"))   ; watch progress/logs
    (table-view-native-compile
     nil
     (lambda (path)
       (cond
        ((not (buffer-live-p buf)) nil)          ; user gave up and killed it
        (path (table-view-native--display-now buffer source spec handlers))
        (t (table-view-native--render-status
            buf spec "Native backend build failed"
            "cargo build failed — see *tvx-compile*.  Fix it, then M-x table-view-native-compile and re-open.")))))
    buf))

;;;###autoload
(defun table-view-native-display (buffer source spec &optional handlers)
  "Display SPEC in BUFFER backed by the native tvx over SOURCE.
SOURCE is a plist naming the data the backend should own:
  (:kind \"rows\" :rows ROWS)  inline table-view ((id . ID) (cells . ALIST)) rows;
  (:kind \"gen\"  :n N)        N synthetic rows generated in the backend.

When the backend must first be built (`table-view-native-auto-compile'), the
display is *deferred*: BUFFER shows a build placeholder, cargo runs
asynchronously with live output in *tvx-compile*, and the table loads once the
build succeeds (or shows the error if it fails).  When the backend is
unavailable and no build happens, falls back to the pure-elisp path with a
warning (a \"rows\" source still renders; others need the backend).  Returns
the buffer."
  (cond
   ;; Ready now: a live connection, or a validated binary to connect to.
   ((or (and table-view-native--connection (jsonrpc-running-p table-view-native--connection))
        (and table-view-native-enabled (table-view-native--resolve)))
    (table-view-native--display-now buffer source spec handlers))
   ((not table-view-native-enabled)
    (table-view-native--display-fallback buffer source spec handlers))
   ;; A build is already running (e.g. a second table): join it, then load.
   (table-view-native--build-in-progress
    (table-view-native--display-deferred buffer source spec handlers))
   ((not (table-view-native--cargo))
    (table-view-native--fallback 'no-cargo)
    (table-view-native--display-fallback buffer source spec handlers))
   ;; Buildable: decide per policy, and on a build defer the display.
   (t (pcase table-view-native-auto-compile
        ('nil (table-view-native--fallback 'no-binary)
              (table-view-native--display-fallback buffer source spec handlers))
        ('t (table-view-native--display-deferred buffer source spec handlers))
        (_ (if (y-or-n-p "Build the table-view native backend now (~30s)? ")
               (table-view-native--display-deferred buffer source spec handlers)
             (table-view-native--fallback 'no-binary)
             (table-view-native--display-fallback buffer source spec handlers)))))))

;;; Live mutation + queries

(defun table-view-native-patch (buffer &rest args)
  "Upsert and delete rows in native table-view BUFFER.
ARGS is a plist: :upserts ROWS (table-view ((id . ID) (cells . ALIST)) rows)
and :deletes IDS.  The backend applies them, bumps its rev, and pushes a
$/delta that updates the subscribed window."
  (let ((buf (get-buffer buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((conn (car table-view-native--conn-handle))
              (handle (cdr table-view-native--conn-handle)))
          (when (and conn (jsonrpc-running-p conn))
            (jsonrpc-async-request
             conn 'patch
             (list :handle handle
                   :upserts (vconcat (mapcar #'table-view-native--row->wire (plist-get args :upserts)))
                   :deletes (vconcat (plist-get args :deletes))))))))))

(defun table-view-native-count (buffer &optional filter)
  "Return the number of rows in native table-view BUFFER matching FILTER."
  (with-current-buffer (get-buffer buffer)
    (plist-get (jsonrpc-request (car table-view-native--conn-handle) 'count
                                (list :handle (cdr table-view-native--conn-handle)
                                      :filter (or filter "")))
               :matched)))

(defun table-view-native-aggregate (buffer column op &optional filter)
  "Aggregate COLUMN of native table-view BUFFER with OP under FILTER.
OP is one of \"sum\", \"min\", \"max\", \"avg\", \"count\" (a string)."
  (with-current-buffer (get-buffer buffer)
    (plist-get (jsonrpc-request (car table-view-native--conn-handle) 'aggregate
                                (list :handle (cdr table-view-native--conn-handle)
                                      :column column :op op :filter (or filter "")))
               :value)))

(defun table-view-native-reset ()
  "Clear native fallback/crash state and rebuild the backend."
  (interactive)
  (clrhash table-view-native--warned)
  (when (and table-view-native--connection (jsonrpc-running-p table-view-native--connection))
    (jsonrpc-shutdown table-view-native--connection))
  (setq table-view-native--connection nil)
  (table-view-native-compile t))

;;; Auto-routing from `table-view-display'

(defun table-view-native-available-p ()
  "Return non-nil when the native backend is enabled and its binary resolves."
  (and table-view-native-enabled (table-view-native--resolve) t))

(defun table-view-native--auto-display (buffer spec handlers)
  "Route a large inline-rows SPEC through the native backend when it is ready.
Registered as `table-view--native-display-function'.  Returns non-nil when it
displayed natively; nil (after recommending a build) to let the elisp path run."
  (if (table-view-native-available-p)
      (prog1 t
        (table-view-native-display buffer
                                   (list :kind "rows" :rows (alist-get 'rows spec))
                                   spec handlers))
    (table-view-native--fallback 'recommend-build (length (alist-get 'rows spec)))
    nil))

;; Loading table-view-native opts every large `table-view-display' into the
;; native backend (when its binary is available); the core stays standalone.
(setq table-view--native-display-function #'table-view-native--auto-display)

(provide 'table-view-native)
;;; table-view-native.el ends here
