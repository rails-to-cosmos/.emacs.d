;;; mijn-lisette.el --- Lisette language support -*- lexical-binding: t; -*-

(require 'eglot)

(defvar lisette-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?/ ". 124" st)
    (modify-syntax-entry ?* ". 23b" st)
    (modify-syntax-entry ?\n ">" st)
    (modify-syntax-entry ?\" "\"" st)
    (modify-syntax-entry ?\' "\"" st)
    (modify-syntax-entry ?_ "_" st)
    (modify-syntax-entry ?# "." st)
    (modify-syntax-entry ?| "." st)
    (modify-syntax-entry ?> "." st)
    (modify-syntax-entry ?< "." st)
    (modify-syntax-entry ?& "." st)
    (modify-syntax-entry ?= "." st)
    (modify-syntax-entry ?+ "." st)
    (modify-syntax-entry ?- "." st)
    (modify-syntax-entry ?{ "(}" st)
    (modify-syntax-entry ?} "){" st)
    (modify-syntax-entry ?\[ "(]" st)
    (modify-syntax-entry ?\] ")[" st)
    st))

(defconst lisette--keywords
  '("if" "else" "match" "while" "for" "return" "break" "continue"
    "loop" "defer" "try" "recover" "task" "select" "in" "as"))

(defconst lisette--declaration-keywords
  '("fn" "struct" "enum" "interface" "impl" "type"
    "const" "var" "let" "mut" "pub" "import" "package"))

(defconst lisette--type-keywords
  '("int" "uint" "float32" "float64" "complex" "string" "bool" "error"
    "i8" "i16" "i32" "i64" "u8" "u16" "u32" "u64" "f32" "f64"
    "byte" "rune" "usize" "isize" "void"))

(defconst lisette--constants
  '("true" "false" "Some" "None" "Ok" "Err" "nil"))

(defconst lisette--font-lock-keywords
  (let ((kw-re (regexp-opt lisette--keywords 'symbols))
        (decl-re (regexp-opt lisette--declaration-keywords 'symbols))
        (type-re (regexp-opt lisette--type-keywords 'symbols))
        (const-re (regexp-opt lisette--constants 'symbols)))
    `(
      (,(rx "#[" (*? nonl) "]") . font-lock-preprocessor-face)
      ("///.*$" . font-lock-doc-face)
      (,decl-re . font-lock-keyword-face)
      (,kw-re . font-lock-keyword-face)
      (,type-re . font-lock-type-face)
      (,const-re . font-lock-constant-face)
      ("\\b\\(fn\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-function-name-face))
      ("\\b\\(struct\\|enum\\|interface\\|type\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
      ("\\b\\(impl\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)"
       (1 font-lock-keyword-face)
       (2 font-lock-type-face))
      ("\\b[A-Z][a-zA-Z0-9_]*\\b" . font-lock-type-face)
      ("\\b0[xX][0-9a-fA-F_]+\\b" . font-lock-constant-face)
      ("\\b0[oO][0-7_]+\\b" . font-lock-constant-face)
      ("\\b0[bB][01_]+\\b" . font-lock-constant-face)
      ("\\b[0-9][0-9_]*\\(?:\\.[0-9_]+\\)?\\(?:[eE][+-]?[0-9_]+\\)?i?\\b" . font-lock-constant-face)
      ("|>" . font-lock-builtin-face)
      ("->" . font-lock-builtin-face)
      ("=>" . font-lock-builtin-face)
      ("\\.\\." . font-lock-builtin-face))))

(defvar lisette-mode-map
  (make-sparse-keymap))

;;;###autoload
(define-derived-mode lisette-mode prog-mode "Lisette"
  "Major mode for editing Lisette source files."
  :syntax-table lisette-mode-syntax-table
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (setq-local indent-tabs-mode nil)
  (setq-local tab-width 4)
  (setq-local font-lock-defaults '(lisette--font-lock-keywords)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.lis\\'" . lisette-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs '(lisette-mode . ("lis" "lsp"))))

(provide 'mijn-lisette)
;;; mijn-lisette.el ends here
