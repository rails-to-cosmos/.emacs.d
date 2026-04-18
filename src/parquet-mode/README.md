# parquet-mode

Emacs major mode for viewing Apache Parquet files with lazy pagination.

## Requirements

- Emacs 28+ (must be built with `--with-modules` for the native backend)
- `org-mode` (built-in)
- **Native backend** (preferred): Rust toolchain (`cargo`) — builds a polars-backed
  dynamic module, no subprocess overhead, full polars performance.
- **Fallback backend**: [uv](https://docs.astral.sh/uv/) — runs `parquet-info.py`
  via subprocess if the native module isn't built or `--with-modules` is unavailable.

The elisp tries the native module first and transparently falls back to Python;
nothing to configure.

## Features

- View parquet file schema (column names and types)
- Lazy page-by-page data loading (default 50 rows per page)
- Org table formatting with aligned columns
- Auto-opens `.parquet` and `.parquet.gz` files via `find-file` or dired
- Detects parquet files by magic bytes (`PAR1`)

## Keybindings

| Key       | Action              |
|-----------|---------------------|
| `n` / `p` | Next / previous line |
| `C-S-n`   | Next page           |
| `C-S-p`   | Previous page       |
| `$`       | Last page           |
| `^`       | First page          |
| `g`       | Refresh             |
| `?`       | Show help           |
| `q`       | Quit                |

## Setup

Add `src/parquet-mode` to your `load-path` and require it:

```elisp
(add-to-list 'load-path (expand-file-name "src/parquet-mode" user-emacs-directory))
(require 'parquet-mode)
```

## Building the native module

```sh
cd src/parquet-mode/rust
cargo build --release
```

This produces `rust/target/release/libparquet_mod.{so,dylib}`, which the elisp
loads on first `parquet-open`. Rebuild after updating polars or bumping the
crate version.

Verify it loaded:

```elisp
(parquet--try-load-native)  ; => t
(fboundp 'parquet-mod-info) ; => t
```

If loading fails (e.g. Emacs built without `--with-modules`), the Python
fallback kicks in automatically.
