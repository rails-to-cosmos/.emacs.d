# parquet-mode

Emacs major mode for viewing Apache Parquet files with lazy pagination.

## Requirements

- [uv](https://docs.astral.sh/uv/) — Python package runner (used to execute the helper script with inline dependencies)
- Emacs 28+
- `org-mode` (built-in)

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
