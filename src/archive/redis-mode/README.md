# redis-mode

Emacs Redis client backed by a native Rust module (the [`redis`](https://crates.io/crates/redis) crate).

Two front-ends:

- **REPL** (`M-x redis-connect`) — type Redis commands, see RESP responses pretty-printed.
- **Key browser** (`M-x redis-browse-keys`) — `SCAN`-driven key list with type tags and value previews.

## Requirements

- Emacs 28+ built with `--with-modules`.
- Rust toolchain (`cargo`) to build the native module.
- A reachable Redis 5+ server.

## Setup

```elisp
(add-to-list 'load-path (expand-file-name "src/redis-mode" user-emacs-directory))
(require 'redis-mode)
```

## Building the native module

```sh
cd src/redis-mode/rust
cargo build --release
```

Produces `rust/target/release/libredis_mod.{so,dylib}`, loaded on the first
`redis-connect` / `redis-browse-keys` call.

Verify:

```elisp
(load-file (expand-file-name "src/redis-mode/redis-mode.el" user-emacs-directory))
(redis--ensure-native)            ;; should not error
(fboundp 'redis-mod-ping)         ;; => t
```

## Connection URLs

Standard `redis://[user:password@]host[:port][/db]` — the `redis` crate parses
auth and DB index for free, e.g.:

```
redis://127.0.0.1:6379/0
redis://default:secret@cache.internal:6379/2
```

Default is `redis-default-url` (`redis://127.0.0.1:6379/0`).

TLS, Sentinel, Cluster, RESP3 push, and pub/sub are **not** supported in v1.

## REPL keybindings

| Key       | Action                              |
|-----------|-------------------------------------|
| `RET`     | Send command                        |
| `M-p`/`M-n` | Walk command history              |
| `C-c C-c` | Clear buffer                        |
| `C-c C-b` | Open the key browser                |
| `C-c C-k` | Drop the cached connection          |

Commands are split with shell-style quoting (`shlex`), so `SET hello "a b"`
sends a single `"a b"` argument.

## Browser keybindings

| Key     | Action                              |
|---------|-------------------------------------|
| `n`/`p` | Next / previous line                |
| `g`     | Restart SCAN                        |
| `m`     | Fetch next SCAN page                |
| `/`     | Set MATCH pattern                   |
| `v`/`RET` | View value preview                |
| `q`     | Quit                                |
| `?`     | Help                                |

Page size is `redis-scan-page-size` (default 100), preview size is
`redis-preview-max-items` (default 16).
