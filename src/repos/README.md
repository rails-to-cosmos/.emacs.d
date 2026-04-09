# repos.el

Git repository dashboard for Emacs. Monitors multiple repos asynchronously and shows their status in an org-mode buffer.

## Usage

`C-x y p` opens the `*repos*` dashboard.

## Statuses

| Status | Meaning |
|---|---|
| CHECKING | Fetching remote status |
| FETCHING | Running git fetch |
| BEHIND | Commits behind upstream |
| MODIFIED | Staged or unstaged changes |
| UNTRACKED | Only untracked files (clean otherwise) |
| MISSING | Local directory not found (has remote URL) |
| ERROR | Git error |
| UP_TO_DATE | Clean, up to date |

The `[/]` cookie in the heading tracks resolved vs in-progress repos.

## Keybindings

| Key | Action |
|---|---|
| `g` | Refresh repo at point (or all) |
| `G` | Refresh all |
| `f` | Pull repo at point |
| `F` | Pull all |
| `c` | Clone missing repo at point |
| `C` | Clone all missing repos |
| `+` | Add repo(s) by directory |
| `RET` | Open magit-status |
| `^` | Cycle sort (status / name / path) |
| `~` | Toggle sort direction (asc / desc) |
| `/` | Search buffer |
| `q` | Quit |

## Sorting

Sort by status follows the `#+TODO` header order. During refresh-all, repos update in-place without shuffling. A full sorted re-render happens once all repos reach a resolved state.

## Configuration

```elisp
;; Extra repo files (loaded but not written to by save)
(setq repos-extra-files '("~/private/repos"))
```

Repos are stored as alist entries `(PATH . REMOTE-URL)`. The primary file defaults to `~/.emacs.d/repos`. Reads both `repos-list` and legacy `scratch-repos` variable names.

## Haskell Backend

The dashboard uses a compiled Haskell backend (`src/repos/backend/`) for git operations. On first use, if the binary is missing, you'll be prompted to build it via `cabal build -O2`.
