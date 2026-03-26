# Changelog

## 2026-03-26

- Refactored from mijn-scratch.el into standalone repos.el package
- Renamed all `scratch-*` internals to `repos-*`
- Dashboard renders in dedicated `*repos*` buffer instead of `*scratch*`
- Added `UNTRACKED` status (done state, for repos with only untracked files)
- Added sorting: cycle with `^` (status/name/path), toggle direction with `~`
- Added `[/]` org cookie on heading for progress tracking
- Backward compat: reads both `repos-list` and `scratch-repos` from files
- Added `C` to clone all missing repos at once
- Added `f`/`F` for pull repo / pull all
- Keybinding: `C-x y p` opens the dashboard

## 2026-03-25

- Initial implementation as part of mijn-scratch.el
- Async git fetch/status via process sentinels
- Support for extra repo files (private repos not committed to github)
- Clone missing repos with `c`
- MISSING state with remote URL tracking
- Migration from flat string list to alist format
