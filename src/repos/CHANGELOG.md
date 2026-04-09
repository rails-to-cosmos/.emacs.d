# Changelog

## 2026-04-09

- Added Haskell backend for git operations
- Auto-build prompt: if backend binary is missing, offers to run `cabal build -O2`
- Fixed first-open rendering: dashboard now seeds CHECKING state and renders header before async fetch
- Stable ordering during refresh: repos update in-place, sorted re-render only after all resolve
- Sort by status follows `#+TODO` header order instead of hardcoded priorities
- BEHIND, MODIFIED, MISSING, ERROR are now resolved (done) statuses alongside UP_TO_DATE/UNTRACKED
- Only CHECKING and FETCHING are in-progress statuses

## 2026-03-30

- Repos now move to correct sorted position on state change
- Refresh-all updates in-place during fetch, sorts only when all complete
- Added `repos--sort-before-p` for position-aware updates
- Colored sort label in heading (method in blue, direction in green)

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
