# makeel — extract the Makefile picker into a package

Proposal date: 2026-11-07
Status: package published to GitHub; archive publication and clean-profile checks pending

Implementation (2026-11-07):
- Created `~/sync/stuff/makeel` on branch `master`, with package metadata,
  CI, README, license, parser fixtures and backend integration tests.
- Ported execution to compilation/comint, retaining the original sentinel.
  Completion uses a read-only map inheriting compilation navigation.
- Added optional `makeel-mode-line-mode`, enabled by this config.
- Added the archive recipe/table row and lab catalog entry; removed `src/make.el`
  and migrated the menu binding to `up makeel`.
- `make check` passes: package-lint, strict byte compilation and four ERT tests.
  The integration test verifies PTY input, failure status, read-only output and
  next-error navigation. A real sudo password prompt remains a manual check.
- Installed locally through `make install-emacs`.
- Config `make typecheck` excludes the removed file, but fails on existing
  `org-complete-structure` errors in mijn-python, mijn-sql and mijn-terminal.
- The original repository was pushed successfully as `rails-to-cosmos/makel`.
  The local package, checkout, recipe and consumers now use `makeel`.
  GitHub's repository has been renamed to `rails-to-cosmos/makeel`, and
  the package rename was pushed on `master` (53ea585).
  Archive publication and fresh lab bootstrap remain unverified.
Scope: new `~/sync/stuff/makeel/`, `~/sync/stuff/elpa/recipes/`, `~/sync/stuff/lab/config/repos.yaml`,
removal of `src/make.el` from this config

## Why extract

`src/make.el` is 320 lines of general-purpose tooling sitting in a personal config. It picks
a target from the dominating Makefile, runs it, tracks every running make in the global mode
line with mouse interaction, and switches between output buffers. Nothing in it is specific
to this configuration.

Two further reasons:

- The `make-` prefix collides with Emacs core. `make-process`, `make-sparse-keymap` and
  dozens more live in that namespace, and `package-lint` rejects a package claiming it. A
  rename is required for publication, independent of taste.
- vterm is the config's last dependency on a native terminal module, and `src/make.el` is
  the only caller of vterm internals. Removing that coupling unblocks the terminal decision
  in [package-security.md](package-security.md).

The package was renamed from `makel` to `makeel` to distinguish it from
the unrelated Makefile lint harness `DamienCassou/makel`.
Prior art worth citing in the README: `makefile-executor`
covers target picking without the mode-line tracking or multiple-buffer handling.

## Backend: drop the terminal emulator

The current design runs `make` inside vterm because the commentary states a requirement:
"the terminal is interactive: sudo passwords and confirmations can be typed in." After the
process exits, `make-done-map` replaces `vterm-mode-map` so keys read the output instead of
feeding a dead terminal. The terminal therefore serves two needs — a PTY during the run, and
a plain read-only buffer afterwards.

Measured on Emacs 31.1: `compilation-start` gives the subprocess a **PTY**, both in plain
`compilation-mode` and with comint enabled. `compile.el` never binds
`process-connection-type`, so the default of `t` applies.

    compilation-mode             PTY
    compilation + comint         PTY

| Backend | Package | PTY | Interactive input | `next-error` | Curses rendering |
|---|---|---|---|---|---|
| vterm (today) | melpa-unstable, native C | yes | yes | no | yes |
| eat | nongnu, pure Elisp | yes | yes | no | yes |
| eshell | built-in | stdout only | yes | no | no |
| compilation + comint | built-in | yes | yes | **yes** | no |

**Decided: `(compilation-start (concat "make " target) t)`.** The comint argument gives
interactive input for sudo prompts and confirmations, and compilation minor mode gives
clickable `file:line` errors and `next-error` navigation — the feature a make runner wants
most, and the one no terminal emulator provides. makeel depends on built-ins alone, and
neither vterm nor eat appears in `Package-Requires`.

What this gives up is cursor addressing. Targets that run a full-screen TUI break, and `\r`
progress bars render as successive lines. Mitigations: `ansi-color-compilation-filter` for
SGR colors and `comint-watch-for-password-prompt` for password echo suppression. A
`makeel-terminal-targets` list could route named targets to `eat` instead, if any target
turns out to need it.

The two-keymap arrangement survives the port. During the run the buffer stays in comint mode
so typed input reaches the process; `makeel--sentinel` then installs `makeel-done-map` and
sets the buffer read-only, exactly as `make-done-map` does against vterm today. Bind `q` to
`quit-window` there rather than `bury-buffer`, matching `compilation-mode`, and keep
`next-error` reachable after exit.

### On eshell

Eshell does give a PTY outside a pipeline, so the suggestion is sound on that point. Three
things argue against it. It routes stderr through a separate `make-pipe-process`, so
programs testing `isatty(2)` see a pipe. It is a shell built for interactive use rather than
programmatic embedding, so driving an eshell buffer to run one command and capture its exit
status costs more code than `compilation-start` and yields no error navigation. And
`eshell-visual-commands` exists precisely because eshell delegates full-screen programs to
`term`, so it is no better than compilation for TUI targets.

## Rename (decided)

| Now | After |
|---|---|
| `make-completing-read` | `makeel-run` |
| `make-switch-buffer` | `makeel-switch-buffer` |
| `make-menu` | `makeel-menu` |
| `make-kill-process` | `makeel-kill-process` |
| `make-mode-line-cleanup-delay` | `makeel-mode-line-cleanup-delay` |
| `make-status-{running,ok,fail}-face`, `make-header-face` | `makeel-*` |
| `make--*` internals | `makeel--*` |

`makeel-run` replaces `make-completing-read`, which names its implementation rather than its
effect. The rename is total: no `make-` symbol, file name, feature name or `provide` form
survives, so `(require 'make)` and `(provide 'make)` both go. Nothing outside
`src/make.el` and the two `init.el` lines references the old names.

## Repository layout

Modeled on `~/sync/stuff/agnostic-translate`, the simplest single-file component already in
the archive:

    ~/sync/stuff/makeel/
      makeel.el          the package; header owns Package-Version and Package-Requires
      packages.json     [{"main": "makeel.el", "files": {"makeel.el": "."}}]
      Makefile          lint / compile / clean, then -include $(ELPA_TOOLS)/package.mk
      melpa-recipe
      README.md
      LICENSE
      .github/          CI running make check
      test/test-makeel.el

Header:

    ;; Package-Version: 0.1.0
    ;; Package-Requires: ((emacs "29.1") (transient "0.4"))

`transient` ships with Emacs 31.1 as a built-in and is also on GNU ELPA, which the new
archive policy signs and enforces. Declaring it explicitly keeps `package-lint` quiet.

- [ ] Create the repo and `git init`; push to `github.com/rails-to-cosmos/makeel`, matching
      the other published components.
- [x] Port `src/make.el` with the rename and the `compilation-start` backend.
- [x] Add tests for `makeel--targets` against a fixture Makefile: multi-target rules
      (`major minor patch:`), `:=` assignments excluded, dot-targets and pattern rules
      excluded. That parser is the part worth testing and is currently untested.

## Wiring

- [x] `~/sync/stuff/elpa/recipes/makeel`:

      (makeel :fetcher github :repo "rails-to-cosmos/makeel"
             :branch "master" :make-targets ("package"))

- [x] Add the row to the packages table in `~/sync/stuff/elpa/README.md`.
- [x] `~/sync/stuff/lab/config/repos.yaml`:

      - dest: ~/sync/stuff/makeel
        url: git@github.com:rails-to-cosmos/makeel.git
        branch: master
        emacs_package: true

      `modules/emacs/packages.py` picks up every `emacs_package: true` entry, orders them
      through `elpa/bin/packages.py order`, and runs `make -C <source> install-emacs`, so no
      change to that module is needed.

- [ ] `make -C ~/sync/stuff/elpa publish` to build and push the archive.

## Changes to this config

- [x] Delete `src/make.el`.
- [x] Replace `init.el:178` `(require 'make)` with a declaration:

      (up makeel
        :pin "rails-to-cosmos"
        :bind ("C-x y m" . makeel-menu)
        :ensure t)

- [x] Drop `init.el:202` `(global-set-key (kbd "C-x y m") #'make-menu)`, now carried by the
      declaration.
- [x] Confirm `make typecheck` no longer compiles `src/make.el` and `SRC_FILES` shrinks.

## Acceptance criteria

| | Done when |
|---|---|
| Package | `make check` passes in `~/sync/stuff/makeel`, with `package-lint` clean |
| Parser | Fixture tests cover multi-target rules, `:=` lines, dot-targets, pattern rules |
| Backend | A target invoking `sudo -S` accepts a typed password; a target emitting `gcc` errors supports `next-error` |
| Archive | `package-install 'makeel` works from a clean profile against rails-to-cosmos |
| Lab | `lab` bootstrap installs makeel on a fresh profile with no manual step |
| Config | `src/make.el` is gone, `C-x y m` still opens the menu |

## Open questions

- Whether the transient menu earns its dependency for two commands, or whether `makeel-run`
  and `makeel-switch-buffer` should stand alone on their own bindings.
- Whether the global mode-line tracker belongs in the package or stays config-local. It
  writes to `global-mode-string` at load time, which is assertive for a library; gating it
  behind `makeel-mode-line-mode` would be better manners.
- Whether `makeel-terminal-targets` is worth building before a target actually needs it.
