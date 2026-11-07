# makel — extract the Makefile picker into a package

Proposal date: 2026-11-07
Status: proposed, nothing implemented
Scope: new `~/sync/stuff/makel/`, `~/sync/stuff/elpa/recipes/`, `~/sync/stuff/lab/config/repos.yaml`,
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

`makel` is free on gnu, nongnu, melpa and stable.melpa. Note that `makel` is also an
unrelated Makefile-based lint harness on GitHub (DamienCassou/makel), so the name collides
in search but not in any archive. Prior art worth citing in the README: `makefile-executor`
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
most, and the one no terminal emulator provides. makel depends on built-ins alone, and
neither vterm nor eat appears in `Package-Requires`.

What this gives up is cursor addressing. Targets that run a full-screen TUI break, and `\r`
progress bars render as successive lines. Mitigations: `ansi-color-compilation-filter` for
SGR colors and `comint-watch-for-password-prompt` for password echo suppression. A
`makel-terminal-targets` list could route named targets to `eat` instead, if any target
turns out to need it.

The two-keymap arrangement survives the port. During the run the buffer stays in comint mode
so typed input reaches the process; `makel--sentinel` then installs `makel-done-map` and
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
| `make-completing-read` | `makel-run` |
| `make-switch-buffer` | `makel-switch-buffer` |
| `make-menu` | `makel-menu` |
| `make-kill-process` | `makel-kill-process` |
| `make-mode-line-cleanup-delay` | `makel-mode-line-cleanup-delay` |
| `make-status-{running,ok,fail}-face`, `make-header-face` | `makel-*` |
| `make--*` internals | `makel--*` |

`makel-run` replaces `make-completing-read`, which names its implementation rather than its
effect. The rename is total: no `make-` symbol, file name, feature name or `provide` form
survives, so `(require 'make)` and `(provide 'make)` both go. Nothing outside
`src/make.el` and the two `init.el` lines references the old names.

## Repository layout

Modeled on `~/sync/stuff/agnostic-translate`, the simplest single-file component already in
the archive:

    ~/sync/stuff/makel/
      makel.el          the package; header owns Package-Version and Package-Requires
      packages.json     [{"main": "makel.el", "files": {"makel.el": "."}}]
      Makefile          lint / compile / clean, then -include $(ELPA_TOOLS)/package.mk
      melpa-recipe
      README.md
      LICENSE
      .github/          CI running make check
      test/test-makel.el

Header:

    ;; Package-Version: 0.1.0
    ;; Package-Requires: ((emacs "29.1") (transient "0.4"))

`transient` ships with Emacs 31.1 as a built-in and is also on GNU ELPA, which the new
archive policy signs and enforces. Declaring it explicitly keeps `package-lint` quiet.

- [ ] Create the repo and `git init`; push to `github.com/rails-to-cosmos/makel`, matching
      the other published components.
- [ ] Port `src/make.el` with the rename and the `compilation-start` backend.
- [ ] Add tests for `makel--targets` against a fixture Makefile: multi-target rules
      (`major minor patch:`), `:=` assignments excluded, dot-targets and pattern rules
      excluded. That parser is the part worth testing and is currently untested.

## Wiring

- [ ] `~/sync/stuff/elpa/recipes/makel`:

      (makel :fetcher github :repo "rails-to-cosmos/makel"
             :branch "master" :make-targets ("package"))

- [ ] Add the row to the packages table in `~/sync/stuff/elpa/README.md`.
- [ ] `~/sync/stuff/lab/config/repos.yaml`:

      - dest: ~/sync/stuff/makel
        url: git@github.com:rails-to-cosmos/makel.git
        branch: master
        emacs_package: true

      `modules/emacs/packages.py` picks up every `emacs_package: true` entry, orders them
      through `elpa/bin/packages.py order`, and runs `make -C <source> install-emacs`, so no
      change to that module is needed.

- [ ] `make -C ~/sync/stuff/elpa publish` to build and push the archive.

## Changes to this config

- [ ] Delete `src/make.el`.
- [ ] Replace `init.el:178` `(require 'make)` with a declaration:

      (up makel
        :pin "rails-to-cosmos"
        :bind ("C-x y m" . makel-menu)
        :ensure t)

- [ ] Drop `init.el:202` `(global-set-key (kbd "C-x y m") #'make-menu)`, now carried by the
      declaration.
- [ ] Confirm `make typecheck` no longer compiles `src/make.el` and `SRC_FILES` shrinks.

## Acceptance criteria

| | Done when |
|---|---|
| Package | `make check` passes in `~/sync/stuff/makel`, with `package-lint` clean |
| Parser | Fixture tests cover multi-target rules, `:=` lines, dot-targets, pattern rules |
| Backend | A target invoking `sudo -S` accepts a typed password; a target emitting `gcc` errors supports `next-error` |
| Archive | `package-install 'makel` works from a clean profile against rails-to-cosmos |
| Lab | `lab` bootstrap installs makel on a fresh profile with no manual step |
| Config | `src/make.el` is gone, `C-x y m` still opens the menu |

## Open questions

- Whether the transient menu earns its dependency for two commands, or whether `makel-run`
  and `makel-switch-buffer` should stand alone on their own bindings.
- Whether the global mode-line tracker belongs in the package or stays config-local. It
  writes to `global-mode-string` at load time, which is assertive for a library; gating it
  behind `makel-mode-line-mode` would be better manners.
- Whether `makel-terminal-targets` is worth building before a target actually needs it.
