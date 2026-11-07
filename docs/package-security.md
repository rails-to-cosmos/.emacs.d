# Package supply-chain hardening

Proposal date: 2026-11-07
Status: proposed, nothing implemented
Scope: `src/mijn-package-policy.el`, `src/mijn-packages.el`, new `scripts/package-update`, new usage-tracking package

## Measured current state

| Property | Value |
|---|---|
| Installed packages | 128 |
| Packages with a verified-signature marker (`elpa/*.signed`) | 7 |
| `package-check-signature` | `allow-unsigned` (default) |
| `package-archive-priorities` | `nil` |
| `package-pinned-packages` | `nil` |
| Lockfile | none |

Archives that publish signatures, from the local `elpa/archives/` cache:

| Archive | `archive-contents.signed` present |
|---|---|
| gnu | yes |
| nongnu | yes |
| melpa | no |
| org | no |
| rails-to-cosmos | no |

`https://stable.melpa.org/packages/archive-contents.sig` returns 404. MELPA publishes no
signatures on either channel.

Availability of the 128 installed packages across the signed-or-controlled archives:

| Source | Packages available |
|---|---|
| stable.melpa.org | 98 |
| gnu | 23 |
| nongnu | 30 |
| rails-to-cosmos | 10 |
| union of the above | 115 |

Thirteen packages exist only on MELPA-unstable:

    dired-hacks-utils  dired-narrow  dired-rainbow  flycheck-mypy
    flycheck-nim       flymake-ruff  highlight-doxygen  ob-mermaid
    ox-reveal          ox-reveal-layouts  ruff-format  vterm
    yasnippet-capf

## Threat model

MELPA-unstable builds from upstream git master, and its version numbers are build
timestamps. A commit pushed to any of ~128 upstream repositories reaches the next
`package-upgrade-all` with no review in between. Compromise of an upstream repository or a
maintainer account is the realistic attack. Emacs Lisp runs unsandboxed with full user
privileges.

Archive transport is already covered by TLS. Typosquatting applies at first install only.

One constraint shapes the design: `package-install` byte-compiles, which expands macros and
evaluates `eval-when-compile` and `eval-and-compile` bodies. Package code therefore runs at
install time, before anything loads it. Inspection has to happen on the downloaded archive
entry, ahead of install.

## Phase 1 — Archive policy

Change `src/mijn-package-policy.el`:

- [ ] Replace the `melpa` entry with `https://stable.melpa.org/packages/`, keeping
      MELPA-unstable as a separate low-priority archive named `melpa-unstable`.
- [ ] Drop the `org` archive. GNU ELPA carries org, is signed, and removing the entry
      removes an unsigned source.
- [ ] Set `package-archive-priorities`:

      rails-to-cosmos  100
      gnu               50
      nongnu            40
      melpa-stable      20
      melpa-unstable     0

- [ ] Set `package-check-signature` to `all`.
- [ ] Set `package-unsigned-archives` to `("melpa-stable" "melpa-unstable" "rails-to-cosmos")`.
- [ ] Declare `gnu-elpa-keyring-update` in `mijn-packages.el`. Expired keys turn into hard
      install failures, so the keyring needs to stay current.

Effect: the 53 packages available from gnu or nongnu install under enforced signatures,
against 7 verified today. Your own archive wins every contest. MELPA-unstable serves only
the 13 packages nothing else carries.

Priorities select the archive before version comparison, so the 13 keep resolving from
MELPA-unstable while everything else moves to a higher-priority source.

### Migration cost

Installed MELPA-unstable versions are timestamps (`magit-20250118.2056`), numerically far
above stable semver (`magit-4.4.1`). Nothing downgrades on its own, so the switch needs a
rebuild of `elpa/` rather than an upgrade. `elpa/` is gitignored and `make startup-test`
already provisions a config from scratch inside podman, so a rebuild is cheap to rehearse
before doing it for real.

- [ ] Rehearse: `make startup-test` under the new policy, confirming all 128 resolve.
- [ ] Rebuild: snapshot `elpa/`, delete it, start Emacs, compare the resulting set against
      the snapshot.

## Phase 2 — Lockfile

- [ ] Write `lock/packages.el`: an alist of `(name version archive sha256)` per installed
      package, sorted by name, one entry per line.
- [ ] `make package-lock` regenerates it from the live `elpa/` tree.
- [ ] `make package-verify` fails when the tree and the lockfile disagree.
- [ ] Commit the lockfile. `*.eld` is gitignored, hence `.el`.

The lockfile turns updates into discrete reviewable events and gives the podman matrix a
specific package set to test.

## Phase 3 — Update pipeline

New `scripts/package-update`, one entry point with subcommands:

- [ ] `fetch` — refresh archive contents, compute the upgradeable set, download each new
      tarball to a staging directory. No install, so no package code runs.
- [ ] `diff` — unpack staged tarballs and diff each against the installed tree, producing
      one unified diff per package.
- [ ] `report` — print the triage summary from Phase 4.
- [ ] `apply` — run the staged install **inside podman**, reusing the image from
      `test/integration/`, so install-time byte-compilation never executes on the host.
      Export the resulting tree, snapshot the live `elpa/`, swap the new tree in, then run
      `make typecheck && make test-unit && make startup-test`. Regenerate the lockfile on
      success; restore the snapshot and exit non-zero on failure.
- [ ] `rollback` — restore the most recent snapshot.

Snapshots belong in `~/.cache/emacs-package-snapshots/<timestamp>.tar.zst`, outside the
repository. The earlier untracked `package-backups/` directory should stay gone.

Running `apply` in a container closes the install-time execution hole that motivates
Phase 4: package code compiles under podman, and only inert files reach the host. The
container needs the package archives over the network and nothing else — no host mounts
beyond the export path.

## Phase 4 — Diff triage

A pattern matcher over Emacs Lisp cannot be sound. `(funcall (intern (concat "shell-"
"command")) ...)` defeats any pattern list, and scanning whole trees produces noise, because
Magit spawns processes, LSP opens sockets, and much legitimate code calls `eval`. Treated as
a verdict machine it would teach you to ignore its output.

It earns a place under one restriction: run it over **added lines in the update diff only**,
and let it rank review attention rather than grant approval.

- [ ] Flag added lines touching process creation (`call-process`, `start-process`,
      `shell-command`, `make-process`), network (`url-retrieve`, `open-network-stream`,
      `make-network-process`), filesystem writes outside the package tree, `eval` of
      constructed or decoded data (`intern`, `base64-decode-string`, `read` on process
      output), and top-level side effects in autoload-generating positions.
- [ ] Rank packages by flag count and print the top entries with their diffs inline.
- [ ] Report "N packages updated, M flagged" and keep the full diffs on disk for reading.

Keep this to one file and a flat pattern table. The pipeline carries the security; this part
only orders your reading.

## Phase 5 — Usage tracking

Purpose: 128 packages is 128 trusted upstreams. Evidence of what goes unused justifies
pruning, and pruning is the only control that removes risk outright.

New `src/mijn-package-usage.el`, promoted to the rails-to-cosmos archive as `package-usage`
once it settles.

- [ ] Resolve loaded files to owning packages by matching `load-history` entries against
      `package-user-dir` paths.
- [ ] Record per package: sessions in which it loaded, first and last seen, and whether it
      loaded eagerly or through an autoload.
- [ ] Advise `call-interactively` to record invoked command symbols, mapping each back to its
      package through `symbol-file`. Load alone is weak evidence, since the config `require`s
      many packages eagerly; an invoked command is strong evidence.
- [ ] Persist to `var/package-usage.eld` on `kill-emacs-hook` and on an idle timer, merging
      rather than overwriting so concurrent Emacs instances do not lose data. `*.eld` is
      already gitignored.
- [ ] `M-x package-usage-report` renders through `table-view`: package, archive, sessions
      loaded, commands invoked, last used, declared in config, removal candidate.
- [ ] Local file only, no network.

Known limits worth stating in the report header: themes, fonts and UI packages count as used
without ever being invoked, and a package required eagerly always registers as loaded. The
report advises, and you decide.

## Phase 6 — Prune

- [ ] After a month of usage data, review the removal candidates.
- [ ] Drop the `up` declarations for packages with no invoked commands and no passive role.
- [ ] Run `package-autoremove`, rebuild, regenerate the lockfile.
- [ ] Prefer a gnu or nongnu equivalent where one exists for a surviving
      MELPA-unstable-only package.

## File layout

    src/mijn-package-policy.el     archives, priorities, signature policy
    src/mijn-package-usage.el      usage tracking
    lock/packages.el               committed lockfile
    scripts/package-update         fetch / diff / report / apply / rollback
    var/package-usage.eld          usage state, gitignored
    ~/.cache/emacs-package-snapshots/   rollback snapshots, outside the repo

Each phase stays one file. Update policy belongs in `mijn-package-policy.el`, and
`mijn-packages.el` keeps doing declaration tracking alone.

## Acceptance criteria

| Phase | Done when |
|---|---|
| 1 | `make startup-test` provisions all 128 packages, and `elpa/*.signed` covers every gnu and nongnu package |
| 2 | `make package-verify` passes on a clean tree and fails after a manual version edit |
| 3 | `apply` on a deliberately broken package set rolls back and exits non-zero |
| 4 | A test diff carrying an injected `call-process` line ranks first in the report |
| 5 | `M-x package-usage-report` lists every installed package, and counts survive a restart |
| 6 | Installed package count drops, with the lockfile and tests current |

## Terminal backend

vterm is the highest-value target in the tree. It is a native C module compiled on the host,
it wraps libvterm, and it reaches the config from MELPA-unstable, the one archive with no
review between an upstream commit and your next update.

| Option | Native code | Archive | Signed | Build dependencies |
|---|---|---|---|---|
| vterm (today) | C module | melpa-unstable only | no | libvterm, cmake, libtool |
| ghostel | Zig module | melpa-stable | no | zig 0.15.2+ |
| eat | none | nongnu | yes | none |

libghostty-vt reached 1.0 on 2026-11-06 with a stable C ABI, MIT licensed, needing only libc
at runtime and buildable freestanding. Neovim has an approved pull request swapping libvterm
for it. `ghostel` is the Emacs module built on it, available on stable.melpa, and it claims
Kitty keyboard and graphics protocols, OSC 8 hyperlinks, synchronized output, and roughly
twice vterm's ASCII throughput. Those throughput and protocol figures are self-reported.

Two findings argue against ghostel under this threat model. Its installer can fetch a
pre-built native module with no hash or signature check, which is weaker than vterm's
source-only path, so adoption would have to force source builds and pin the Zig toolchain.
And it remains a native module, so it keeps the install-time compilation surface that
Phases 3 and 4 exist to contain.

`eat` removes native code from the terminal altogether. It is pure Emacs Lisp on nongnu, the
only archive in the new policy that both publishes signatures and gets them enforced. It
drops libvterm, cmake and libtool from all four platform branches of `scripts/bootstrap`,
and deletes the native-module assertion in `scripts/bootstrap.el`. It supports Sixel, full
mouse, and shell integration, and its only dependency is `compat`.

`src/make.el` was the only caller of vterm internals (`vterm--process`, `vterm-mode-map`,
`vterm-shell`, `vterm-kill-buffer-on-exit`, `vterm-mode`). [makel.md](makel.md) moves it out
to its own package and rebuilds it on `compilation-start` with comint, which was measured to
give a PTY on Emacs 31.1. Once that lands, nothing in the config depends on a terminal
emulator and vterm drops to interactive `M-x` use alone, which eat covers.

Eat's newest release is 0.9.4 from March 2024, so upstream has been quiet for over two
years.

Recommendation: `eat`. It is the only option that shrinks the native-code surface, and it
converts the terminal from an unsigned MELPA-unstable dependency into a signed nongnu one.
ghostel is the better terminal and the wrong trade here.

- [ ] Prototype `eat` alongside vterm, both installed, and use it for a week.
- [ ] Land [makel.md](makel.md) first, which removes the only vterm-internals caller.
- [ ] Drop libvterm, cmake and libtool from `scripts/bootstrap`; replace the native-module
      assertion in `scripts/bootstrap.el` with an `eat` smoke test.
- [ ] Remove the vterm declaration from `src/mijn-terminal.el`.
- [ ] Update `docs/bootstrap.org` and `docs/osx-install.org`, which both document the
      libvterm build requirement.

## Open questions

- Whether the 13 MELPA-unstable packages warrant vendoring into `packages/` instead, where
  they would be pinned by commit and reviewed on change.
- Whether eat's two-year release gap is acceptable for a daily-driver terminal, given that
  pure Elisp keeps the failure modes inspectable.
