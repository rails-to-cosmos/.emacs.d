# Package supply-chain hardening

Proposal date: 2026-11-07
Status: implementation added; live archive migration blocked by rehearsal failures
Scope: `src/mijn-package-policy.el`, `src/mijn-packages.el`, new `scripts/package-update`, new usage-tracking package

## Implementation status (2026-11-07)

Archive priorities and strict signatures are configured. The bootstrap declarations include
`gnu-elpa-keyring-update`. Eat is declared alongside vterm for the trial; existing vterm
integrations remain in use.

`lock/packages.el` records the current installed tree. Its SHA256 covers relative filenames
and file bytes, including compiled files; it is a drift detector, not a download checksum or
a reproducible-build guarantee. Historical archive provenance is `unknown`. Successful staged
updates record provenance in `elpa/origins.json`. The lockfile does not reconstruct packages
that an archive has since removed.

Commands:

```sh
make package-upgrade                      # fetch, review, approve, install and validate
make package-lock
make package-verify
scripts/package-update fetch             # upgrades only
scripts/package-update fetch --rebuild   # preferred archives, including downgrades
scripts/package-update diff
scripts/package-update report
scripts/package-update apply
scripts/package-update rollback
make test-package-security
```

`make package-upgrade` requires an interactive terminal. It verifies the current lockfile,
fetches upgrades under the archive policy, and prints the triage report and full-diff location.
Review all diffs, then type `apply` to install the staged set. Enter leaves it staged.
The command holds the update lock throughout review and rejects changes to the staged
manifest before installation. Installation, validation, rollback, and lock regeneration use
the existing `apply` workflow. Failed verification or validation stops the upgrade; the
documented startup blockers must be resolved before an upgrade can succeed.

Fetch and installation run in the integration Podman image with copied inputs and no host
mounts. Fetch validates signed archive metadata and signed package downloads before staging.
A failed archive refresh aborts instead of using old metadata. Staged hashes are checked
before diff or apply. Apply refuses dependencies absent from staging. Snapshot archives are
written outside the repository; a failed validation restores both the package tree and lock.
The host validation commands execute the reviewed packages after the swap. Restart Emacs
after apply or rollback: already loaded definitions cannot be replaced by a filesystem swap.
The Linux container's native artifacts must also be compatible with the host.

`M-x package-usage-report` shows installed packages and distinct invoked command symbols.
Interactive startup enables collection; batch checks do not. Per-session records merge under
an exclusive lock and atomic rename. Retained per-session `.eld` journals in
`var/package-usage.eld.sessions/` preserve evidence when the merge lock is busy at shutdown.
A stale `.eld.lock` directory can be removed after confirming no writer is active. The report's candidates need passive-role
review before removal. Usage persistence does not send network requests.

Validation so far:

- Thirteen Python security tests and three usage ERT tests pass, including upgrade review gates, snapshot restoration,
  staged-byte tampering, archive traversal, and concurrent-session persistence.
- Archive-policy ERT tests pass; the policy and usage modules byte-compile cleanly.
- `make package-verify` passes against the recorded live tree.
- Full local ERT: 78/79 pass; the Darwin modifier prior-state test fails.
- Full typecheck fails in existing Org-dependent modules with `org-complete-structure` unbound.
- Fresh Podman startup rejects the signatures on `exec-path-from-shell-2.2` and
  `rainbow-delimiters-2.1.5`, even after installing the keyring update. It then aborts at
  the external `repos` require. Signature checks remain strict.
- The preferred-source fetch also encountered a GNU archive signature retrieval failure
  and correctly aborted. A retry passed metadata verification but was stopped during slow
  package downloads after the startup blockers were confirmed. No staged rebuild remains.
  A complete live rebuild/apply has not been validated or performed.

The live tree remains in place. The month-long pruning review, week-long eat trial, archive
publication of the usage package, and removal of vterm/build dependencies remain pending.

## Measured baseline at proposal time

| Property                                                    | Value                      |
|-------------------------------------------------------------|----------------------------|
| Installed packages                                          | 128                        |
| Packages with a verified-signature marker (`elpa/*.signed`) | 7                          |
| `package-check-signature`                                   | `allow-unsigned` (default) |
| `package-archive-priorities`                                | `nil`                      |
| `package-pinned-packages`                                   | `nil`                      |
| Lockfile                                                    | none                       |

Archives that publish signatures, from the local `elpa/archives/` cache:

| Archive         | `archive-contents.signed` present |
|-----------------|-----------------------------------|
| gnu             | yes                               |
| nongnu          | yes                               |
| melpa           | no                                |
| org             | no                                |
| rails-to-cosmos | no                                |

`https://stable.melpa.org/packages/archive-contents.sig` returns 404. MELPA publishes no
signatures on either channel.

Availability of the 128 installed packages across the signed-or-controlled archives:

| Source             | Packages available |
|--------------------|--------------------|
| stable.melpa.org   | 98                 |
| gnu                | 23                 |
| nongnu             | 30                 |
| rails-to-cosmos    | 10                 |
| union of the above | 115                |

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

- [x] Replace the `melpa` entry with `https://stable.melpa.org/packages/`, keeping
      MELPA-unstable as a separate low-priority archive named `melpa-unstable`.
- [x] Drop the `org` archive. GNU ELPA carries org, is signed, and removing the entry
      removes an unsigned source.
- [x] Set `package-archive-priorities`:

      rails-to-cosmos  100
      gnu               50
      nongnu            40
      melpa-stable      20
      melpa-unstable     0

- [x] Set `package-check-signature` to `all`.
- [x] Set `package-unsigned-archives` to `("melpa-stable" "melpa-unstable" "rails-to-cosmos")`.
- [x] Declare `gnu-elpa-keyring-update` in `mijn-packages.el`. Expired keys turn into hard
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

- [x] Write `lock/packages.el`: an alist of `(name version archive sha256)` per installed
      package, sorted by name, one entry per line.
- [x] `make package-lock` regenerates it from the live `elpa/` tree.
- [x] `make package-verify` fails when the tree and the lockfile disagree.
- [x] Commit the lockfile. `*.eld` is gitignored, hence `.el`.

The lockfile turns updates into discrete reviewable events and gives the podman matrix a
specific package set to test.

## Phase 3 — Update pipeline

New `scripts/package-update`, one entry point with subcommands:

- [x] `fetch` — refresh archive contents, compute the upgradeable set, download each new
      tarball to a staging directory. No install, so no package code runs.
- [x] `diff` — unpack staged tarballs and diff each against the installed tree, producing
      one unified diff per package.
- [x] `report` — print the triage summary from Phase 4.
- [x] `apply` — run the staged install **inside podman**, reusing the image from
      `test/integration/`, so install-time byte-compilation never executes on the host.
      Export the resulting tree, snapshot the live `elpa/`, swap the new tree in, then run
      `make typecheck && make test-unit && make startup-test`. Regenerate the lockfile on
      success; restore the snapshot and exit non-zero on failure.
- [x] `rollback` — restore the most recent snapshot.

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

- [x] Flag added lines touching process creation (`call-process`, `start-process`,
      `shell-command`, `make-process`), network (`url-retrieve`, `open-network-stream`,
      `make-network-process`), filesystem writes outside the package tree, `eval` of
      constructed or decoded data (`intern`, `base64-decode-string`, `read` on process
      output), and top-level side effects in autoload-generating positions.
- [x] Rank packages by flag count and print the top entries with their diffs inline.
- [x] Report "N packages updated, M flagged" and keep the full diffs on disk for reading.

Keep this to one file and a flat pattern table. The pipeline carries the security; this part
only orders your reading.

## Phase 5 — Usage tracking

Purpose: 128 packages is 128 trusted upstreams. Evidence of what goes unused justifies
pruning, and pruning is the only control that removes risk outright.

New `src/mijn-package-usage.el`, promoted to the rails-to-cosmos archive as `package-usage`
once it settles.

- [x] Resolve loaded files to owning packages by matching `load-history` entries against
      `package-user-dir` paths.
- [x] Record per package: sessions in which it loaded, first and last seen, and whether it
      loaded eagerly or through an autoload.
- [x] Advise `call-interactively` to record invoked command symbols, mapping each back to its
      package through `symbol-file`. Load alone is weak evidence, since the config `require`s
      many packages eagerly; an invoked command is strong evidence.
- [x] Persist to `var/package-usage.eld` on `kill-emacs-hook` and on an idle timer, merging
      rather than overwriting so concurrent Emacs instances do not lose data. `*.eld` is
      already gitignored.
- [x] `M-x package-usage-report` renders through `table-view`: package, archive, sessions
      loaded, commands invoked, last used, declared in config, removal candidate.
- [x] Local file only, no network.

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
`vterm-shell`, `vterm-kill-buffer-on-exit`, `vterm-mode`). [makeel.md](makeel.md) moves it out
to its own package and rebuilds it on `compilation-start` with comint, which was measured to
give a PTY on Emacs 31.1. Once that lands, nothing in the config depends on a terminal
emulator and vterm drops to interactive `M-x` use alone, which eat covers.

Eat's newest release is 0.9.4 from March 2024, so upstream has been quiet for over two
years.

Recommendation: `eat`. It is the only option that shrinks the native-code surface, and it
converts the terminal from an unsigned MELPA-unstable dependency into a signed nongnu one.
ghostel is the better terminal and the wrong trade here.

- [ ] Prototype `eat` alongside vterm, both installed, and use it for a week.
- [ ] Land [makeel.md](makeel.md) first, which removes the only vterm-internals caller.
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
