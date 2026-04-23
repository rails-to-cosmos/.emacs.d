# Email in Emacs — proposal

Date: 2026-04-22. Status: proposed. Scope: set up a stable, local-first email workflow for `akatovda@gmail.com` driven entirely from Emacs, with room to add more accounts later.

## Stack

| Layer | Choice | Why |
|---|---|---|
| Transport (fetch) | **mbsync** (`isync` package) | Atomic Maildir sync, resume-safe, per-channel selectivity, very mature |
| Storage | **Maildir** at `~/Maildir/gmail/` | Lock-free, works with every Emacs MUA, trivial to back up |
| Indexer / MUA | **mu4e** (ships with `mu` package) | Fast Xapian index, context/account system, active upstream, good Gmail ergonomics |
| Transport (send) | **msmtp** | Stateless, tiny, delegated by `message-send-mail-with-sendmail`; keeps Emacs unblocked |
| Credentials | **pass** (+ existing `gnupg`) | Password-store is standard on Arch, integrates with `mbsync`/`msmtp` via `PassCmd`/`passwordeval` — no plaintext on disk |
| Scheduling | `mbsync -a && mu index` via cron/systemd user timer or Emacs timer | External tool keeps the Emacs session unblocked |

## Why not the alternatives

- **Gnus**: built-in, but IMAP is famously slow and state-heavy; folder-navigation UX is harsher than mu4e.
- **notmuch**: excellent search, but tag-centric workflow is a bigger mental switch and Gmail's labels-as-folders model maps more naturally to mu4e's maildir-based contexts.
- **offlineimap**: written in Python, historically less robust than mbsync for large Gmail accounts; `mbsync` is the clear winner today.
- **OAuth2 instead of app password**: possible (via `pass` + `oauth2ms` or `oama`), but adds a token-refresh daemon. We defer this — app passwords are fine on a personal machine with disk-encrypted `~/.password-store/`.
- **Emacs-only SMTP** (`smtpmail-send-it`): works, but ties the Emacs main loop to the SMTP call. `msmtp` runs detached and logs separately.

## Directory layout

```
~/Maildir/
  gmail/
    INBOX/
    Sent/
    Drafts/
    All/
    Trash/
    Spam/

~/.mbsyncrc           # mbsync accounts + channels
~/.msmtprc            # msmtp accounts (chmod 600)
~/.password-store/
  email/gmail         # gpg-encrypted app password

~/.emacs.d/
  src/email/
    mijn-email.el         # main loader, defgroup, global bindings
    mijn-email-mu4e.el    # mu4e configuration (folders, shortcuts, view)
    mijn-email-smtp.el    # send-via-msmtp setup
    setup-email.sh        # one-shot install + config skeleton
    sync-email.sh         # mbsync -a && mu index — for cron/systemd-timer
    email-credentials.sh  # `pass insert email/gmail` helper
    email-proposal.md     # this file
```

Add to `init.el`:

```elisp
(add-to-list 'load-path (expand-file-name "src/email" user-emacs-directory))
(require 'mijn-email)
```

## Setup flow (runbook)

1. **Generate a Gmail app password.** Google account → Security → 2-Step Verification → App passwords → create "Emacs mbsync". Note: requires 2FA enabled.
2. **Install system packages.** `src/email/setup-email.sh` runs `sudo pacman -S --needed isync mu msmtp pass gnupg` and initialises `pass` against your default GPG key.
3. **Store the app password.** `src/email/email-credentials.sh` prompts for the value and runs `pass insert email/gmail`.
4. **Verify config.** Script writes `~/.mbsyncrc`, `~/.msmtprc`, creates Maildir skeleton. Idempotent — bails on existing files with a diff unless `--force` is passed.
5. **First sync.** `mbsync -a` (takes a while on a big Gmail inbox — patience). Then `mu init --maildir=~/Maildir --my-address=akatovda@gmail.com` and `mu index`.
6. **Emacs side.** Add the two `init.el` lines above, restart / eval-buffer, then `M-x mu4e`.

## Security

- App password sits in `~/.password-store/email/gmail.gpg`, encrypted to your GPG key. Only the running GPG agent ever decrypts.
- `~/.msmtprc` and `~/.mbsyncrc` contain no secrets — only references (`passwordeval "pass show email/gmail"`).
- `~/.msmtprc` is `chmod 600` (msmtp refuses to run otherwise).
- `~/.authinfo.gpg` not used — we consolidate on `pass` to avoid two parallel credential stores.
- TLS: both `mbsync` and `msmtp` verify with the system CA bundle; `SSLType IMAPS` / `tls on`.

## Open decisions / defer list

- **Multi-account.** Proposal supports one account (`gmail`). The Elisp side already pivots on `mu4e-contexts` so adding another account later is ~20 lines + a new `IMAPAccount` block in `.mbsyncrc`.
- **Auto-sync cadence.** Default: Emacs-side timer at 5 min (`mu4e-update-interval`). Alternative: systemd user timer runs `src/email/sync-email.sh` every 3 min regardless of Emacs state. Not enabled by default — one more moving part; flip it on if you want mail when Emacs isn't running.
- **OAuth2.** Deferred; app password is strictly simpler. Revisit when Google eventually deprecates app passwords.
- **notmuch as a secondary.** `mu` and `notmuch` can share a Maildir; you can run `notmuch new` alongside `mu index` if you ever want tag-based workflow on the same data. Not wired in this iteration.
- **Attachment-dir convention.** mu4e defaults to asking. Could set `mu4e-attachment-dir "~/Downloads/email"` — punted.
- **HTML viewing.** Default to plain text (`mu4e-view-prefer-html nil`); `v` in a message falls back to rendering via `shr` or external browser.

## Future enhancements (future tickets)

| # | Item |
|---|---|
| 1 | OAuth2 for Gmail via `oama` or `oauth2ms`, replacing `pass show email/gmail` in `passwordeval` / `PassCmd`. |
| 2 | systemd user timer wrapping `src/email/sync-email.sh` for background sync when Emacs isn't running. |
| 3 | Per-account mu4e context for a second address (work / alias). |
| 4 | Org-mode capture for emails (`org-mu4e-store-and-capture`), tied into existing `mijn-org` setup. |
| 5 | `mu4e-alert` integration with `dunst` for desktop notifications (we already have dunst working on xmonad). |
| 6 | `consult-mu` bridge for narrowing over mu4e results via vertico/consult. |
| 7 | Sending attachments from `dired`: `C-c RET C-a` in dired → compose with file attached. |
