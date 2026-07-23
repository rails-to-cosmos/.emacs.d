#!/usr/bin/env bash
# email-credentials.sh — store the Gmail app password in `pass`.
#
# Run once after setup-email.sh. Never prints the password.

set -euo pipefail

ENTRY="email/gmail"

if ! command -v pass >/dev/null 2>&1; then
  echo "pass not installed — run src/email/setup-email.sh first" >&2
  exit 1
fi

if pass show "$ENTRY" >/dev/null 2>&1; then
  printf 'Entry %s already exists. Overwrite? [y/N] ' "$ENTRY"
  read -r reply
  case "$reply" in
    [yY]) ;;
    *) echo "Aborted."; exit 0 ;;
  esac
  pass rm -f "$ENTRY" >/dev/null
fi

echo "Generate an app password at https://myaccount.google.com/apppasswords"
echo "Paste it when prompted. Input is hidden."
pass insert "$ENTRY"

echo
echo "Stored under pass entry: $ENTRY"
echo "mbsync/msmtp will read it via: pass show $ENTRY"
