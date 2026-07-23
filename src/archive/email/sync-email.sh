#!/usr/bin/env bash
# sync-email.sh — fetch mail with mbsync and reindex with mu.
# Safe to run from cron / systemd user timers / manually.
#
#   ~/.emacs.d/src/email/sync-email.sh
#
# Exits non-zero if any stage fails, so cron/systemd will surface the failure.

set -euo pipefail

LOG_DIR="${XDG_STATE_HOME:-$HOME/.local/state}/mijn-email"
mkdir -p "$LOG_DIR"
LOG="${LOG_DIR}/sync.log"

ts() { date +'%Y-%m-%d %H:%M:%S'; }

{
  echo "[$(ts)] mbsync -a"
  mbsync -a
  echo "[$(ts)] mu index"
  mu index --quiet
  echo "[$(ts)] done"
} >>"$LOG" 2>&1
