#!/usr/bin/env bash
# Runs inside the integration container. Copies the bind-mounted repo into a
# writable ~/.emacs.d, provisions packages by loading the real init, then runs
# the smoke test, the byte-compile warning gate, and the ERT suite.
#
# Env:
#   EMACS_CONFIG_STRICT=1  make the warning gate fatal (default: report only)
set -euo pipefail

SRC=/src
CFG="$HOME/.emacs.d"
CACHE=/elpa-cache

echo "==> Emacs: $(emacs --version | head -1)"

echo "==> Syncing repo into $CFG (excluding host build artifacts)"
mkdir -p "$CFG"
rsync -a --delete \
  --exclude '.git' \
  --exclude 'elpa' \
  --exclude 'eln-cache' \
  --exclude '*.elc' \
  --exclude 'auto-save-list' \
  --exclude 'tramp' \
  "$SRC"/ "$CFG"/

# Reuse a per-version package cache when the caller mounted one.
if [ -d "$CACHE/elpa" ]; then
  echo "==> Restoring cached elpa"
  cp -a "$CACHE/elpa" "$CFG/elpa"
fi

cd "$CFG"

# Persist whatever got installed on ANY exit, not only success. Provisioning is
# the slow part; caching it even when a later gate fails keeps reruns fast.
save_cache() {
  if [ -d "$CACHE" ] && [ -d "$CFG/elpa" ]; then
    echo "==> Saving elpa cache"
    rm -rf "$CACHE/elpa.tmp" "$CACHE/elpa"
    cp -a "$CFG/elpa" "$CACHE/elpa"
  fi
}
trap save_cache EXIT

echo "==> [1/3] Load full init (provision packages + smoke test)"
emacs --batch -l test/integration/load-check.el

echo "==> [2/3] Byte-compile our src (warning gate)"
if [ "${EMACS_CONFIG_STRICT:-0}" = "1" ]; then
  make typecheck-strict EMACS=emacs
else
  # Report warnings but do not fail the run.
  make typecheck EMACS=emacs || true
fi

echo "==> [3/3] ERT unit tests"
make test EMACS=emacs

echo "==> ALL GREEN on $(emacs --version | head -1)"
