#!/usr/bin/env bash
# Build and run the Emacs-configuration integration tests across several Emacs
# versions using podman. Each version gets its own image and a persistent
# package-cache volume so repeat runs are fast.
#
# Usage:
#   test/integration/run.sh [VERSION ...]
#
# Defaults to 29.4 30.2 31.1. Honours:
#   EMACS_CONFIG_STRICT=1   fail on unexpected byte-compile warnings
#   ENGINE=docker           use docker instead of podman
set -euo pipefail

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
ENGINE="${ENGINE:-podman}"
STRICT="${EMACS_CONFIG_STRICT:-0}"

VERSIONS=("$@")
if [ "${#VERSIONS[@]}" -eq 0 ]; then
  VERSIONS=(29.4 30.2 31.1)
fi

echo "Engine:   $ENGINE"
echo "Versions: ${VERSIONS[*]}"
echo "Strict:   $STRICT"
echo

declare -A RESULT
fail=0

for v in "${VERSIONS[@]}"; do
  img="emacs-config-test:${v}"
  vol="emacs-config-elpa-${v//./_}"

  echo "############################################################"
  echo "# Emacs ${v}"
  echo "############################################################"

  "$ENGINE" build \
    --build-arg "EMACS_VERSION=${v}" \
    -t "$img" \
    -f "$REPO_ROOT/test/integration/Containerfile" \
    "$REPO_ROOT"

  "$ENGINE" volume inspect "$vol" >/dev/null 2>&1 || "$ENGINE" volume create "$vol" >/dev/null

  if "$ENGINE" run --rm \
      -e "EMACS_CONFIG_STRICT=${STRICT}" \
      -v "$REPO_ROOT:/src:ro" \
      -v "${vol}:/elpa-cache" \
      "$img"; then
    RESULT[$v]=PASS
  else
    RESULT[$v]=FAIL
    fail=1
  fi
  echo
done

echo "############################################################"
echo "# Summary"
echo "############################################################"
for v in "${VERSIONS[@]}"; do
  printf "  Emacs %-6s %s\n" "$v" "${RESULT[$v]}"
done

exit "$fail"
