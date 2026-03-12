#!/usr/bin/env bash
set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 <path-to-emacs-30.2.tar.gz>"
    exit 1
fi

TARBALL="$(realpath "$1")"

if [[ ! -f "$TARBALL" ]]; then
    echo "Error: file not found: $TARBALL"
    exit 1
fi

# Install build dependencies
echo "==> Installing build dependencies..."
sudo apt-get update
sudo apt-get install -y \
    build-essential \
    texinfo \
    libx11-dev \
    libxpm-dev \
    libjpeg-dev \
    libpng-dev \
    libgif-dev \
    libtiff-dev \
    libgtk-3-dev \
    libncurses-dev \
    libgnutls28-dev \
    libxml2-dev \
    libjansson-dev \
    libharfbuzz-dev \
    libsqlite3-dev \
    libtree-sitter-dev \
    "libgccjit-$(gcc -dumpversion | cut -d. -f1)-dev" \
    libacl1-dev \
    libotf-dev \
    libm17n-dev \
    libsystemd-dev \
    mailutils \
    autoconf

# Extract
BUILDDIR="$(mktemp -d)"
echo "==> Extracting to $BUILDDIR..."
tar xzf "$TARBALL" -C "$BUILDDIR"

cd "$BUILDDIR/emacs-30.2"

# Configure with native compilation and all features
echo "==> Configuring..."
./configure \
    --with-native-compilation=aot \
    --with-json \
    --with-tree-sitter \
    --with-mailutils \
    --with-x-toolkit=gtk3 \
    --with-gnutls \
    --with-jpeg \
    --with-png \
    --with-tiff \
    --with-gif \
    --with-xpm \
    --with-xml2 \
    --with-harfbuzz \
    --with-sqlite3

# Build
echo "==> Building (this will take a while)..."
make -j"$(nproc)"

# Install
echo "==> Installing..."
sudo make install

# Cleanup
echo "==> Cleaning up build directory..."
rm -rf "$BUILDDIR"

echo "==> Done! Emacs installed:"
emacs --version
