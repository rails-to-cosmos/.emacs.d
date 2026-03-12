#!/bin/bash
set -euo pipefail

# Install keyd
sudo pacman -S --needed keyd

# Create config for Framework Laptop 16 keyboard (32AC:0012)
# Uses overload + oneshot to work around matrix ghosting
# where CapsLock+S can't be held simultaneously.
#   Hold CapsLock + key  → Ctrl+key  (normal modifier)
#   Tap CapsLock, then key → Ctrl+key (oneshot, for ghosted combos like C-s)
sudo tee /etc/keyd/default.conf > /dev/null << 'EOF'
[ids]
32AC:0012

[main]
capslock = overload(control, oneshot(control))
EOF

# Enable and start the daemon
sudo systemctl enable --now keyd

echo "keyd is running. Press Caps Lock — it should act as Ctrl now."
echo "Verify with: sudo keyd -m"
