#!/usr/bin/env -S uv run
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "pathlib",
# ]
# ///

import os
import sys
import shutil
import subprocess
from pathlib import Path
from typing import Callable, Final, TypeAlias

Effect: TypeAlias = Callable[[], None]

XKB_CONFIG_PATH: Final[Path] = Path("/etc/X11/xorg.conf.d/00-keyboard.conf")
XKB_OPTION: Final[str] = "ctrl:nocaps"
DELAY: Final[int] = 170
RATE: Final[int] = 60


def get_distro_installer() -> Effect:
    """Detects package manager and returns an install effect."""
    managers = {
        "pacman": ["pacman", "-S", "--noconfirm", "xorg-xset", "xorg-setxkbmap"],
        "apt": ["apt-get", "install", "-y", "x11-xserver-utils"],
        "dnf": ["dnf", "install", "-y", "xset", "setxkbmap"],
    }
    for mgr, cmd in managers.items():
        if shutil.which(mgr):
            return lambda: subprocess.run(cmd, check=True, capture_output=True)
    return lambda: print("[WARN] No known package manager found. Skipping install.")


def apply_live_settings() -> None:
    """Applies settings to the current X session immediately."""
    try:
        # Apply Caps -> Ctrl
        subprocess.run(["setxkbmap", "-option", XKB_OPTION], check=True)
        # Apply Repeat Rate
        subprocess.run(["xset", "r", "rate", str(DELAY), str(RATE)], check=True)
        print(f"[LIVE] Settings applied: {XKB_OPTION}, rate {DELAY}/{RATE}")
    except Exception as e:
        print(f"[ERROR] Could not apply live settings: {e}")


def write_persistence() -> None:
    """Writes the Xorg configuration file for persistence."""
    content = f"""Section "InputClass"
        Identifier "system-keyboard"
        MatchIsKeyboard "on"
        Option "XkbOptions" "{XKB_OPTION}"
        Option "AutoRepeat" "{DELAY} {RATE}"
EndSection
"""
    XKB_CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
    XKB_CONFIG_PATH.write_text(content)
    print(f"[FILE] Persistent config written to {XKB_CONFIG_PATH}")


def main() -> None:
    if os.geteuid() != 0:
        sys.exit("[ERROR] This script must be run with sudo.")

    if not (shutil.which("xset") and shutil.which("setxkbmap")):
        print("[INFO] Tools missing. Attempting installation...")
        get_distro_installer()()

    print(f"\n--- Keyboard Optimization Plan ---")
    print(f"PERMANENT: Write to {XKB_CONFIG_PATH}")
    print(f"IMMEDIATE: setxkbmap {XKB_OPTION} && xset r rate {DELAY} {RATE}")

    if input("\nApply this plan? (y/n): ").lower() != 'y':
        sys.exit("Aborted.")

    write_persistence()
    apply_live_settings()

    print("\n[SUCCESS] Keyboard is now optimized and persistent.")

if __name__ == "__main__":
    main()
