#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "fire",
# ]
# ///

import os
import shutil
import subprocess
import fire
from pathlib import Path
import sys

class SystemSetup:
    def _install(self, packages: list[str]) -> None:
        managers = {
            "pacman": ["pacman", "-S", "--noconfirm"],
            "apt": ["apt-get", "install", "-y"],
            "dnf": ["dnf", "install", "-y"],
        }
        for mgr, cmd in managers.items():
            if shutil.which(mgr):
                # Sudo is usually required for package installation
                subprocess.run(["sudo"] + cmd + packages, check=True)
                return

    def _is_wayland(self) -> bool:
        return os.environ.get("XDG_SESSION_TYPE", "").lower() == "wayland"

    def rofi(self) -> None:
        """Sets up Rofi (or rofi-wayland) depending on the session type."""
        if self._is_wayland():
            print("Wayland detected. Using rofi-wayland.")
            # rofi-wayland is often named 'rofi-wayland' or just replaces 'rofi' depending on distro
            pkg_name = "rofi-wayland"

            if not shutil.which("rofi"):
                self._install([pkg_name])
            print("Note: On Wayland, keybindings should be configured in your compositor (Hyprland, Sway, etc.).")
            return

        # X11 Setup
        print("X11 detected. Using standard rofi + sxhkd.")
        pkg_list = ["rofi", "sxhkd"]

        rofi_conf = Path(os.path.expanduser("~/.config/rofi"))
        sxhkd_conf = Path(os.path.expanduser("~/.config/sxhkd/sxhkdrc"))
        bind_cmd = "ctrl + alt + space\n    rofi -show combi\n"

        if not (shutil.which("rofi") and shutil.which("sxhkd")):
            self._install(pkg_list)

        rofi_conf.mkdir(parents=True, exist_ok=True)
        sxhkd_conf.parent.mkdir(parents=True, exist_ok=True)

        if not sxhkd_conf.exists() or "rofi -show" not in sxhkd_conf.read_text():
            with open(sxhkd_conf, "a+") as f:
                f.write(f"\n# Rofi\n{bind_cmd}")

        subprocess.run(["pkill", "-USR1", "sxhkd"], check=False)
        subprocess.Popen(["sxhkd"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        print(f"Rofi (X11) setup complete. Bound to: {bind_cmd.strip()}")

    def keyboard(
        self,
        layouts: list[str] = ["us", "ru"],
