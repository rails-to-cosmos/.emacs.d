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
                # Small fix for package naming differences if needed
                # But generally relying on package manager to resolve or aliases
                subprocess.run(["sudo"] + cmd + packages, check=True)
                return

    def _is_wayland(self) -> bool:
        return os.environ.get("XDG_SESSION_TYPE", "").lower() == "wayland"

    def rofi(self) -> None:
        # Determine package and binary based on session type
        if self._is_wayland():
            print("Wayland detected. Using rofi-wayland.")
            pkg_name = "rofi-wayland"
            # On Wayland, sxhkd is generally not used (compositor handles binds),
            # so we skip sxhkd setup to avoid errors.
            if not shutil.which("rofi"):
                self._install([pkg_name])
            print("Note: On Wayland, configure keybindings in your compositor (Hyprland, Sway, etc.) config.")
            return

        # X11 Setup
        print("X11 detected (or non-Wayland). Using standard rofi + sxhkd.")
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
        options: list[str] = ["ctrl:nocaps", "grp:shifts_toggle"],
        delay: int = 170,
        rate: int = 60
    ) -> None:
        ly_str, op_str = ",".join(layouts), ",".join(options)

        # Cinnamon detection
        if "cinnamon" in os.environ.get("XDG_CURRENT_DESKTOP", "").lower() and shutil.which("gsettings"):
            src = f"[{', '.join(f"('xkb', '{l}')" for l in layouts)}]"
            opts = f"[{', '.join(f"'{o}'" for o in options)}]"
            try:
                subprocess.run(["gsettings", "set", "org.cinnamon.desktop.input-sources", "sources", src], check=True)
                subprocess.run(["gsettings", "set", "org.cinnamon.desktop.input-sources", "xkb-options", opts], check=True)
                print(f"Cinnamon config applied: {src}")
            except subprocess.CalledProcessError as e:
                print(f"Cinnamon error: {e}")

        # Check for xset and install if missing
        if not shutil.which("xset"):
            print("xset not found, installing...")
            # 'xorg-xset' is common on Arch/Fedora. Debian/Ubuntu might use 'x11-xserver-utils' or 'xset'
            # We try a generic approach or specific if on apt.
            if shutil.which("apt"):
                 self._install(["x11-xserver-utils"]) # Common provider on Debian-based
            else:
                 self._install(["xorg-xset"]) # Common on Arch/Fedora

        try:
            # Set Layout
            subprocess.run(["setxkbmap", "-layout", ly_str, "-option", op_str], check=True)
            # Set Typematic rate
            subprocess.run(["xset", "r", "rate", str(delay), str(rate)], check=True)
            print(f"X11 config applied: {ly_str} | {op_str} | Delay: {delay} Rate: {rate}")
        except subprocess.CalledProcessError as e:
            print(f"X11 error (setxkbmap/xset): {e}")

    def syncthing(self) -> None:
        if not shutil.which("syncthing"):
            self._install(["syncthing"])

        try:
            subprocess.run(["systemctl", "--user", "enable", "--now", "syncthing"], check=True)
            print("Syncthing enabled (user service). UI: http://127.0.0.1:8384")
        except subprocess.CalledProcessError as e:
            print(f"Systemd error: {e}")

    def all(self) -> None:
        self.rofi()
        self.keyboard()
        self.syncthing()

if __name__ == "__main__":
    if len(sys.argv) == 1:
        SystemSetup().all()
    else:
        fire.Fire(SystemSetup)
