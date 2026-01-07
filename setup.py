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
                subprocess.run(["sudo"] + cmd + packages, check=True)
                return

    def rofi(self) -> None:
        rofi_conf = Path(os.path.expanduser("~/.config/rofi"))
        sxhkd_conf = Path(os.path.expanduser("~/.config/sxhkd/sxhkdrc"))
        bind_cmd = "ctrl + alt + space\n    rofi -show combi\n"

        if not (shutil.which("rofi") and shutil.which("sxhkd")):
            self._install(["rofi", "sxhkd"])

        rofi_conf.mkdir(parents=True, exist_ok=True)
        sxhkd_conf.parent.mkdir(parents=True, exist_ok=True)

        if not sxhkd_conf.exists() or "rofi -show" not in sxhkd_conf.read_text():
            with open(sxhkd_conf, "a+") as f:
                f.write(f"\n# Rofi\n{bind_cmd}")

        subprocess.run(["pkill", "-USR1", "sxhkd"], check=False)
        subprocess.Popen(["sxhkd"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        print(f"Rofi setup complete. Bound to: {bind_cmd.strip()}")

    def keyboard(
        self,
        ly_str: str = "us, ru",
        # Changed altwin:super_win -> altwin:alt_win
        op_str: str = "ctrl:nocaps, grp:shifts_toggle, altwin:alt_win",
        delay: int = 170,
        rate: int = 60
    ) -> None:
        layouts = [layout.strip() for layout in ly_str.split(",")]
        options = [op.strip() for op in op_str.split(",")]

        if "cinnamon" in os.environ.get("XDG_CURRENT_DESKTOP", "").lower() and shutil.which("gsettings"):
            # Note: The f-string nesting used here requires Python 3.12+
            src = f"[{', '.join(f"('xkb', '{l}')" for l in layouts)}]"
            opts = f"[{', '.join(f"'{o}'" for o in options)}]"
            try:
                subprocess.run(["gsettings", "set", "org.cinnamon.desktop.input-sources", "sources", src], check=True)
                subprocess.run(["gsettings", "set", "org.cinnamon.desktop.input-sources", "xkb-options", opts], check=True)
                print(f"Cinnamon config applied: {src}")
            except subprocess.CalledProcessError as e:
                print(f"Cinnamon error: {e}")

        try:
            subprocess.run(["setxkbmap", "-layout", ly_str, "-option", op_str], check=True)
            subprocess.run(["xset", "r", "rate", str(delay), str(rate)], check=True)
            print(f"X11 config applied: {ly_str} | {op_str}")
        except subprocess.CalledProcessError as e:
            print(f"X11 error: {e}")

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
