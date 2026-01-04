#!/usr/bin/env -S uv run --script
# /// script
# requires-python = ">=3.11"
# dependencies = [
#     "fire",
#     "pathlib",
# ]
# ///

import os
import shutil
import subprocess
import fire
from pathlib import Path

class SystemSetup:
    def all(self) -> None:
        self.rofi()
        self.keyboard()

    def rofi(self) -> None:
        rofi_conf = Path(os.path.expanduser("~/.config/rofi"))
        sxhkd_conf = Path(os.path.expanduser("~/.config/sxhkd/sxhkdrc"))
        bind_cmd = "ctrl + alt + space\n    rofi -show combi\n"

        if not (shutil.which("rofi") and shutil.which("sxhkd")):
            managers = {
                "pacman": ["pacman", "-S", "--noconfirm", "rofi", "sxhkd"],
                "apt": ["apt-get", "install", "-y", "rofi", "sxhkd"],
                "dnf": ["dnf", "install", "-y", "rofi", "sxhkd"],
            }
            for mgr, args in managers.items():
                if shutil.which(mgr):
                    subprocess.run(["sudo"] + args, check=True)
                    break

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
        layouts: list[str] = ["us", "ru"],
        options: list[str] = ["ctrl:nocaps", "grp:shifts_toggle"],
        delay: int = 170,
        rate: int = 60
    ) -> None:
        ly_str, op_str = ",".join(layouts), ",".join(options)

        if "cinnamon" in os.environ.get("XDG_CURRENT_DESKTOP", "").lower() and shutil.which("gsettings"):
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

if __name__ == "__main__":
    fire.Fire(SystemSetup)
