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

    def _is_wayland(self) -> bool:
        return os.environ.get("XDG_SESSION_TYPE", "").lower() == "wayland"

    def rofi(self) -> None:
        """Sets up Rofi (or rofi-wayland)."""
        if self._is_wayland():
            print("Wayland detected. Using rofi-wayland.")
            pkg_name = "rofi-wayland"
            # On some distros 'rofi' package supports both, but often split.
            # We assume 'rofi' command will exist after install.
            if not shutil.which("rofi"):
                self._install([pkg_name])
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
        options: list[str] = ["ctrl:nocaps", "grp:shifts_toggle"],
        delay: int = 170,
        rate: int = 60
    ) -> None:
        """Sets X11 keyboard settings. (Compositor settings handled in 'compositor' method)."""
        if self._is_wayland():
            print("Wayland detected. Keyboard settings must be applied via Compositor config (see 'compositor' command).")
            return

        ly_str, op_str = ",".join(layouts), ",".join(options)

        # Check for xset and install if missing
        if not shutil.which("xset"):
            print("xset not found. Installing...")
            if shutil.which("apt"):
                 self._install(["x11-xserver-utils"])
            else:
                 self._install(["xorg-xset"])

        try:
            subprocess.run(["setxkbmap", "-layout", ly_str, "-option", op_str], check=True)
            subprocess.run(["xset", "r", "rate", str(delay), str(rate)], check=True)
            print(f"X11 config applied: {ly_str} | {op_str} | Rate: {rate} Delay: {delay}")
        except subprocess.CalledProcessError as e:
            print(f"X11 error: {e}")

    def compositor(self) -> None:
        """Detects Wayland Compositor (Hyprland/Sway) and injects keyboard & rofi bindings."""
        if not self._is_wayland():
            print("Not running Wayland. Skipping compositor setup.")
            return

        # Configuration values to inject
        rofi_cmd = "rofi -show combi"
        kb_layouts = "us,ru"
        kb_options = "ctrl:nocaps,grp:shifts_toggle"
        kb_rate = "60"
        kb_delay = "170"

        # --- HYPRLAND SETUP ---
        if os.environ.get("HYPRLAND_INSTANCE_SIGNATURE"):
            print("Hyprland detected.")
            conf_path = Path(os.path.expanduser("~/.config/hypr/hyprland.conf"))

            if not conf_path.exists():
                print(f"Error: {conf_path} not found.")
                return

            config_content = conf_path.read_text()

            # 1. Inject Input Config (Keyboard)
            input_block = f"""
# --- Injected by setup.py ---
input {{
    kb_layout = {kb_layouts}
    kb_options = {kb_options}
    repeat_rate = {kb_rate}
    repeat_delay = {kb_delay}
}}
# ----------------------------
"""
            if "kb_layout" not in config_content:
                print("Injecting input/keyboard config into Hyprland...")
                with open(conf_path, "a") as f:
                    f.write(input_block)
            else:
                print("Keyboard config already present in Hyprland (skipping override).")

            # 2. Inject Keybinding
            # Hyprland syntax: bind = MODS, key, dispatcher, params
            bind_line = f"bind = CTRL ALT, space, exec, {rofi_cmd}"

            if "rofi -show" not in config_content:
                print(f"Injecting Rofi binding: {bind_line}")
                with open(conf_path, "a") as f:
                    f.write(f"\n{bind_line}\n")
            else:
                print("Rofi binding already present.")

            # Reload Hyprland
            subprocess.run(["hyprctl", "reload"], check=False)

        # --- SWAY SETUP ---
        elif os.environ.get("SWAYSOCK"):
            print("Sway detected.")
            conf_path = Path(os.path.expanduser("~/.config/sway/config"))

            if not conf_path.exists():
                # Fallback to creating local config from /etc/sway/config if needed
                conf_path.parent.mkdir(parents=True, exist_ok=True)
                if Path("/etc/sway/config").exists() and not conf_path.exists():
                     shutil.copy("/etc/sway/config", conf_path)

            config_content = conf_path.read_text()

            # 1. Inject Input Config
            # Sway syntax: input <identifier> { ... }
            input_cmd = f"""
# --- Injected by setup.py ---
input type:keyboard {{
    xkb_layout {kb_layouts}
    xkb_options {kb_options}
    repeat_delay {kb_delay}
    repeat_rate {kb_rate}
}}
# ----------------------------
"""
            if "xkb_layout" not in config_content:
                print("Injecting input/keyboard config into Sway...")
                with open(conf_path, "a") as f:
                    f.write(input_cmd)

            # 2. Inject Keybinding
            # Sway syntax: bindsym <modifiers>+<key> <command>
            # Mod1 is Alt. Control is Control.
            bind_line = f"bindsym Control+Mod1+space exec {rofi_cmd}"

            if "rofi -show" not in config_content:
                print(f"Injecting Rofi binding: {bind_line}")
                with open(conf_path, "a") as f:
                    f.write(f"\n{bind_line}\n")

            subprocess.run(["swaymsg", "reload"], check=False)

        else:
            print("Unknown Wayland Compositor. Please configure bindings manually.")

    def syncthing(self) -> None:
        """Installs and enables Syncthing user service."""
        if not shutil.which("syncthing"):
            self._install(["syncthing"])

        try:
            subprocess.run(["systemctl", "--user", "enable", "--now", "syncthing"], check=True)
            print("Syncthing enabled. UI: http://127.0.0.1:8384")
        except subprocess.CalledProcessError as e:
            print(f"Systemd error: {e}")

    def all(self) -> None:
        """Runs all setup steps."""
        self.rofi()
        self.keyboard()
        self.compositor()
        self.syncthing()

if __name__ == "__main__":
    fire.Fire(SystemSetup)
