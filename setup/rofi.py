#!/usr/bin/env -S uv run --script
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
from typing import Callable, Final, TypeAlias, Mapping

Effect: TypeAlias = Callable[[], None]
PackageMap: TypeAlias = Mapping[str, list[str]]

ROFI_CONFIG_DIR: Final[Path] = Path(os.path.expanduser("~/.config/rofi"))
SXHKD_CONFIG_PATH: Final[Path] = Path(os.path.expanduser("~/.config/sxhkd/sxhkdrc"))
ROFI_BINDING: Final[str] = "ctrl + alt + space"
ROFI_COMMAND: Final[str] = "rofi -show combi"

PM_STRATEGIES: Final[PackageMap] = {
    "pacman": ["pacman", "-S", "--noconfirm", "rofi", "sxhkd"],
    "apt": ["apt-get", "install", "-y", "rofi", "sxhkd"],
    "dnf": ["dnf", "install", "-y", "rofi", "sxhkd"],
}

def get_install_effect() -> Effect:
    for manager, args in PM_STRATEGIES.items():
        if shutil.which(manager):
            return lambda: subprocess.run(["sudo"] + args, check=True)
    return lambda: sys.exit("[ERROR] No supported package manager found.")

def is_installed(binary: str) -> bool:
    return shutil.which(binary) is not None

def setup_sxhkd_binding() -> None:
    binding_config = f"\n# Rofi Launcher\n{ROFI_BINDING}\n    {ROFI_COMMAND}\n"
    SXHKD_CONFIG_PATH.parent.mkdir(parents=True, exist_ok=True)
    with open(SXHKD_CONFIG_PATH, "a+") as f:
        f.seek(0)
        if ROFI_COMMAND not in f.read():
            f.write(binding_config)

def main() -> None:
    if not (is_installed("rofi") and is_installed("sxhkd")):
        get_install_effect()()

    print(f"[PLAN] Install Rofi/sxhkd and bind {ROFI_BINDING} -> {ROFI_COMMAND}")
    if input("Proceed? (y/n): ").lower() != 'y':
        sys.exit("Aborted.")

    ROFI_CONFIG_DIR.mkdir(parents=True, exist_ok=True)
    setup_sxhkd_binding()

    try:
        subprocess.run(["pkill", "-USR1", "sxhkd"], check=False)
        subprocess.Popen(["sxhkd"], stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
    except Exception:
        pass

    print("[SUCCESS] Rofi configured. Ensure 'sxhkd &' is in your startup script.")

if __name__ == "__main__":
    main()
