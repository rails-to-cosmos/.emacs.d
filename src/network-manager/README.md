# network-manager.el

NetworkManager interface for Emacs. Async wifi scanning, connecting, DNS management, and status monitoring via `nmcli`.

## Usage

`C-x y n` opens the `*network*` buffer.

## Status Panel

Displays current connection details:
- Connectivity, device, connection name
- Signal strength, frequency (2.4/5 GHz), link speed
- IP address, gateway, DNS
- RX/TX traffic
- Active VPN connections (WireGuard, OpenVPN)

## Keybindings

| Key | Action |
|---|---|
| `g` | Refresh status and networks |
| `s` | Rescan wifi |
| `RET` | Connect to network at point |
| `d` | Disconnect current connection |
| `D` | Forget (delete) a saved connection |
| `e` | Set DNS (Google, Cloudflare, Quad9, Auto) |
| `r` | Open router admin page in browser |
| `t` | Toggle wifi on/off |
| `n/p` | Navigate lines |
| `q` | Quit |

## Features

- Async wifi scanning with deduplication (keeps strongest signal per SSID)
- Band (2.4G/5G) and rate columns in wifi list
- Smart connection handling:
  - Saved networks: tries activation, falls back to password prompt on secrets error
  - Saved networks with expired passwords: updates stored PSK then activates
  - New secured networks: prompts for password
  - Open networks: connects and disables autoconnect to prevent auto-joining
- DNS presets configurable via `network-manager-dns-presets`
- Auto-refresh while buffer is open (configurable interval)

## Configuration

```elisp
;; Refresh interval (default 10 seconds)
(setq network-manager-refresh-interval 10)

;; Custom DNS presets
(setq network-manager-dns-presets
      '(("Google"     . "8.8.8.8 8.8.4.4")
        ("Cloudflare" . "1.1.1.1 1.0.0.1")
        ("Quad9"      . "9.9.9.9 149.112.112.112")
        ("Auto"       . nil)))
```
