# Changelog

## 2026-03-26

- Fixed connect to saved networks with expired passwords (auto-retry with password prompt)
- Removed FIXME comments

## 2026-03-25

- Added extended status: IP, gateway, DNS, signal, frequency, link speed, traffic, VPN
- Added band (2.4G/5G) and rate columns to wifi network list
- Added DNS management (`e`): presets for Google, Cloudflare, Quad9, auto-revert
- Added router admin page shortcut (`r`): opens gateway in browser
- Fixed `let` vs `let*` scoping in gather-status
- Fixed empty connection string check in active-connection helper

## 2026-03-25

- Initial implementation
- Async nmcli-based wifi scanning and status monitoring
- Connect/disconnect/forget saved connections
- Toggle wifi on/off
- Auto-refresh timer while buffer is open
- Keybinding: `C-x y n` opens the network manager
