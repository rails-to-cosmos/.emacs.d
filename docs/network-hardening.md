# WiFi Network Hardening

Audit date: 2026-03-26
Connection: Ziggo2613784_EXT (Ziggo router, gateway 192.168.178.1)

## Router-side (admin panel at 192.168.178.1)

- [ ] Rename SSID to something non-identifying (remove default Ziggo prefix)
- [ ] Enable WPA3-SAE if supported, or WPA2/WPA3 mixed mode
- [ ] Disable WPS (WiFi Protected Setup) — trivially brute-forceable
- [ ] Disable UPnP unless specifically needed
- [ ] Update router firmware
- [ ] Change default admin password

## Client-side (nmcli)

### Enable Protected Management Frames (PMF)

Prevents deauthentication attacks (e.g. WiFi Deauther, mdk3).

```bash
nmcli connection modify "Ziggo2613784_EXT" 802-11-wireless-security.pmf 2
```

### Enable MAC address randomization

Prevents tracking across networks by APs and passive observers.

```bash
nmcli connection modify "Ziggo2613784_EXT" 802-11-wireless.mac-address-randomization always
```

### Enable DNS-over-TLS

Encrypts DNS queries so ISP/network observers can't see lookups.

```bash
nmcli connection modify "Ziggo2613784_EXT" \
  connection.dns-over-tls 2 \
  ipv4.dns "1.1.1.1 1.0.0.1" \
  ipv4.ignore-auto-dns yes
```

To revert to ISP DNS:

```bash
nmcli connection modify "Ziggo2613784_EXT" \
  connection.dns-over-tls -1 \
  ipv4.dns "" \
  ipv4.ignore-auto-dns no
```

### Clean up stale saved connections

Old saved connections leak SSIDs in probe requests.

```bash
nmcli connection delete \
  "Auto CloudNet1813" \
  "Auto CYTA_4fJp_2.4G" \
  "Auto CYTA_4fJp_5G" \
  "Auto CYTA_xrTf_2.4G" \
  "Auto Dmitry's iPhone" \
  "Auto phi_44869284C465" \
  "Auto WiFimodem-e207" \
  "Auto Ziggo2613784" \
  "SmartLife-2A5B" \
  "Ziggo1877696"
```

### Apply changes

After modifying, reconnect:

```bash
nmcli connection up "Ziggo2613784_EXT"
```

## Summary

| Issue | Severity | Fix |
|---|---|---|
| WPA2-PSK (no WPA3) | Medium | Router: enable WPA3-SAE |
| PMF disabled | Medium | `nmcli modify ... pmf 2` |
| MAC randomization off | Low | `nmcli modify ... mac-address-randomization always` |
| DNS-over-TLS off | Low | `nmcli modify ... dns-over-tls 2` |
| ISP DNS (unencrypted) | Low | Switch to Cloudflare/Google |
| 10 stale saved WiFi networks | Low | `nmcli connection delete ...` |
