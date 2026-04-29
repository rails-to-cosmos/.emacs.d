# displays-mode

Buffer-based interactive editor for X display configuration.

Shells out to `xrandr` and `autorandr` — no native code required.

## Install

```elisp
(use-package displays-mode
  :load-path "~/.emacs.d/src/displays-mode"
  :commands (displays-show displays-watch-mode)
  :bind ("C-c d" . displays-show))
```

## Usage

```
M-x displays-show
```

Opens a `*Displays*` buffer showing each output:

```
Displays

▶ eDP-1   2160x1350 @ 60Hz  +0+0  ★ primary
  HDMI-1  (disconnected)
  DP-1    (disconnected)
```

### Keys

| Key       | Action |
|-----------|--------|
| `n` / `p` | Select next / previous display |
| `h j k l` | Move selected display left/down/up/right of an enabled neighbor |
| `R`       | Cycle resolution |
| `F`       | Cycle refresh rate (within current resolution) |
| `r`       | Cycle rotation: normal → left → inverted → right |
| `P`       | Mark display as primary |
| `d` / `e` | Disable / enable display |
| `g`       | Refresh from `xrandr --query` |
| `C-c C-c` | Apply current layout via `xrandr` |
| `C-c C-s` | Save current layout as autorandr profile |
| `C-c C-l` | Load autorandr profile (with completion) |
| `?`       | Help |

The buffer shows `[unapplied]` after any change until you `C-c C-c`.

## Hot-plug

```elisp
(displays-watch-mode 1)
```

Polls `xrandr` every 5 s and runs hooks on connect / disconnect:

```elisp
(add-hook 'displays-on-connect-hook
          (lambda (name)
            (message "Display %s connected" name)
            (call-process "autorandr" nil nil nil "--change")))
```

## Profile interop

Profiles are saved via `autorandr --save`, so the system-wide
`autorandr.service` keeps handling hotplug. `displays-mode` is the
*editor*; autorandr is the *daemon*.

## Roadmap

- ASCII grid view (boxes with relative positions)
- Transient menu (Magit-style) — `C-c d` opens a chord menu
- Rust backend via `x11rb` for direct EDID querying (when xrandr text
  parsing becomes too lossy)
