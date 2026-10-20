# Wrike token refresh for Emacs

This is an Emacs Lisp replacement for the interactive refresh flow in `@wdt/auth-token-refresh` 1.2.1. It requires Emacs 28.1 or newer and no Node.js package. The file lives in `src/` and `init.el` loads it automatically.

Run `M-x wrike-token-refresh`. The command shows where to issue each token, accepts token values through masked prompts, and lets you keep an existing value by pressing Return. It updates `~/.wrike.env` (or your chosen path) and the current Emacs process environment. The first run asks whether tokens are needed by GUI apps, terminal programs, or both. Use `C-u M-x wrike-token-refresh` to choose again.

Run `M-x wrike-token-refresh-status` to see tracked expiration times. The command reads the JWT expiration claim when present. For opaque tokens, it uses the eight-hour lifetime for the AI Proxy and Wrike tokens; GitLab expiry remains unknown.

On macOS, the GUI and both profiles also call `launchctl setenv` and offer to restart Claude Desktop if it is running. The terminal and both profiles offer to add a source line to `~/.zshrc`. Source the env file in already-open terminals after a refresh.

Preferences and expiry state use the same `~/.config/wrike-tokens/{config,state}.json` files as the published CLI. Env and JSON writes use private file permissions. Existing unrelated lines in the env file are preserved.

The published 1.2.1 executable does not contain the Claude Desktop config patching described in its README, so this implementation follows the executable's token refresh behavior.

Run the tests with:

```sh
emacs -Q --batch -L src -l src/wrike-token-refresh.el -l test/test-wrike-token-refresh.el -f ert-run-tests-batch-and-exit
```
