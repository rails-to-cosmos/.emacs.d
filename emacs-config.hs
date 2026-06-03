#!/usr/bin/env runghc
-- emacs-config.hs — a typed Haskell DSL that emits Emacs Lisp.
--
-- Run:  runghc emacs-config.hs > generated.el
-- Or:   make emacs-config  (from ~/.config/xmonad)
--
-- init.el loads the result via:  (load (expand-file-name "generated.el" user-emacs-directory) t)
--
-- This gives you Haskell's type checking and abstraction for the parts of your
-- Emacs config that benefit from it, while Emacs still runs plain Elisp.

module Main where

import Data.List (intercalate)

-- ─────────────────────────────────────────────────────────────────────────
-- DSL types
-- ─────────────────────────────────────────────────────────────────────────

-- An Elisp value we know how to render.
data Val
  = VInt    Int
  | VFloat  Double
  | VStr    String
  | VBool   Bool          -- t / nil
  | VSym    String        -- a bare symbol, e.g. 'box  (we add the quote)
  | VRaw    String        -- emitted verbatim (escape hatch)

-- A top-level configuration form.
data Form
  = Setq   String Val             -- (setq NAME VAL)
  | SetqDefault String Val        -- (setq-default NAME VAL)
  | Require String                -- (require 'MODULE)
  | GlobalKey String String       -- (global-set-key (kbd "KEY") #'CMD)
  | AddHook String String         -- (add-hook 'HOOK #'FN)
  | Load String                   -- (load "FILE" t)
  | Comment String                -- ;; comment
  | Raw String                    -- arbitrary elisp line(s)
  | Use UsePackage                -- (use-package ...)

-- A use-package declaration. Build with `use "name"` then set fields, or
-- with record syntax. Empty fields are omitted from output.
data UsePackage = UsePackage
  { upName    :: String              -- package name
  , upEnsure  :: Bool                -- :ensure t  (install if missing)
  , upDefer   :: Bool                -- :defer t   (lazy load)
  , upBind    :: [(String, String)]  -- :bind  (("KEY" . cmd) ...)
  , upHook    :: [String]            -- :hook  (mode ...)  — hooks without the -hook suffix
  , upMode    :: [(String, String)]  -- :mode  (("\\.ext\\'" . mode) ...)
  , upCommands:: [String]            -- :commands (cmd ...)
  , upCustom  :: [(String, Val)]     -- :custom  ((var val) ...)
  , upInit    :: [String]            -- :init  — raw elisp lines, run before load
  , upConfig  :: [String]            -- :config — raw elisp lines, run after load
  }

-- A blank use-package with sensible defaults (:ensure t).
use :: String -> UsePackage
use name = UsePackage
  { upName = name, upEnsure = True, upDefer = False
  , upBind = [], upHook = [], upMode = [], upCommands = []
  , upCustom = [], upInit = [], upConfig = []
  }

-- ─────────────────────────────────────────────────────────────────────────
-- Rendering
-- ─────────────────────────────────────────────────────────────────────────

renderVal :: Val -> String
renderVal v = case v of
  VInt n    -> show n
  VFloat f  -> show f
  VStr s    -> "\"" ++ concatMap esc s ++ "\""
  VBool b   -> if b then "t" else "nil"
  VSym s    -> "'" ++ s
  VRaw s    -> s
  where
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc c    = [c]

renderForm :: Form -> String
renderForm f = case f of
  Setq n v        -> "(setq " ++ n ++ " " ++ renderVal v ++ ")"
  SetqDefault n v -> "(setq-default " ++ n ++ " " ++ renderVal v ++ ")"
  Require m       -> "(require '" ++ m ++ ")"
  GlobalKey k c   -> "(global-set-key (kbd \"" ++ k ++ "\") #'" ++ c ++ ")"
  AddHook h fn    -> "(add-hook '" ++ h ++ " #'" ++ fn ++ ")"
  Load file       -> "(load \"" ++ file ++ "\" t)"
  Comment s       -> ";; " ++ s
  Raw s           -> s
  Use up          -> renderUse up

-- Render a use-package block. Each set field becomes a keyword section;
-- empty fields are omitted entirely.
renderUse :: UsePackage -> String
renderUse up = intercalate "\n" $
     ["(use-package " ++ upName up]
  ++ kw upEnsure   (const ["  :ensure t"])
  ++ kw upDefer    (const ["  :defer t"])
  ++ list upCommands (\cs -> ["  :commands (" ++ unwords cs ++ ")"])
  ++ list upBind   (\bs -> ["  :bind (" ++ intercalate "\n         " (map pair bs) ++ ")"])
  ++ list upHook   (\hs -> ["  :hook (" ++ unwords hs ++ ")"])
  ++ list upMode   (\ms -> ["  :mode (" ++ intercalate "\n         " (map modePair ms) ++ ")"])
  ++ list upCustom (\cs -> ["  :custom" ] ++ map (("  " ++) . customPair) cs)
  ++ block upInit   ":init"
  ++ block upConfig ":config"
  ++ ["  )"]
  where
    -- keyword that depends on a Bool field
    kw sel render = if sel up then render up else []
    -- keyword that depends on a list field (omitted when empty)
    list sel render = if null (sel up) then [] else render (sel up)
    -- :init / :config raw elisp lines
    block sel keyword =
      let ls = sel up
      in if null ls then [] else ("  " ++ keyword) : map ("  " ++) ls
    pair (k, c)    = "(\"" ++ k ++ "\" . " ++ c ++ ")"
    modePair (p,m) = "(\"" ++ p ++ "\" . " ++ m ++ ")"
    customPair (n,v) = "(" ++ n ++ " " ++ renderVal v ++ ")"

header :: [String]
header =
  [ ";;; generated.el --- GENERATED FROM emacs-config.hs — DO NOT EDIT BY HAND"
  , ";;; Commentary:"
  , ";;; Edit ~/.emacs.d/emacs-config.hs and run `make emacs-config`."
  , ";;; Code:"
  , ""
  ]

footer :: [String]
footer =
  [ ""
  , "(provide 'generated)"
  , ";;; generated.el ends here"
  ]

render :: [Form] -> String
render forms = unlines (header ++ map renderForm forms ++ footer)

-- ─────────────────────────────────────────────────────────────────────────
-- Your configuration (edit this part)
-- ─────────────────────────────────────────────────────────────────────────

config :: [Form]
config =
  [ Comment "── Editing defaults ──"
  , SetqDefault "indent-tabs-mode" (VBool False)
  , SetqDefault "tab-width" (VInt 4)
  , Setq "require-final-newline" (VBool True)
  , Setq "sentence-end-double-space" (VBool False)

  , Raw ""
  , Comment "── UI ──"
  , Setq "inhibit-startup-screen" (VBool True)
  , Setq "ring-bell-function" (VSym "ignore")
  , Setq "cursor-type" (VSym "box")

  , Raw ""
  , Comment "── Backups / autosave ──"
  , Setq "make-backup-files" (VBool False)
  , Setq "create-lockfiles" (VBool False)

  , Raw ""
  , Comment "── Keybindings ──"
  , GlobalKey "C-x C-b" "ibuffer"
  , GlobalKey "M-/"     "hippie-expand"

  , Raw ""
  , Comment "── Hooks ──"
  , AddHook "prog-mode-hook" "display-line-numbers-mode"
  , AddHook "before-save-hook" "delete-trailing-whitespace"

  , Raw ""
  , Comment "── Packages (use-package) ──"

  -- Minimal: just install
  , Use (use "diminish")

  -- With keybindings + lazy load
  , Use (use "magit")
      { upBind = [("C-x g", "magit-status")] }

  -- Full example: bind, hook, custom, config
  , Use (use "rainbow-mode")
      { upHook   = ["prog-mode"]
      , upCommands = ["rainbow-mode"]
      }

  , Use (use "which-key")
      { upConfig = ["(which-key-mode 1)"]
      , upCustom = [("which-key-idle-delay", VFloat 0.3)]
      }

  -- Mode association + defer
  , Use (use "yaml-mode")
      { upDefer = True
      , upMode  = [("\\\\.ya?ml\\\\'", "yaml-mode")]
      }
  ]

main :: IO ()
main = putStr (render config)
