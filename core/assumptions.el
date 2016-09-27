(dolist (key '("\C-l" "\C-t" "\C-xi" "\C-cC-b"))
  (global-unset-key key))

(setq mac-option-key-is-meta nil
      mac-command-key-is-meta t
      mac-command-modifier 'meta
      mac-option-modifier nil)

(provide 'assumptions)
