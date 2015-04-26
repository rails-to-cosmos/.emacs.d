(use-package csv-mode
  :ensure t)
(use-package csv-nav
  :ensure t)

(add-auto-mode 'csv-mode "\\.[Cc][Ss][Vv]\\'")

(setq csv-separators '("," ";" "|" " "))

(provide 'init-csv)
