(defun my-kill-line-smartly ()
  (interactive)
  (delete-horizontal-space)
  (cond
    ((bolp) (save-excursion
              (cond
                (interactive-haskell-mode t)
                (t (call-interactively #'indent-for-tab-command)))))
    ((looking-at ")") t)
    (t (insert " "))))

(advice-add #'kill-line :after #'my-kill-line-smartly)
