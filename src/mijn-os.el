(use-package disk-usage
  :ensure t)

(defun reset-caps-lock ()
  "Reset caps lock by sending a Caps_Lock key event via xdotool."
  (interactive)
  (if (executable-find "xdotool")
      (progn
        (call-process "xdotool" nil nil nil "key" "Caps_Lock")
        (message "Caps Lock toggled"))
    (message "Error: xdotool not found. Install it to use this function.")))

(provide 'mijn-os)
