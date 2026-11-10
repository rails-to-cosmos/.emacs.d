;;; mijn-cisco-vpn.el --- Cisco Secure Client commands -*- lexical-binding: t; -*-

(require 'comint)
(require 'subr-x)

(defcustom mijn-cisco-vpn-program "/opt/cisco/secureclient/bin/vpn"
  "Path to the Cisco Secure Client VPN command."
  :type 'file
  :group 'external)

(defun mijn-cisco-vpn--hosts ()
  "Return the host names saved in Cisco Secure Client."
  (with-temp-buffer
    (when (zerop (call-process mijn-cisco-vpn-program nil t nil "hosts"))
      (goto-char (point-min))
      (let (hosts)
        (while (re-search-forward "^[[:space:]]*>[[:space:]]*\\(.+\\)$" nil t)
          (push (string-trim (match-string 1)) hosts))
        (nreverse hosts)))))

(defun mijn-cisco-vpn-connect (host)
  "Connect to Cisco VPN HOST, prompting for the password when requested.
Other Cisco prompts can be answered in the process buffer."
  (interactive
   (progn
     (unless (file-executable-p mijn-cisco-vpn-program)
       (user-error "Cisco Secure Client CLI not found: %s" mijn-cisco-vpn-program))
     (list (completing-read "Cisco VPN host: " (mijn-cisco-vpn--hosts) nil t))))
  (unless (file-executable-p mijn-cisco-vpn-program)
    (user-error "Cisco Secure Client CLI not found: %s" mijn-cisco-vpn-program))
  (when (string-empty-p host)
    (user-error "Cisco VPN host is required"))
  (let ((buffer (get-buffer-create "*Cisco VPN*")))
    (when (process-live-p (get-buffer-process buffer))
      (user-error "A Cisco VPN command is already running"))
    (let ((default-directory (expand-file-name "~/")))
      (make-comint-in-buffer "Cisco VPN" buffer mijn-cisco-vpn-program nil
                             "connect" host))
    (pop-to-buffer buffer)))

(provide 'mijn-cisco-vpn)
;;; mijn-cisco-vpn.el ends here
