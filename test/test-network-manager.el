;;; test-network-manager.el --- Tests for network-manager -*- lexical-binding: t; -*-

(require 'ert)
(require 'network-manager)

;; ---------------------------------------------------------------------------
;; Helper
;; ---------------------------------------------------------------------------

(defmacro with-network-buffer (&rest body)
  "Run BODY with a temporary *network* buffer and clean state."
  (declare (indent 0))
  `(let ((network-manager--status nil)
         (network-manager--wifi-networks nil)
         (network-manager--saved-connections nil)
         (network-manager--timer nil)
         (network-manager-buffer-name "*network-test*"))
     (unwind-protect
         (progn
           (get-buffer-create network-manager-buffer-name)
           ,@body)
       (when (get-buffer network-manager-buffer-name)
         (kill-buffer network-manager-buffer-name)))))

;; ---------------------------------------------------------------------------
;; network-manager-mode
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-mode-derives-from-special ()
  "network-manager-mode should derive from special-mode."
  (with-temp-buffer
    (network-manager-mode)
    (should (derived-mode-p 'special-mode))))

(ert-deftest test-nm-mode-is-read-only ()
  "network-manager-mode should set buffer-read-only."
  (with-temp-buffer
    (network-manager-mode)
    (should buffer-read-only)))

(ert-deftest test-nm-mode-truncates-lines ()
  "network-manager-mode should truncate lines."
  (with-temp-buffer
    (network-manager-mode)
    (should truncate-lines)))

;; ---------------------------------------------------------------------------
;; Keybindings
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-keybindings ()
  "Key bindings should be set in network-manager-mode-map."
  (should (eq (lookup-key network-manager-mode-map "g") #'network-manager-refresh))
  (should (eq (lookup-key network-manager-mode-map "s") #'network-manager-scan))
  (should (eq (lookup-key network-manager-mode-map (kbd "RET")) #'network-manager-connect))
  (should (eq (lookup-key network-manager-mode-map "d") #'network-manager-disconnect))
  (should (eq (lookup-key network-manager-mode-map "D") #'network-manager-delete-connection))
  (should (eq (lookup-key network-manager-mode-map "t") #'network-manager-toggle-wifi))
  (should (eq (lookup-key network-manager-mode-map "n") #'next-line))
  (should (eq (lookup-key network-manager-mode-map "p") #'previous-line))
  (should (eq (lookup-key network-manager-mode-map "q") #'quit-window)))

;; ---------------------------------------------------------------------------
;; network-manager--parse-lines
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-parse-lines-basic ()
  "Should split output into trimmed non-empty lines."
  (should (equal (network-manager--parse-lines "foo\nbar\nbaz\n")
                 '("foo" "bar" "baz"))))

(ert-deftest test-nm-parse-lines-empty ()
  "Should handle empty string."
  (should (null (network-manager--parse-lines ""))))

(ert-deftest test-nm-parse-lines-whitespace ()
  "Should filter blank lines and trim whitespace."
  (should (equal (network-manager--parse-lines "  foo  \n\n  bar  \n\n")
                 '("foo" "bar"))))

;; ---------------------------------------------------------------------------
;; network-manager--signal-indicator
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-signal-excellent ()
  "Signal >= 75 should show excellent."
  (should (string= "excellent" (substring-no-properties
                                (network-manager--signal-indicator 80)))))

(ert-deftest test-nm-signal-good ()
  "Signal 50-74 should show good."
  (should (string= "good" (substring-no-properties
                           (network-manager--signal-indicator 60)))))

(ert-deftest test-nm-signal-fair ()
  "Signal 25-49 should show fair."
  (should (string= "fair" (substring-no-properties
                           (network-manager--signal-indicator 30)))))

(ert-deftest test-nm-signal-weak ()
  "Signal < 25 should show weak."
  (should (string= "weak" (substring-no-properties
                           (network-manager--signal-indicator 10)))))

(ert-deftest test-nm-signal-boundaries ()
  "Test exact boundary values."
  (should (string= "excellent" (substring-no-properties
                                (network-manager--signal-indicator 75))))
  (should (string= "good" (substring-no-properties
                           (network-manager--signal-indicator 50))))
  (should (string= "fair" (substring-no-properties
                           (network-manager--signal-indicator 25))))
  (should (string= "weak" (substring-no-properties
                           (network-manager--signal-indicator 0)))))

;; ---------------------------------------------------------------------------
;; network-manager--render
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-render-no-status ()
  "Render without status should show gathering message."
  (with-network-buffer
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (should (string-match-p "Gathering status" (buffer-string))))))

(ert-deftest test-nm-render-with-status ()
  "Render with status should show connectivity and device info."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "Network Status" text))
        (should (string-match-p "full" text))
        (should (string-match-p "wlan0" text))
        (should (string-match-p "MyWifi" text))))))

(ert-deftest test-nm-render-no-wifi ()
  "Render without wifi networks should show scan hint."
  (with-network-buffer
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (should (string-match-p "Press 's' to scan" (buffer-string))))))

(ert-deftest test-nm-render-with-wifi ()
  "Render with wifi networks should show SSID list."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "HomeNet"))
    (setq network-manager--wifi-networks
          (list (list :ssid "HomeNet" :signal 80 :security "WPA2" :in-use t)
                (list :ssid "Neighbor" :signal 40 :security "WPA2" :in-use nil)
                (list :ssid "OpenWifi" :signal 30 :security "" :in-use nil)))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "HomeNet" text))
        (should (string-match-p "Neighbor" text))
        (should (string-match-p "OpenWifi" text))
        (should (string-match-p "open" text))
        (should (string-match-p "WPA2" text))))))

(ert-deftest test-nm-render-keybindings-shown ()
  "Render should display keybinding hints."
  (with-network-buffer
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "refresh" text))
        (should (string-match-p "scan" text))
        (should (string-match-p "connect" text))
        (should (string-match-p "quit" text))))))

(ert-deftest test-nm-render-idempotent ()
  "Multiple renders should not duplicate content."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "Test"))
    (network-manager--render)
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (= 1 (with-temp-buffer
                        (insert text)
                        (goto-char (point-min))
                        (let ((count 0))
                          (while (search-forward "Network Status" nil t)
                            (setq count (1+ count)))
                          count))))))))

;; ---------------------------------------------------------------------------
;; network-manager--ssid-at-point
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-ssid-at-point ()
  "Should extract SSID from a rendered wifi line."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "HomeNet"))
    (setq network-manager--wifi-networks
          (list (list :ssid "HomeNet" :signal 80 :security "WPA2" :in-use t)
                (list :ssid "OtherNet" :signal 50 :security "WPA2" :in-use nil)))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      ;; Go to the line with "OtherNet"
      (goto-char (point-min))
      (search-forward "OtherNet")
      (should (equal "OtherNet" (network-manager--ssid-at-point))))))

(ert-deftest test-nm-ssid-at-point-nil-on-header ()
  "Should return nil when not on a wifi network line."
  (with-network-buffer
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (goto-char (point-min))
      (should (null (network-manager--ssid-at-point))))))

;; ---------------------------------------------------------------------------
;; network-manager-disconnect (no connection)
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-disconnect-no-connection ()
  "Should error when there is no active connection."
  (let ((network-manager--status nil))
    (should-error (network-manager-disconnect) :type 'user-error)))

;; ---------------------------------------------------------------------------
;; Timer management
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-start-stop-timer ()
  "Starting and stopping timer should work without errors."
  (let ((network-manager--timer nil)
        (network-manager-refresh-interval 999))
    (network-manager--start-timer)
    (should network-manager--timer)
    (network-manager--stop-timer)
    (should-not network-manager--timer)))

(ert-deftest test-nm-start-timer-replaces-existing ()
  "Starting a new timer should cancel the old one."
  (let ((network-manager--timer nil)
        (network-manager-refresh-interval 999))
    (network-manager--start-timer)
    (let ((first network-manager--timer))
      (network-manager--start-timer)
      (should network-manager--timer)
      (should-not (eq first network-manager--timer))
      (network-manager--stop-timer))))

;; ---------------------------------------------------------------------------
;; network-manager--format-bytes
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-format-bytes-gb ()
  "Should format gigabytes."
  (should (string= "1.5 GB" (network-manager--format-bytes (* 1536 1024 1024)))))

(ert-deftest test-nm-format-bytes-mb ()
  "Should format megabytes."
  (should (string= "14.0 MB" (network-manager--format-bytes (* 14 1024 1024)))))

(ert-deftest test-nm-format-bytes-kb ()
  "Should format kilobytes."
  (should (string= "512.0 KB" (network-manager--format-bytes (* 512 1024)))))

(ert-deftest test-nm-format-bytes-b ()
  "Should format small byte counts."
  (should (string= "42 B" (network-manager--format-bytes 42))))

;; ---------------------------------------------------------------------------
;; Render: extended status fields
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-render-ip-gateway-dns ()
  "Render should show IP, gateway, and DNS when available."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"
                :ip "192.168.1.100/24" :gateway "192.168.1.1"
                :dns "8.8.8.8"))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "192.168.1.100/24" text))
        (should (string-match-p "192.168.1.1" text))
        (should (string-match-p "8.8.8.8" text))))))

(ert-deftest test-nm-render-wifi-link-details ()
  "Render should show signal, frequency, and link speed."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"
                :signal 85 :freq "5220 MHz" :rate "1170 Mbit/s"))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "excellent" text))
        (should (string-match-p "85%" text))
        (should (string-match-p "5220 MHz" text))
        (should (string-match-p "5 GHz" text))
        (should (string-match-p "1170 Mbit/s" text))))))

(ert-deftest test-nm-render-2ghz-band ()
  "Render should show 2.4 GHz for lower frequencies."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"
                :signal 60 :freq "2462 MHz" :rate "130 Mbit/s"))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (should (string-match-p "2.4 GHz" (buffer-string))))))

(ert-deftest test-nm-render-traffic ()
  "Render should show rx/tx traffic."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"
                :rx (* 14 1024 1024) :tx (* 2 1024 1024)))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "rx 14.0 MB" text))
        (should (string-match-p "tx 2.0 MB" text))))))

(ert-deftest test-nm-render-vpn ()
  "Render should show VPN connections."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"
                :vpn '("wg0")))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (should (string-match-p "wg0" (buffer-string))))))

(ert-deftest test-nm-render-no-vpn-when-absent ()
  "Render should not show VPN line when no VPN active."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "MyWifi"))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (should-not (string-match-p "VPN" (buffer-string))))))

;; ---------------------------------------------------------------------------
;; Render: wifi list with band and rate
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-render-wifi-band-info ()
  "Wifi list should show band and rate columns."
  (with-network-buffer
    (setq network-manager--status
          (list :connectivity "full" :device "wlan0" :type "wifi"
                :state "connected" :connection "Net5G"))
    (setq network-manager--wifi-networks
          (list (list :ssid "Net5G" :signal 90 :security "WPA2"
                      :freq "5220 MHz" :rate "1170 Mbit/s" :in-use t)
                (list :ssid "Net2G" :signal 60 :security "WPA2"
                      :freq "2462 MHz" :rate "130 Mbit/s" :in-use nil)))
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (let ((text (buffer-string)))
        (should (string-match-p "5G" text))
        (should (string-match-p "2\\.4G" text))
        (should (string-match-p "1170 Mbit/s" text))
        (should (string-match-p "130 Mbit/s" text))))))

;; ---------------------------------------------------------------------------
;; DNS commands
;; ---------------------------------------------------------------------------

(ert-deftest test-nm-dns-keybinding ()
  "Key e should be bound to network-manager-set-dns."
  (should (eq (lookup-key network-manager-mode-map "e") #'network-manager-set-dns)))

(ert-deftest test-nm-set-dns-no-connection ()
  "Should error when there is no active connection."
  (let ((network-manager--status nil))
    (should-error (network-manager-set-dns) :type 'user-error)))

(ert-deftest test-nm-dns-presets-has-google ()
  "DNS presets should include Google."
  (should (assoc "Google" network-manager-dns-presets)))

(ert-deftest test-nm-dns-presets-has-auto ()
  "DNS presets should include Auto with nil value."
  (let ((auto (assoc "Auto" network-manager-dns-presets)))
    (should auto)
    (should (null (cdr auto)))))

(ert-deftest test-nm-render-dns-hint ()
  "Render should show dns keybinding hint."
  (with-network-buffer
    (network-manager--render)
    (with-current-buffer network-manager-buffer-name
      (should (string-match-p "dns" (buffer-string))))))

(provide 'test-network-manager)

;;; test-network-manager.el ends here
