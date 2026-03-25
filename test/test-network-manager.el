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

(provide 'test-network-manager)

;;; test-network-manager.el ends here
