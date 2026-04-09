;;; network-manager.el --- NetworkManager interface for Emacs -*- lexical-binding: t; -*-

(require 'seq)

;; --- Customizable faces & variables ---

(defvar network-manager-refresh-interval 10
  "Seconds between automatic status refreshes.")

(defvar network-manager--timer nil
  "Timer for periodic status refresh.")

(defvar network-manager--status nil
  "Current network status plist.
Keys: :connectivity :device :type :state :connection
      :ip :gateway :dns :signal :freq :rate :rx :tx")

(defvar network-manager--wifi-networks nil
  "List of available wifi networks, each a plist.
Keys: :ssid :signal :security :in-use")

(defvar network-manager--saved-connections nil
  "List of saved connection plists.
Keys: :name :type :autoconnect")

(defvar network-manager-buffer-name "*network*"
  "Name of the network manager buffer.")

(defvar network-manager-dns-presets
  '(("Google"     . "8.8.8.8 8.8.4.4")
    ("Cloudflare" . "1.1.1.1 1.0.0.1")
    ("Quad9"      . "9.9.9.9 149.112.112.112")
    ("Auto"       . nil))
  "Alist of DNS preset names to DNS server strings.
nil means revert to automatic (DHCP-provided) DNS.")

;; --- Keymap ---

(defvar network-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g"       #'network-manager-refresh)
    (define-key map "s"       #'network-manager-scan)
    (define-key map (kbd "RET") #'network-manager-connect)
    (define-key map "d"       #'network-manager-disconnect)
    (define-key map "D"       #'network-manager-delete-connection)
    (define-key map "t"       #'network-manager-toggle-wifi)
    (define-key map "e"       #'network-manager-set-dns)
    (define-key map "r"       #'network-manager-open-router)
    (define-key map "n"       #'next-line)
    (define-key map "p"       #'previous-line)
    (define-key map "q"       #'quit-window)
    map)
  "Keymap for `network-manager-mode'.")

;; --- Major mode ---

(define-derived-mode network-manager-mode special-mode "NetManager"
  "Major mode for managing network connections via NetworkManager."
  (setq-local revert-buffer-function (lambda (_ignore-auto _noconfirm)
                                       (network-manager-refresh)))
  (setq truncate-lines t))

;; --- Parsing nmcli output ---

(defun network-manager--parse-lines (output)
  "Split OUTPUT into trimmed non-empty lines."
  (seq-remove #'string-empty-p
              (mapcar #'string-trim (split-string output "\n"))))

(defun network-manager--format-bytes (bytes)
  "Format BYTES as a human-readable string."
  (cond
   ((>= bytes (* 1024 1024 1024)) (format "%.1f GB" (/ bytes (* 1024.0 1024 1024))))
   ((>= bytes (* 1024 1024))      (format "%.1f MB" (/ bytes (* 1024.0 1024))))
   ((>= bytes 1024)               (format "%.1f KB" (/ bytes 1024.0)))
   (t                              (format "%d B" bytes))))

(defun network-manager--gather-status (callback)
  "Asynchronously gather network status, then call CALLBACK."
  (let* ((result (list :connectivity nil :device nil :type nil
                       :state nil :connection nil
                       :ip nil :gateway nil :dns nil
                       :signal nil :freq nil :rate nil
                       :rx nil :tx nil :vpn nil))
         (pending 3)
         (done-fn
          (lambda ()
            (setq pending (1- pending))
            (when (= pending 0)
              (when-let ((dev (plist-get result :device)))
                (with-temp-buffer
                  (insert-file-contents "/proc/net/dev")
                  (when (re-search-forward
                         (format "^\\s-*%s:\\s-*\\([0-9]+\\).*" (regexp-quote dev))
                         nil t)
                    (let* ((fields (split-string (match-string 0) "\\s-+" t))
                           (rx (string-to-number (nth 1 fields)))
                           (tx (string-to-number (nth 9 fields))))
                      (plist-put result :rx rx)
                      (plist-put result :tx tx)))))
              (setq network-manager--status result)
              (funcall callback)))))
    ;; 1. Connectivity
    (let ((buf (generate-new-buffer " *nm-connectivity*")))
      (set-process-sentinel
       (start-process "nm-connectivity" buf "nmcli" "networking" "connectivity")
       (lambda (proc _event)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (plist-put result :connectivity (string-trim (buffer-string)))))
         (kill-buffer (process-buffer proc))
         (funcall done-fn))))
    ;; 2. Device details (IP, gateway, DNS, connection name)
    (let ((buf (generate-new-buffer " *nm-device*")))
      (set-process-sentinel
       (start-process "nm-device" buf "nmcli" "-t" "device" "show")
       (lambda (proc _event)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (let ((lines (network-manager--parse-lines (buffer-string)))
                   (cur-dev nil)
                   (cur-type nil)
                   (found nil))
               (dolist (line lines)
                 (cond
                  ((string-match "^GENERAL\\.DEVICE:\\(.*\\)" line)
                   (setq cur-dev (match-string 1 line)))
                  ((string-match "^GENERAL\\.TYPE:\\(.*\\)" line)
                   (setq cur-type (match-string 1 line)))
                  ((and (not found)
                        (string-match "^GENERAL\\.STATE:100" line)
                        (not (equal cur-type "loopback")))
                   (setq found t)
                   (plist-put result :device cur-dev)
                   (plist-put result :type cur-type))
                  ((and found (string-match "^GENERAL\\.CONNECTION:\\(.*\\)" line))
                   (plist-put result :connection (match-string 1 line)))
                  ((and found (string-match "^IP4\\.ADDRESS\\[1\\]:\\(.*\\)" line))
                   (plist-put result :ip (match-string 1 line)))
                  ((and found (string-match "^IP4\\.GATEWAY:\\(.*\\)" line))
                   (plist-put result :gateway (match-string 1 line)))
                  ((and found (string-match "^IP4\\.DNS\\[1\\]:\\(.*\\)" line))
                   (plist-put result :dns (match-string 1 line))))))))
         (kill-buffer (process-buffer proc))
         (funcall done-fn))))
    ;; 3. Active connections (VPN) + wifi link details (signal, freq, rate)
    (let ((buf (generate-new-buffer " *nm-active*")))
      (set-process-sentinel
       (start-process "nm-active" buf
                      "nmcli" "-t" "-f" "NAME,TYPE,DEVICE" "connection" "show" "--active")
       (lambda (proc _event)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (let ((vpns nil))
               (dolist (line (network-manager--parse-lines (buffer-string)))
                 (let ((fields (split-string line ":")))
                   (when (and (>= (length fields) 2)
                              (member (nth 1 fields) '("wireguard" "vpn")))
                     (push (nth 0 fields) vpns))))
               (when vpns
                 (plist-put result :vpn (nreverse vpns))))))
         (kill-buffer (process-buffer proc))
         (let ((buf2 (generate-new-buffer " *nm-wifi-link*")))
           (set-process-sentinel
            (start-process "nm-wifi-link" buf2
                           "nmcli" "-t" "-f" "SSID,SIGNAL,FREQ,RATE,IN-USE"
                           "device" "wifi" "list")
            (lambda (proc2 _event2)
              (when (eq (process-exit-status proc2) 0)
                (with-current-buffer (process-buffer proc2)
                  (dolist (line (network-manager--parse-lines (buffer-string)))
                    (when (string-suffix-p ":*" line)
                      (let ((fields (split-string line ":")))
                        (plist-put result :signal
                                   (string-to-number (nth 1 fields)))
                        (plist-put result :freq (nth 2 fields))
                        (plist-put result :rate (nth 3 fields)))))))
              (kill-buffer (process-buffer proc2))
              (funcall done-fn)))))))))

(defun network-manager--scan-wifi (callback)
  "Asynchronously scan for wifi networks, then call CALLBACK with results."
  (let ((buf (generate-new-buffer " *nm-wifi*")))
    (set-process-sentinel
     (start-process "nm-wifi-scan" buf
                    "nmcli" "-t" "-f" "SSID,SIGNAL,SECURITY,FREQ,RATE,IN-USE"
                    "device" "wifi" "list" "--rescan" "yes")
     (lambda (proc _event)
       (let (networks)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (dolist (line (network-manager--parse-lines (buffer-string)))
               (let ((fields (split-string line ":")))
                 (when (and (>= (length fields) 5)
                            (not (string-empty-p (nth 0 fields))))
                   (let* ((ssid (nth 0 fields))
                          (signal (string-to-number (nth 1 fields)))
                          (security (nth 2 fields))
                          (freq (nth 3 fields))
                          (rate (nth 4 fields))
                          (in-use (string= "*" (nth 5 fields)))
                          (existing (seq-find
                                     (lambda (n) (equal (plist-get n :ssid) ssid))
                                     networks)))
                     (if existing
                         (when (> signal (plist-get existing :signal))
                           (plist-put existing :signal signal)
                           (plist-put existing :freq freq)
                           (plist-put existing :rate rate)
                           (plist-put existing :in-use
                                      (or in-use (plist-get existing :in-use))))
                       (push (list :ssid ssid :signal signal
                                   :security security :freq freq
                                   :rate rate :in-use in-use)
                             networks))))))))
         (kill-buffer (process-buffer proc))
         (setq networks (sort networks
                              (lambda (a b)
                                (> (plist-get a :signal)
                                   (plist-get b :signal)))))
         (setq network-manager--wifi-networks networks)
         (funcall callback networks))))))

(defun network-manager--get-saved-connections (callback)
  "Asynchronously get saved connections, then call CALLBACK."
  (let ((buf (generate-new-buffer " *nm-saved*")))
    (set-process-sentinel
     (start-process "nm-saved" buf
                    "nmcli" "-t" "-f" "NAME,TYPE,AUTOCONNECT" "connection" "show")
     (lambda (proc _event)
       (let (conns)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (dolist (line (network-manager--parse-lines (buffer-string)))
               (let ((fields (split-string line ":")))
                 (when (>= (length fields) 3)
                   (push (list :name (nth 0 fields)
                               :type (nth 1 fields)
                               :autoconnect (string= "yes" (nth 2 fields)))
                         conns))))))
         (kill-buffer (process-buffer proc))
         (setq network-manager--saved-connections (nreverse conns))
         (funcall callback conns))))))

;; --- Rendering ---

(defun network-manager--signal-indicator (signal)
  "Return a visual indicator for SIGNAL strength (0-100)."
  (cond
   ((>= signal 75) (propertize "excellent" 'face '(:foreground "#9ece6a")))
   ((>= signal 50) (propertize "good"      'face '(:foreground "#e0af68")))
   ((>= signal 25) (propertize "fair"      'face '(:foreground "#ff9e64")))
   (t              (propertize "weak"      'face '(:foreground "#f7768e")))))

(defun network-manager--render ()
  "Render the network manager buffer."
  (let ((buf (get-buffer-create network-manager-buffer-name)))
    (with-current-buffer buf
      (let ((inhibit-read-only t)
            (pos (point)))
        (erase-buffer)
        ;; Header: status
        (let ((s network-manager--status))
          (if s
              (let ((label-face 'shadow))
                (insert (propertize "Network Status\n" 'face 'bold))
                (insert (propertize "  Connectivity: " 'face label-face)
                        (let ((c (plist-get s :connectivity)))
                          (if (equal c "full")
                              (propertize c 'face '(:foreground "#9ece6a"))
                            (propertize (or c "unknown") 'face '(:foreground "#f7768e"))))
                        "\n")
                (when (plist-get s :device)
                  (insert (propertize "  Device:       " 'face label-face)
                          (plist-get s :device) " (" (plist-get s :type) ")\n")
                  (insert (propertize "  Connection:   " 'face label-face)
                          (or (plist-get s :connection) "none") "\n"))
                ;; Wifi link details
                (when (plist-get s :signal)
                  (insert (propertize "  Signal:       " 'face label-face)
                          (network-manager--signal-indicator (plist-get s :signal))
                          (format " (%d%%)" (plist-get s :signal)) "\n")
                  (when (plist-get s :freq)
                    (insert (propertize "  Frequency:    " 'face label-face)
                            (plist-get s :freq)
                            (if (string-match-p "^5" (plist-get s :freq))
                                (propertize " (5 GHz)" 'face '(:foreground "#9ece6a"))
                              (propertize " (2.4 GHz)" 'face '(:foreground "#e0af68")))
                            "\n"))
                  (when (plist-get s :rate)
                    (insert (propertize "  Link speed:   " 'face label-face)
                            (plist-get s :rate) "\n")))
                ;; IP info
                (when (plist-get s :ip)
                  (insert (propertize "  IP address:   " 'face label-face)
                          (plist-get s :ip) "\n"))
                (when (plist-get s :gateway)
                  (insert (propertize "  Gateway:      " 'face label-face)
                          (plist-get s :gateway) "\n"))
                (when (plist-get s :dns)
                  (insert (propertize "  DNS:          " 'face label-face)
                          (plist-get s :dns) "\n"))
                ;; Traffic
                (when (plist-get s :rx)
                  (insert (propertize "  Traffic:      " 'face label-face)
                          (format "rx %s / tx %s"
                                  (network-manager--format-bytes (plist-get s :rx))
                                  (network-manager--format-bytes (plist-get s :tx)))
                          "\n"))
                ;; VPN
                (when (plist-get s :vpn)
                  (insert (propertize "  VPN:          " 'face label-face)
                          (propertize (string-join (plist-get s :vpn) ", ")
                                      'face '(:foreground "#9ece6a"))
                          "\n")))
            (insert (propertize "Gathering status...\n" 'face 'shadow))))

        ;; Wifi networks
        (insert "\n")
        (insert (propertize "WiFi Networks\n" 'face 'bold))
        (if network-manager--wifi-networks
            (progn
              (insert (propertize
                       (format "  %-4s %-30s %-10s %-6s %-14s %s\n"
                               "" "SSID" "Signal" "Band" "Rate" "Security")
                       'face 'shadow))
              (dolist (net network-manager--wifi-networks)
                (let* ((ssid (plist-get net :ssid))
                       (signal (plist-get net :signal))
                       (security (plist-get net :security))
                       (freq (or (plist-get net :freq) ""))
                       (rate (or (plist-get net :rate) ""))
                       (band (cond
                              ((string-match-p "^5" freq) "5G")
                              ((string-match-p "^2" freq) "2.4G")
                              (t "")))
                       (in-use (plist-get net :in-use)))
                  (insert (format "  %-4s %-30s %-10s %-6s %-14s %s\n"
                                  (if in-use (propertize "*" 'face '(:foreground "#9ece6a")) "")
                                  (if in-use
                                      (propertize ssid 'face '(:foreground "#9ece6a"))
                                    ssid)
                                  (network-manager--signal-indicator signal)
                                  (if (string-empty-p band) ""
                                    (if (equal band "5G")
                                        (propertize band 'face '(:foreground "#9ece6a"))
                                      (propertize band 'face '(:foreground "#e0af68"))))
                                  rate
                                  (if (string-empty-p security) "open" security))))))
          (insert (propertize "  Press 's' to scan\n" 'face 'shadow)))

        ;; Keybindings
        (insert "\n")
        (insert (propertize "Keys: " 'face 'shadow))
        (insert (propertize "g" 'face 'bold) (propertize "=refresh " 'face 'shadow))
        (insert (propertize "s" 'face 'bold) (propertize "=scan " 'face 'shadow))
        (insert (propertize "RET" 'face 'bold) (propertize "=connect " 'face 'shadow))
        (insert (propertize "d" 'face 'bold) (propertize "=disconnect " 'face 'shadow))
        (insert (propertize "D" 'face 'bold) (propertize "=forget " 'face 'shadow))
        (insert (propertize "e" 'face 'bold) (propertize "=dns " 'face 'shadow))
        (insert (propertize "r" 'face 'bold) (propertize "=router " 'face 'shadow))
        (insert (propertize "t" 'face 'bold) (propertize "=toggle-wifi " 'face 'shadow))
        (insert (propertize "q" 'face 'bold) (propertize "=quit" 'face 'shadow))
        (insert "\n")
        (goto-char (min pos (point-max)))))))

;; --- SSID at point ---

(defun network-manager--ssid-at-point ()
  "Return the SSID on the current line, or nil."
  (save-excursion
    (beginning-of-line)
    (when (looking-at "^  .\\{4\\} \\(.\\{1,30\\}?\\) +\\(excellent\\|good\\|fair\\|weak\\)")
      (string-trim (match-string 1)))))

;; --- Interactive commands ---

(defun network-manager-refresh ()
  "Refresh network status and redisplay."
  (interactive)
  (network-manager--gather-status
   (lambda ()
     (network-manager--scan-wifi
      (lambda (_nets)
        (network-manager--render))))))

(defun network-manager-scan ()
  "Rescan wifi networks."
  (interactive)
  (message "Scanning...")
  (network-manager--scan-wifi
   (lambda (_nets)
     (network-manager--render)
     (message "Scan complete"))))

(defun network-manager--connect-with-password (ssid &optional saved-p)
  "Prompt for password and connect to SSID.
If SAVED-P, deletes stale saved connection first, then uses
`device wifi connect' which creates a fresh profile with the password."
  (let ((password (read-passwd (format "Password for %s: " ssid))))
    (if saved-p
        ;; Delete stale profile, then connect fresh
        (let ((buf (generate-new-buffer " *nm-delete-stale*")))
          (set-process-sentinel
           (start-process "nm-delete-stale" buf "nmcli" "connection" "delete" ssid)
           (lambda (proc _event)
             (kill-buffer (process-buffer proc))
             (network-manager--run-nmcli
              (format "Connecting to %s..." ssid)
              "device" "wifi" "connect" ssid "password" password))))
      (network-manager--run-nmcli
       (format "Connecting to %s..." ssid)
       "device" "wifi" "connect" ssid "password" password))))

(defun network-manager--try-activate (ssid has-security)
  "Try `connection up SSID'. On secrets failure, prompt for password and retry."
  (message "Connecting to %s..." ssid)
  (let ((buf (generate-new-buffer " *nm-activate*")))
    (set-process-sentinel
     (start-process "nm-activate" buf "nmcli" "connection" "up" ssid)
     (lambda (proc _event)
       (let ((exit (process-exit-status proc))
             (output (with-current-buffer (process-buffer proc)
                       (string-trim (buffer-string)))))
         (kill-buffer (process-buffer proc))
         (if (eq exit 0)
             (progn
               (message "Connected to %s" ssid)
               (run-at-time 1 nil #'network-manager-refresh))
           (if (string-match-p "\\(?:Secrets\\|secret\\|password\\|Password\\|WPS\\)" output)
               (network-manager--connect-with-password ssid t)
             (message "nmcli error: %s" output))))))))

(defun network-manager-connect ()
  "Connect to the wifi network at point, or prompt for one."
  (interactive)
  (let ((ssid (or (network-manager--ssid-at-point)
                  (completing-read "Connect to: "
                                   (mapcar (lambda (n) (plist-get n :ssid))
                                           network-manager--wifi-networks)
                                   nil t))))
    (when (and ssid (not (string-empty-p ssid)))
      (let* ((net (seq-find (lambda (n) (equal (plist-get n :ssid) ssid))
                            network-manager--wifi-networks))
             (security (and net (plist-get net :security)))
             (has-security (and security (not (string-empty-p security))))
             (saved (seq-find (lambda (c) (equal (plist-get c :name) ssid))
                              network-manager--saved-connections)))
        (if saved
            ;; Saved — try activating, fall back to password prompt on secrets error
            (network-manager--try-activate ssid has-security)
          ;; New network
          (if has-security
              (network-manager--connect-with-password ssid)
            ;; Open network — connect but disable autoconnect to prevent
            ;; automatically joining untrusted hotspots in the future
            (progn
              (network-manager--run-nmcli
               (format "Connecting to %s..." ssid)
               "device" "wifi" "connect" ssid)
              (run-at-time 2 nil
                           (lambda ()
                             (network-manager--run-nmcli
                              "Disabling autoconnect for open network..."
                              "connection" "modify" ssid
                              "connection.autoconnect" "no"))))))))))

(defun network-manager-disconnect ()
  "Disconnect the current wifi connection."
  (interactive)
  (let ((conn (and network-manager--status
                   (plist-get network-manager--status :connection))))
    (if conn
        (when (y-or-n-p (format "Disconnect from %s? " conn))
          (network-manager--run-nmcli
           (format "Disconnecting from %s..." conn)
           "connection" "down" conn))
      (user-error "No active connection"))))

(defun network-manager-delete-connection ()
  "Delete (forget) a saved connection."
  (interactive)
  (network-manager--get-saved-connections
   (lambda (conns)
     (let* ((names (mapcar (lambda (c) (plist-get c :name)) conns))
            (choice (completing-read "Forget connection: " names nil t)))
       (when (y-or-n-p (format "Forget '%s'? " choice))
         (network-manager--run-nmcli
          (format "Forgetting %s..." choice)
          "connection" "delete" choice))))))

(defun network-manager--active-connection ()
  "Return the name of the active connection, or nil."
  (when-let ((conn (and network-manager--status
                        (plist-get network-manager--status :connection))))
    (unless (string-empty-p conn) conn)))

(defun network-manager-set-dns ()
  "Set DNS servers for the active connection from presets."
  (interactive)
  (let ((conn (network-manager--active-connection)))
    (unless conn (user-error "No active connection"))
    (let* ((names (mapcar #'car network-manager-dns-presets))
           (choice (completing-read (format "DNS for %s: " conn) names nil t))
           (servers (alist-get choice network-manager-dns-presets nil nil #'equal)))
      (if servers
          (progn
            (network-manager--run-nmcli
             (format "Setting DNS to %s..." choice)
             "connection" "modify" conn
             "ipv4.dns" servers
             "ipv4.ignore-auto-dns" "yes")
            (run-at-time 1.5 nil
                         (lambda ()
                           (network-manager--run-nmcli
                            "Reactivating connection..."
                            "connection" "up" conn))))
        ;; Auto — revert to DHCP
        (network-manager--run-nmcli
         "Reverting DNS to auto..."
         "connection" "modify" conn
         "ipv4.dns" ""
         "ipv4.ignore-auto-dns" "no")
        (run-at-time 1.5 nil
                     (lambda ()
                       (network-manager--run-nmcli
                        "Reactivating connection..."
                        "connection" "up" conn)))))))

(defun network-manager-open-router ()
  "Open the default gateway (router admin page) in a browser."
  (interactive)
  (let ((gw (and network-manager--status
                 (plist-get network-manager--status :gateway))))
    (if (and gw (not (string-empty-p gw)))
        (browse-url (format "http://%s" gw))
      (user-error "No gateway detected"))))

(defun network-manager-toggle-wifi ()
  "Toggle wifi on/off."
  (interactive)
  (let ((buf (generate-new-buffer " *nm-radio*")))
    (set-process-sentinel
     (start-process "nm-radio-check" buf "nmcli" "radio" "wifi")
     (lambda (proc _event)
       (let ((state (when (eq (process-exit-status proc) 0)
                      (with-current-buffer (process-buffer proc)
                        (string-trim (buffer-string))))))
         (kill-buffer (process-buffer proc))
         (let ((new-state (if (equal state "enabled") "off" "on")))
           (network-manager--run-nmcli
            (format "Turning wifi %s..." new-state)
            "radio" "wifi" new-state)))))))

;; --- Async nmcli runner ---

(defun network-manager--run-nmcli (msg &rest args)
  "Run nmcli with ARGS asynchronously, showing MSG. Refresh on completion."
  (message "%s" msg)
  (let ((buf (generate-new-buffer " *nm-cmd*")))
    (set-process-sentinel
     (apply #'start-process "nm-cmd" buf "nmcli" args)
     (lambda (proc _event)
       (let ((exit (process-exit-status proc))
             (output (with-current-buffer (process-buffer proc)
                       (string-trim (buffer-string)))))
         (kill-buffer (process-buffer proc))
         (if (eq exit 0)
             (progn
               (message "%s done" msg)
               ;; Small delay for NM to settle, then refresh
               (run-at-time 1 nil #'network-manager-refresh))
           (message "nmcli error: %s" output)))))))

;; --- Auto-refresh timer ---

(defun network-manager--start-timer ()
  "Start the auto-refresh timer."
  (network-manager--stop-timer)
  (setq network-manager--timer
        (run-at-time network-manager-refresh-interval
                     network-manager-refresh-interval
                     (lambda ()
                       (when (get-buffer network-manager-buffer-name)
                         (network-manager--gather-status
                          (lambda () (network-manager--render))))))))

(defun network-manager--stop-timer ()
  "Stop the auto-refresh timer."
  (when network-manager--timer
    (cancel-timer network-manager--timer)
    (setq network-manager--timer nil)))

;; --- Entry point ---

;;;###autoload
(defun network-manager ()
  "Open the network manager buffer."
  (interactive)
  (let ((buf (get-buffer-create network-manager-buffer-name)))
    (with-current-buffer buf
      (network-manager-mode))
    (switch-to-buffer buf)
    (network-manager--get-saved-connections #'ignore)
    (network-manager-refresh)
    (network-manager--start-timer)
    (add-hook 'kill-buffer-hook #'network-manager--stop-timer nil t)))

(global-set-key (kbd "C-x y n") #'network-manager)

(provide 'network-manager)

;;; network-manager.el ends here
