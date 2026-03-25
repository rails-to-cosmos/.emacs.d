;;; network-manager.el --- NetworkManager interface for Emacs -*- lexical-binding: t; -*-

(require 'seq)

;; --- Customizable faces & variables ---

(defvar network-manager-refresh-interval 10
  "Seconds between automatic status refreshes.")

(defvar network-manager--timer nil
  "Timer for periodic status refresh.")

(defvar network-manager--status nil
  "Current network status plist.
Keys: :connectivity :device :type :state :connection :signal :ssid")

(defvar network-manager--wifi-networks nil
  "List of available wifi networks, each a plist.
Keys: :ssid :signal :security :in-use")

(defvar network-manager--saved-connections nil
  "List of saved connection plists.
Keys: :name :type :autoconnect")

(defvar network-manager-buffer-name "*network*"
  "Name of the network manager buffer.")

;; --- Keymap ---

(defvar network-manager-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "g"       #'network-manager-refresh)
    (define-key map "s"       #'network-manager-scan)
    (define-key map (kbd "RET") #'network-manager-connect)
    (define-key map "d"       #'network-manager-disconnect)
    (define-key map "D"       #'network-manager-delete-connection)
    (define-key map "t"       #'network-manager-toggle-wifi)
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

(defun network-manager--gather-status (callback)
  "Asynchronously gather network status, then call CALLBACK."
  (let* ((result (list :connectivity nil :device nil :type nil
                       :state nil :connection nil :ssid nil :signal nil))
         (pending 2)
         (done-fn (lambda ()
                    (setq pending (1- pending))
                    (when (= pending 0)
                      (setq network-manager--status result)
                      (funcall callback)))))
    ;; Connectivity
    (let ((buf (generate-new-buffer " *nm-connectivity*")))
      (set-process-sentinel
       (start-process "nm-connectivity" buf "nmcli" "networking" "connectivity")
       (lambda (proc _event)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (plist-put result :connectivity (string-trim (buffer-string)))))
         (kill-buffer (process-buffer proc))
         (funcall done-fn))))
    ;; Active device info
    (let ((buf (generate-new-buffer " *nm-device*")))
      (set-process-sentinel
       (start-process "nm-device" buf
                      "nmcli" "-t" "-f" "DEVICE,TYPE,STATE,CONNECTION" "device")
       (lambda (proc _event)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (let ((lines (network-manager--parse-lines (buffer-string))))
               (when-let ((active (seq-find
                                   (lambda (l) (string-match-p ":connected" l))
                                   lines)))
                 (let ((fields (split-string active ":")))
                   (plist-put result :device (nth 0 fields))
                   (plist-put result :type (nth 1 fields))
                   (plist-put result :state (nth 2 fields))
                   (plist-put result :connection (nth 3 fields)))))))
         (kill-buffer (process-buffer proc))
         (funcall done-fn))))))

(defun network-manager--scan-wifi (callback)
  "Asynchronously scan for wifi networks, then call CALLBACK with results."
  (let ((buf (generate-new-buffer " *nm-wifi*")))
    (set-process-sentinel
     (start-process "nm-wifi-scan" buf
                    "nmcli" "-t" "-f" "SSID,SIGNAL,SECURITY,IN-USE"
                    "device" "wifi" "list" "--rescan" "yes")
     (lambda (proc _event)
       (let (networks)
         (when (eq (process-exit-status proc) 0)
           (with-current-buffer (process-buffer proc)
             (dolist (line (network-manager--parse-lines (buffer-string)))
               (let ((fields (split-string line ":")))
                 (when (and (>= (length fields) 3)
                            (not (string-empty-p (nth 0 fields))))
                   (let ((ssid (nth 0 fields))
                         (signal (string-to-number (nth 1 fields)))
                         (security (nth 2 fields))
                         (in-use (string= "*" (nth 3 fields))))
                     ;; Deduplicate, keeping highest signal
                     (if-let ((existing (seq-find (lambda (n) (equal (plist-get n :ssid) ssid))
                                                  networks)))
                         (when (> signal (plist-get existing :signal))
                           (plist-put existing :signal signal)
                           (plist-put existing :in-use (or in-use (plist-get existing :in-use))))
                       (push (list :ssid ssid :signal signal
                                   :security security :in-use in-use)
                             networks))))))))
         (kill-buffer (process-buffer proc))
         (setq networks (sort networks (lambda (a b)
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
              (progn
                (insert (propertize "Network Status\n" 'face 'bold))
                (insert (format "  Connectivity: %s\n"
                                (let ((c (plist-get s :connectivity)))
                                  (if (equal c "full")
                                      (propertize c 'face '(:foreground "#9ece6a"))
                                    (propertize (or c "unknown") 'face '(:foreground "#f7768e"))))))
                (when (plist-get s :device)
                  (insert (format "  Device:       %s (%s)\n"
                                  (plist-get s :device)
                                  (plist-get s :type)))
                  (insert (format "  Connection:   %s\n"
                                  (or (plist-get s :connection) "none")))))
            (insert (propertize "Gathering status...\n" 'face 'shadow))))

        ;; Wifi networks
        (insert "\n")
        (insert (propertize "WiFi Networks\n" 'face 'bold))
        (if network-manager--wifi-networks
            (progn
              (insert (propertize
                       (format "  %-4s %-30s %-8s %s\n" "" "SSID" "Signal" "Security")
                       'face 'shadow))
              (dolist (net network-manager--wifi-networks)
                (let ((ssid (plist-get net :ssid))
                      (signal (plist-get net :signal))
                      (security (plist-get net :security))
                      (in-use (plist-get net :in-use)))
                  (insert (format "  %-4s %-30s %-8s %s\n"
                                  (if in-use (propertize "*" 'face '(:foreground "#9ece6a")) "")
                                  (if in-use
                                      (propertize ssid 'face '(:foreground "#9ece6a"))
                                    ssid)
                                  (network-manager--signal-indicator signal)
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
             ;; Check if we have a saved connection for this SSID
             (saved (seq-find (lambda (c) (equal (plist-get c :name) ssid))
                              network-manager--saved-connections)))
        (if saved
            ;; Already saved — just activate
            (network-manager--run-nmcli
             (format "Connecting to %s..." ssid)
             "connection" "up" ssid)
          ;; New network
          (if has-security
              (let ((password (read-passwd (format "Password for %s: " ssid))))
                (network-manager--run-nmcli
                 (format "Connecting to %s..." ssid)
                 "device" "wifi" "connect" ssid "password" password))
            (network-manager--run-nmcli
             (format "Connecting to %s..." ssid)
             "device" "wifi" "connect" ssid)))))))

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

(provide 'network-manager)

;;; network-manager.el ends here
