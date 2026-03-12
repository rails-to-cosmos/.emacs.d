;;; inxi-mode-line.el --- Display inxi system info in the mode-line -*- lexical-binding: t -*-

;;; Commentary:
;; This package provides a minor mode, `inxi-mode-line-mode`, which
;; displays system information from the `inxi` utility in the mode-line.
;; It periodically updates information such as CPU speed, memory usage,
;; and CPU temperature. If the `inxi` command is not found in your
;; system's PATH, the mode will issue a warning and disable itself.

;;; Code:

(defgroup inxi-mode-line nil
  "Display system information from inxi in the mode-line."
  :group 'mode-line)

(defcustom inxi-mode-line-update-interval 60
  "The update interval in seconds for inxi data."
  :type 'integer
  :group 'inxi-mode-line)

(defcustom inxi-mode-line-inxi-args '("-c0" "-v1" "-C" "-M" "-s")
  "Arguments to pass to the inxi utility.
Defaults are chosen to fetch CPU, Memory, and Sensor (temp) data."
  :type '(repeat string)
  :group 'inxi-mode-line)

(defcustom inxi-mode-line-format " [CPU:%s Mem:%s Temp:%s] "
  "Format string for the mode-line display.
Receives CPU speed, Memory usage, and CPU Temperature strings as arguments."
  :type 'string
  :group 'inxi-mode-line)

(defvar inxi-mode-line-string ""
  "The string to be displayed in the mode-line.")

(defvar inxi-mode-line-process nil
  "The process running inxi.")

(defvar inxi-mode-line-timer nil
  "Timer for periodically updating inxi info.")

(defun inxi-mode-line--parse-output (output)
  "Parse OUTPUT from inxi and return a plist."
  (prin1 output)
  (let ((cpu "N/A") (mem "N/A") (temp "N/A"))
    (with-temp-buffer
      (insert output)
      (goto-char (point-min))
      ;; Extract CPU speed (e.g., "speed: 2900 MHz")
      (when (re-search-forward "CPU:.+ Speed (MHz): \\([0-9.]+\\)" nil t)
        (setq cpu (format "%.1fGHz" (/ (string-to-number (match-string 1)) 1000.0))))
      (goto-char (point-min))
      ;; Extract Memory (e.g., "used: 11.83 GiB")
      (when (re-search-forward "Memory:.+used: \\([0-9.]+\\s[A-Za-z]+\\b\\)" nil t)
        (setq mem (match-string 1)))
      (goto-char (point-min))
      ;; Extract CPU Temperature (e.g., "cpu: 45.0 C")
      (when (re-search-forward "[Ss]ensors:.+cpu: \\([0-9.]+\\)" nil t)
        (setq temp (concat (match-string 1) "C"))))
    `(:cpu ,cpu :mem ,mem :temp ,temp)))

(defun inxi-mode-line--update-string (data)
  "Update `inxi-mode-line-string` from parsed DATA plist."
  (if data
      (let ((cpu (plist-get data :cpu))
            (mem (plist-get data :mem))
            (temp (plist-get data :temp)))
        (setq inxi-mode-line-string
              (propertize
               (format inxi-mode-line-format cpu mem temp)
               'face 'mode-line-emphasis
               'help-echo (format "Inxi Info\nCPU Speed: %s\nMemory Used: %s\nCPU Temp: %s"
                                  cpu mem temp))))
    (setq inxi-mode-line-string "[inxi error]")))

(defun inxi-mode-line--process-sentinel (process event)
  "Process sentinel for the inxi command."
  (when (memq (process-status process) '(exit signal))
    (let ((output (with-current-buffer (process-buffer process)
                    (buffer-string))))
      (kill-buffer (process-buffer process))
      (if (zerop (process-exit-status process))
          (let ((parsed-data (inxi-mode-line--parse-output output)))
            (inxi-mode-line--update-string parsed-data))
        (message "inxi-mode-line: inxi process failed. %s" event)
        (setq inxi-mode-line-string "[inxi error]")))))

(defun inxi-mode-line--fetch-data ()
  "Asynchronously fetch system information using inxi."
  (when (and (or (null inxi-mode-line-process)
                 (not (process-live-p inxi-mode-line-process)))
             (executable-find "inxi"))
    (setq inxi-mode-line-process
          (apply #'start-process "inxi" (generate-new-buffer "*inxi-output*") "inxi" inxi-mode-line-inxi-args))
    (set-process-sentinel inxi-mode-line-process #'inxi-mode-line--process-sentinel)))

;;;###autoload
(define-minor-mode inxi-mode-line-mode
  "Display system information from inxi in the mode-line."
  :lighter " Inxi"
  :global t
  (if inxi-mode-line-mode
      ;; When enabling the mode
      (if (not (executable-find "inxi"))
          (progn
            (message "inxi-mode-line-mode: 'inxi' utility not found. Disabling.")
            (setq inxi-mode-line-mode nil)) ; Abort enabling
        (setq inxi-mode-line-string " [inxi loading...]")
        (add-to-list 'mode-line-format '(:eval inxi-mode-line-string) t)
        (setq inxi-mode-line-timer
              (run-with-timer 0 inxi-mode-line-update-interval
                              #'inxi-mode-line--fetch-data)))
    ;; When disabling the mode
    (when inxi-mode-line-timer
      (cancel-timer inxi-mode-line-timer)
      (setq inxi-mode-line-timer nil))
    (when (and inxi-mode-line-process (process-live-p inxi-mode-line-process))
      (kill-process inxi-mode-line-process)
      (setq inxi-mode-line-process nil))
    (setq mode-line-format (delete '(:eval inxi-mode-line-string) mode-line-format))
    (setq inxi-mode-line-string "")))

(provide 'inxi-mode-line)

;;; inxi-mode-line.el ends here
