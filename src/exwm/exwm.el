(require 'exwm)
(require 'exwm-config)
(require 'exwm-randr)

(exwm-randr-enable)
(exwm-enable)

(defun exwm-auto-buffer-name ()
  (interactive)
  (let* ((buffer (current-buffer))
         (id (exwm--buffer->id buffer)))
    (when id
      (let ((getter (make-instance 'xcb:ewmh:get-_NET_WM_NAME :window id)))
        (rename-buffer
         (concat "*exwm:"
                 (slot-value (xcb:+request-unchecked+reply exwm--connection getter) 'value)
                 "*")
         t)))))

(add-hook 'exwm-mode-hook #'exwm-auto-buffer-name)

(defun exwm-rofi ()
  (interactive)
  (call-process "rofi" nil nil nil "-show"))

(defun exwm-switch-layout ()
  (interactive)
  (call-process "switch-layout"))

(defun exwm-brightness ()
  (string-to-number (car (last (s-split " " (s-trim (shell-command-to-string "brightness")))))))

(defun exwm-brightness-down ()
  (interactive)
  (when (> (exwm-brightness) 0)
    (call-process "brightness" nil nil nil "Down")))

(defun exwm-brightness-up ()
  (interactive)
  (when (< (exwm-brightness) 1)
    (call-process "brightness" nil nil nil "Up")))

(defun exwm-reboot ()
  (interactive)
  (add-hook 'kill-emacs-hook #'(lambda () (call-process "reboot")))
  (kill-emacs))

(defun exwm-shutdown ()
  (interactive)
  (call-process "shutdown" nil nil nil "now"))

(defun exwm-minikube-dashboard ()
  (interactive)
  (start-process "minikube-dashboard" "*minikube-dashboard*" "minikube" "dashboard"))

(defun exwm-volume-up ()
  (interactive)
  (let ((process (start-process "volume-up" "*volume-up*" "pactl" "--" "set-sink-volume" "0" "+10%")))
    ;; (set-process-sentinel process (lambda (&rest _) (eye-refresh)))
    ))

(defun exwm-volume-down ()
  (interactive)
  (start-process "volume-down" "*volume-down*" "pactl" "--" "set-sink-volume" "0" "-10%"))

(defun exwm-volume-mute ()
  (interactive)
  (start-process "volume-mute" "*volume-mute*" "pactl" "--" "set-sink-volume" "0" "0%"))

(defun exwm-samsung-buds-connect ()
  (interactive)
  (start-process "samsung-buds-connect" "*samsung-buds*" "bluetoothctl" "connect" "70:CE:8C:7C:CD:E7"))

(setq exwm-input-global-keys
      (list
       ;; (cons (kbd "C-M-<SPC>") #'my-app-launcher)
       (cons (kbd "C-M-<SPC>") #'exwm-rofi)
       ;; (cons (kbd "C-x l") #'exwm-switch-layout)
       (cons (kbd "<f7>") #'exwm-brightness-down)
       (cons (kbd "<f8>") #'exwm-brightness-up)
       (cons (kbd "<f1>") #'exwm-volume-mute)
       (cons (kbd "<f2>") #'exwm-volume-down)
       (cons (kbd "<f3>") #'exwm-volume-up)
       (cons (kbd "<f11>") #'exwm-switch-layout)))

(define-key exwm-mode-map (kbd "C-q") #'exwm-input-send-next-key)

(define-key exwm-mode-map [?\s-x] 'exwm-input-send-next-key)

;; (defvar exwm-bluetooth-enabled (persist-load 'exwm-bluetooth-enabled))

;; (cond (exwm-bluetooth-enabled (start-process "bluetoothctl" "*exwm*" "bluetoothctl" "--" "power" "on"))
;;       (t (start-process "bluetoothctl" "*exwm*" "bluetoothctl" "--" "power" "off")))

;; (defun exwm-bluetooth-persist ()
;;   (persist-save 'exwm-bluetooth-enabled))

;; (add-hook 'kill-emacs-hook #'exwm-bluetooth-persist)
