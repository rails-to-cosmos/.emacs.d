(use-package dockerfile-mode
  :ensure t)

(cl-defun my-docker-container-running-p (container)
  "Check if a Docker CONTAINER is running.
Returns t if running, nil otherwise."
  (string= (-> (format "docker inspect -f '{{.State.Running}}' %s 2>/dev/null" container)
               (shell-command-to-string)
               (s-trim))
           "true"))

(defun my-docker-eshell (container dir)
  "Open Eshell via TRAMP in a Docker container.
If CONTAINER is running, open Eshell in /docker:{CONTAINER}:/workspace/.
If not, run `docker compose up -d` in DIR, wait for it to start (up to 5 minutes), then open Eshell."
  (interactive "sContainer name: \nDDirectory for docker-compose: ")

  (let ((default-directory dir))
    (if (my-docker-container-running-p container)
        (message "Container %s is already running." container)

      (message "Container %s is not running. Starting with docker compose up -d..." container)
      (shell-command "docker compose up -d")

      (let ((elapsed-time 0)
            (max-wait-time 300)
            (interval 1))

        (while (and (< elapsed-time max-wait-time)
                    (not (my-docker-container-running-p container)))
          (message "Waiting for container %s to start... (%d/%d seconds)"
                   container elapsed-time max-wait-time)
          (sit-for interval)
          (setq elapsed-time (+ elapsed-time interval)))

        (if (my-docker-container-running-p container)
            (message "Container %s started successfully!" container)
          (error "Container %s did not start within %d seconds" container max-wait-time)))))

  (let ((tramp-path (format "/docker:%s:/workspace/" container)))
    (message "Opening Eshell in %s..." tramp-path)
    (eshell-browse-directory tramp-path)))

(provide 'init-docker)
