;; -*- lexical-binding: t; -*-

(provide 'exwm-system-report)

(require 'a)
(require 'battery)
(require 'ctable)
(require 'dash)
(require 'pcase)
(require 's)
(require 'simple)

(defvar exwm-system-report-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)

    (define-key map "n" 'forward-line)
    (define-key map "p" 'previous-line)
    (define-key map "g" 'exwm-system-report)

    map))

;; Sorters

(defun boolsort (lhs rhs)
  (cond
    ((not (or lhs rhs)) 0)
    (lhs -1)
    (rhs 1)
    (t 0)))

(defun sigsort (lhs rhs)
  (ctbl:sort-number-lessp
   (s-count-matches "_" lhs)
   (s-count-matches "_" rhs)))

(defun numsort (lhs rhs)
  "Sort collection of strings by first number parsed."
  (ctbl:sort-number-lessp
   (string-to-number lhs)
   (string-to-number rhs)))

(define-derived-mode exwm-system-report-mode special-mode
  #("" 0 1 (rear-nonsticky t display (raise -0.15) font-lock-face (:family "file-icons" :height 1.2) face (:family "file-icons" :height 1.2)))
  "Special mode extended to work with ctbl.")

(defun ctbl::sort-current ()
  (interactive)
  (let ((cp (ctbl:cp-get-component))
        (cell-id (ctbl:cursor-to-nearest-cell)))
    (when (and cp cell-id)
      (ctbl:cmodel-sort-action cp (cdr (ctbl:cp-get-selected cp)))

      (ctbl:cp-set-selected-cell cp cell-id))))

;; (defun ctbl::filter-current ()
;;   (interactive)
;;   (let ((cp (ctbl:cp-get-component))
;;         (cell-id (ctbl:cursor-to-nearest-cell)))
;;     (when (and cp cell-id)
;;       (ctbl:cmodel-sort-action cp (cdr (ctbl:cp-get-selected cp)))
;;       (ctbl:cp-set-selected-cell cp cell-id))))

(defun ctbl::navi-move-backward ()
  (interactive)
  (let* ((cp (ctbl:cp-get-component))
         (cell-id (ctbl:cursor-to-nearest-cell))
         (row-id (car cell-id)))
    (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
      (ctbl:navi-move-left)
      (unless (cursor-moved-p)
        (ctbl:navi-move-up)
        (if (cursor-moved-p)
            (ctbl:navi-move-right-most)
          (forward-line -4))))))

(defun ctbl::navi-move-forward ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (let* ((cell-id (ctbl:cursor-to-nearest-cell))
           (row-id (car cell-id)))
      (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
        (ctbl:navi-move-right)
        (unless (cursor-moved-p)
          (ctbl:navi-move-down)
          (if (cursor-moved-p)
              (ctbl:navi-move-left-most)
            (forward-line 2)))))))

(defun ctbl::navi-move-down ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (let* ((cell-id (ctbl:cursor-to-nearest-cell))
           (row-id (car cell-id)))
      (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
        (ctbl:navi-move-down)
        (unless (cursor-moved-p)
          (forward-line 2))))))

(defun ctbl::navi-move-up ()
  (interactive)
  (when-let (cp (condition-case nil
                    (ctbl:cp-get-component)
                  (error nil)))
    (let* ((cell-id (ctbl:cursor-to-nearest-cell))
           (row-id (car cell-id)))
      (cl-flet ((cursor-moved-p () (not (eql cell-id (ctbl:cursor-to-nearest-cell)))))
        (ctbl:navi-move-up)
        (unless (cursor-moved-p)
          (forward-line -4))))))

(defvar exwm-system-report-nm-schema
  (list (make-ctbl:cmodel :title ""
                          :align 'right
                          :min-width 1
                          :sorter #'boolsort)
        (make-ctbl:cmodel :title "Name"
                          :align 'left)
        (make-ctbl:cmodel :title "Signal"
                          :align 'left
                          :sorter #'sigsort)
        (make-ctbl:cmodel :title "Channel"
                          :align 'left
                          :sorter #'numsort)
        (make-ctbl:cmodel :title "Rate"
                          :align 'left
                          :sorter #'numsort)
        (make-ctbl:cmodel :title "Security"
                          :align 'left)))

(defvar exwm-system-report-nm-map
  (ctbl:define-keymap
   '(("k" . ctbl::navi-move-up)
     ("j" . ctbl::navi-move-down)
     ("h" . ctbl:navi-move-left)
     ("l" . ctbl:navi-move-right)

     ("p" . ctbl::navi-move-up)
     ("n" . ctbl::navi-move-down)
     ("b" . ctbl:navi-move-left)
     ("f" . ctbl:navi-move-right)

     ("c" . ctbl:navi-jump-to-column)

     ("e" . ctbl:navi-move-right-most)
     ("a" . ctbl:navi-move-left-most)

     ("g" . exwm-system-report:refresh)

     ("?" . ctbl:describe-bindings)

     ([mouse-1] . ctbl:navi-on-click)
     ("C-m" . ctbl:navi-on-click)
     ("RET" . ctbl:navi-on-click)
     ("TAB" . ctbl::navi-move-forward)
     ("<backtab>" . ctbl::navi-move-backward)

     ("^" . ctbl::sort-current))))

(defvar exwm-system-report-data (a-list))

(defun exwm-system-report--set-date-time ()
  (setq exwm-system-report-data
        (let ((now (current-time)))
          (a-assoc exwm-system-report-data
                   :date (format-time-string "%d %b %Y, %a" now)
                   :time (format-time-string "%H:%M %Z" now)))))

(defun exwm-system-report--set-battery-status ()
  (setq exwm-system-report-data
        (let ((battery-status (funcall battery-status-function)))
          (a-assoc exwm-system-report-data
                   :power (battery-format "%L" battery-status)
                   :battery (battery-format "%B" battery-status)
                   :load (battery-format "%p%%" battery-status)
                   :remaining (battery-format "%t" battery-status)))))

(defun exwm-system-report--set-network-status ()
  (setq exwm-system-report-data
        (let ((battery-status (funcall battery-status-function)))
          (a-assoc exwm-system-report-data
                   :network (s-trim (shell-command-to-string "nmcli networking connectivity"))))))

(defvar exwm-monitoring-daemons
  #'(exwm-system-report--set-date-time
     exwm-system-report--set-battery-status
     exwm-system-report--set-network-status
     exwm-system-report--refresh)
  "List of functions to run in a background.")

(defvar exwm-monitoring-timers '()
  "List of active timers.")

(defconst exwm-eye-mode-lighter
  #(" " 0 1 (rear-nonsticky t display nil font-lock-face (:family "FontAwesome" :height 1.2) face (:family "FontAwesome" :height 1.2))))

(define-minor-mode exwm-eye-mode
    "Monitor exwm system in a background and refresh `exwm-system-report-data'."
  nil exwm-eye-mode-lighter nil)

(define-globalized-minor-mode global-exwm-eye-mode
    exwm-eye-mode exwm-eye-mode nil
    (cond (global-exwm-eye-mode (cl-loop for daemon in (reverse exwm-monitoring-daemons)
                                   initially (setq exwm-system-report-data nil)
                                   do (funcall daemon)
                                   do (pushnew (run-with-timer 1 60 daemon) exwm-monitoring-timers)))
          (t (mapc #'cancel-timer exwm-monitoring-timers)
             (setq exwm-monitoring-timers '()))))

(defvar exwm-system-report-buffer "*exwm-system-report*")

(defun exwm-system-report ()
  (interactive)
  (let* ((data (cl-loop for (key . value) in exwm-system-report-data
                  collect (list (->> key
                                     symbol-name
                                     s-titleize
                                     (s-replace-regexp "^:" ""))
                                value)))
         (schema (list (make-ctbl:cmodel :title "Key")
                       (make-ctbl:cmodel :title "Value")))
         (model (make-ctbl:model :column-model schema :data data)))
    (with-current-buffer (get-buffer-create exwm-system-report-buffer)
      (exwm-system-report-mode)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert "Overview\n")
        (ctbl:create-table-component-region
         :model model
         :keymap exwm-system-report-nm-map)))))

(defun exwm-system-report--refresh ()
  (interactive)
  (save-window-excursion
    (with-current-buffer (get-buffer-create exwm-system-report-buffer)
      (let ((point (point)))
        (exwm-system-report)
        (goto-char point)
        (condition-case nil
            (ctbl:navi-goto-cell (ctbl:cursor-to-cell))
          (error nil))))))

(defun exwm-network-report ()
  (interactive)
  (let* ((buffer (get-buffer-create exwm-system-report-buffer))

         (process (start-process "nmcli" buffer "nmcli" "-t" "device" "wifi"))
         (process-output-parser (lambda (line)
                                  (let* ((separator ":")
                                         (escape-symbol "\\")
                                         (parted (let ((group 0)) (--partition-by (if (s-ends-with? escape-symbol it)
                                                                                      (1+ group)
                                                                                    (incf group))
                                                                                  (s-split separator line)))))
                                    (cl-loop for chunk in parted
                                       if (= 1 (length chunk))
                                       collect (car chunk)
                                       else
                                       collect (s-replace escape-symbol "" (s-join separator chunk))))))

         (schema exwm-system-report-nm-schema)
         (data nil))

    (set-process-filter process
                        (lambda (process output)
                          (cl-loop
                             for line in (s-split "\n" output)
                             do (-let [(in-use bssid ssid mode channel rate signal bars security) (funcall process-output-parser line)]
                                  (when ssid
                                    (cl-pushnew (list (string= in-use "*") ssid bars channel rate security) data))))))

    (set-process-sentinel process
                          (lambda (process status)
                            (when (string= (s-trim status) "finished")
                              (with-current-buffer buffer

                                (exwm-system-report-mode)

                                (let ((inhibit-read-only t))
                                  (delete-region (point-min) (point-max))
                                  (insert "Wireless\n")
                                  (let* ((model (make-ctbl:model
                                                 :column-model schema
                                                 :data data
                                                 :sort-state '(3)))
                                         (component (ctbl:create-table-component-region
                                                     :model model
                                                     :keymap exwm-system-report-nm-map)))

                                    (ctbl:cp-add-click-hook
                                     component
                                     (lambda ()
                                       (message "CTable : Click Hook [%S] [%S] [%S]"
                                                (ctbl:cp-get-selected component)
                                                (ctbl:cp-get-selected-data-row component)
                                                (ctbl:cp-get-selected-data-cell component)))))))
                              (unless (eql (current-buffer) buffer)
                                (switch-to-buffer-other-window buffer)))))))
