;;; bluetooth.el --- A mode for interacting with Bluetooth devices  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2024 Free Software Foundation, Inc.

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;;         Etienne Prud’homme <e.e.f.prudhomme@gmail.com>
;;
;; Maintainer: Raffael Stocker <r.stocker@mnet-mail.de>
;; Created: 13 Aug 2019
;; Version: 0.4.1
;; Package-Requires: ((emacs "26.1") (dash "2.18.1") (compat "30.0.0.0") (transient "0.5.0"))
;; Keywords: hardware
;; URL: https://codeberg.org/rstocker/emacs-bluetooth

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package implements basic Bluetooth management functionality, such as
;; connecting and disconnecting devices, setting properties and aliases,
;; putting the adapter in discovery mode and controlling its power supply.
;; It also includes a pairing agent.
;; It uses the Bluez Bluetooth stack on GNU/Linux via the D-Bus interface.
;; Therefore, it requires an Emacs with D-Bus support compiled-in.
;;
;; To use the package, invoke `bluetooth-list-devices'.

;;; Code:

(require 'dbus)
(require 'cl-lib)
(require 'let-alist)
(require 'dash)
(require 'rx)
(require 'compat)
(eval-when-compile (require 'subr-x))
(require 'bluetooth-pa)
(require 'bluetooth-lib)
(require 'bluetooth-device)
(require 'bluetooth-uuid)
(require 'bluetooth-plugin)
(require 'transient)


;;;; customization

(defgroup bluetooth nil
  "Bluetooth device management."
  :group 'comm)

(defcustom bluetooth-update-interval 2
  "Update interval of the device list view."
  :type '(natnum))

(defcustom bluetooth-list-format
  '(("Alias" 24) ("Paired" 8) ("Connected" 11) ("Address" 18)
    ("Blocked" 9) ("Trusted" 9))
  "The list view format for Bluetooth mode.
Each element describes the header and the width of a table
column.

NOTE: the headers MUST correspond to Bluez device properties
as they are used to gather the information from Bluez."
  :type '(alist :key-type string :value-type (group natnum)))

(defgroup bluetooth-faces nil
  "Faces used by Bluetooth mode."
  :group 'faces)

(defface bluetooth-info-heading
  '((t . (:foreground "royal blue" :weight bold)))
  "Face for device info headings.")

(defface bluetooth-info-attribute
  '((t . (:slant italic)))
  "Face for device attribute names.")


;;;; internal constants and variables

(defconst bluetooth-buffer-name "*Bluetooth*"
  "Name of the buffer in which to list Bluetooth devices.")

(defconst bluetooth-info-buffer-name "*Bluetooth info*"
  "Name of the Bluetooth info buffer.")

(defconst bluetooth--mode-name "Bluetooth" "Pretty print mode name.")

(defvar bluetooth--mode-info
  '(:eval (bluetooth--mode-info))
  "Mode info display.")

(put 'bluetooth--mode-info 'risky-local-variable t)

(defvar bluetooth--initialized nil)

(cl-defstruct bluetooth-property
  "Bluetooth state information for the mode line.
This structure holds the texts shown in active and inactive state
of a property."
  active-p
  (active-text nil :read-only t)
  (inactive-text nil :read-only t))

(defun bluetooth-property-text (property)
  "Return the text describing the state of PROPERTY."
  (if (bluetooth-property-active-p property)
      (bluetooth-property-active-text property)
    (bluetooth-property-inactive-text property)))


(defvar bluetooth--mode-state `(("Powered" . ,(make-bluetooth-property
                                               :inactive-text "off"))
                                ("Discoverable" . ,(make-bluetooth-property
                                                    :active-text "discoverable"))
                                ("Pairable" . ,(make-bluetooth-property
                                                :active-text "pairable"))
                                ("Discovering" . ,(make-bluetooth-property
                                                   :active-text "scan")))
  "Mode line adapter state information.

The state information list defines the kind of adapter state
displayed in the mode-line.  The first element of each sub-list
is an adapter property, the second is a ‘bluetooth-property’
structure containing the
 - current status of the item (t or nil),
 - string displayed if the property is non-nil,
 - string displayed if the property is nil.

If a display element is nil, nothing will be displayed for this
property and state.")

;; this variable holds the adapter signal handler object to allow clean-up in
;; the kill-buffer-hook
(defvar bluetooth--adapter-signal nil "D-Bus adapter signal object.")

(defvar bluetooth--update-timer nil
  "The Bluetooth device table update timer.")

(defvar bluetooth--follow-last-id nil)


;;;; internal functions

;; This function provides the list entries for the tabulated-list
;; view.  It is called from `tabulated-list-print'.
(defun bluetooth--list-entries ()
  "Provide the list entries for the tabulated view."
  (bluetooth--barf-if-uninitialized)
  (let (dev-list)
    (bluetooth-device-map
     (lambda (dev-id device)
       (when (bluetooth-device-properties device)
         (push (list dev-id
                     (cl-map 'vector
                             (lambda (key)
                               (let ((value (bluetooth-device-property device key)))
                                 (cond ((stringp value) value)
                                       ((null value) "no")
                                       (t "yes"))))
                             (mapcar #'cl-first bluetooth-list-format)))
               dev-list))))
    dev-list))

(defun bluetooth--print-list (&optional _device)
  "Print the device list."
  (when (get-buffer bluetooth-buffer-name)
    (with-current-buffer bluetooth-buffer-name
      (tabulated-list-print t)
      (and (fboundp 'hl-line-highlight)
           (bound-and-true-p hl-line-mode)
           (hl-line-highlight)))))

(defun bluetooth--update-with-callback ()
  (bluetooth--barf-if-uninitialized)
  (bluetooth-device-update-all #'bluetooth--print-list))

(defun bluetooth--update-print ()
  "Update device info and print device list view."
  (ignore-errors
    (bluetooth--update-with-callback)
    (bluetooth--print-list)))

;; Build up the index for Imenu.  This function is used as
;; `imenu-create-index-function'.
(defun bluetooth--create-imenu-index ()
  "Create the Bluetooth device index for Imenu."
  (goto-char (point-min))
  (cl-loop for (pos entry) = (list (point) (tabulated-list-get-entry))
           while entry
           do (forward-line 1)
           collect (cons (elt entry 0) pos)))

(defun bluetooth--initialize-mode-info ()
  "Get the current adapter state and display it.
This function only uses the first adapter reported by Bluez."
  (let* ((adapter (cl-first (bluetooth-lib-query-adapters)))
         (props (bluetooth-lib-adapter-properties adapter))
         (info (--map (list (cl-first it)
                            (list (cl-rest (assoc (cl-first it) props))))
                      bluetooth--mode-state)))
    (bluetooth--handle-prop-change (bluetooth-lib-interface :adapter)
                                   info)))

(defun bluetooth--cleanup ()
  "Clean up when mode buffer is killed."
  (when (dbus-unregister-object bluetooth--adapter-signal)
    (setq bluetooth--adapter-signal nil))
  (remove-hook 'dbus-event-error-functions #'bluetooth--show-error)
  (when bluetooth--update-timer
    (cancel-timer bluetooth--update-timer)
    (setq bluetooth--update-timer nil)))

(defun bluetooth--barf-if-uninitialized ()
  (unless bluetooth--initialized
    (user-error "Bluetooth is not initialized.  Try ‘bluetooth-list-devices’.")))

(defun bluetooth--mode-info ()
  "Update the mode info display."
  ;; This function is called from Emacs's mode-line update code
  ;; and must not contain any calls to D-Bus functions.
  (let ((info (mapconcat #'identity
                         (--keep (bluetooth-property-text (cl-rest it))
                                 bluetooth--mode-state)
                         ",")))
    (unless (string-blank-p info)
      (concat " [" info "]"))))

(defun bluetooth--handle-prop-change (interface data &rest _)
  "Handle property change signals on D-Bus INTERFACE as given by DATA.
Only adapter properties are considered.  If an adapter property changes,
update the status display accordingly."
  (when (string= interface (bluetooth-lib-interface :adapter))
    (mapc (lambda (elt)
            (cl-destructuring-bind (prop (value)) elt
              (when-let* ((property (cl-rest (assoc prop bluetooth--mode-state))))
                (setf (bluetooth-property-active-p property) value))
              (when (string= "Discovering" prop)
                (if value
                    (unless bluetooth--update-timer
                      (setf bluetooth--update-timer
                            (run-at-time nil bluetooth-update-interval
                                         #'bluetooth--update-print)))
                  (when bluetooth--update-timer
                    (cancel-timer bluetooth--update-timer)
                    (setq bluetooth--update-timer nil))))))
          data)
    (force-mode-line-update)))

(defun bluetooth-devatpt ()
  "Return the device at point."
  (when (and bluetooth--initialized
             (string= (buffer-name) bluetooth-buffer-name))
    (bluetooth-device (tabulated-list-get-id))))

(defun bluetooth--make-path (api)
  "Return the path of the currently selected device."
  (cond ((eq :device api)
         (bluetooth-device-path (bluetooth-devatpt)))
        ((eq :adapter api)
         (bluetooth-lib-path (cl-first (bluetooth-lib-query-adapters))))))

(defun bluetooth--show-error (&optional event error)
  "Display a D-Bus error, if one occurred."
  (when (and event error
             (string-search "org.bluez.Error"
                            (dbus-event-member-name event)))
    (message "Bluetooth error: %s (%s)"
             (nth 2 error)
             (nth 1 error))))


;;;; command definitions

(defun bluetooth--choose-uuid ()
  "Ask for a UUID and return it in a form suitable for ‘interactive’."
  (bluetooth--barf-if-uninitialized)
  (if current-prefix-arg
      (let* ((device (bluetooth-devatpt))
             (uuids (bluetooth-uuid-interpret
                     (bluetooth-device-property device "UUIDs")))
             (profile (completing-read "Profile: "
                                       (mapcar (lambda (x)
                                                 (let ((desc (cl-second x)))
                                                   (concat (cl-first desc)
                                                           ", "
                                                           (cl-second desc))))
                                               uuids)
                                       nil t)))
        (list (cl-rassoc profile uuids
                         :key (lambda (x)
                                (let ((desc (cl-first x)))
                                  (concat (cl-first desc) ", " (cl-second desc))))
                         :test #'string=)))
    '(nil)))

(defun bluetooth-connect (uuid)
  "Connect to the Bluetooth device at point.
When called with a prefix argument, ask for a profile and
connect only this profile.  Otherwise, or when called
non-interactively with UUID set to nil, connect to all profiles."
  (interactive (bluetooth--choose-uuid) bluetooth-mode)
  (let ((alias (bluetooth-device-property
                (bluetooth-devatpt)
                "Alias")))
    (if uuid
        (progn
          (bluetooth-lib-dbus-method (bluetooth--make-path :device) "ConnectProfile"
                                     :device #'bluetooth--show-error
                                     (cl-first uuid))
          (message "Attempting to connect profile %s of %s"
                   (cl-second (cl-second uuid)) alias))
      (bluetooth-lib-dbus-method (bluetooth--make-path :device) "Connect"
                                 :device #'bluetooth--show-error)
      (message "Attempting to connect to %s" alias))))

(defun bluetooth-disconnect (uuid)
  "Disconnect the Bluetooth device at point.
When called with a prefix argument, ask for a profile and
disconnect only this profile.  Otherwise, or when called
non-interactively with UUID set to nil, disconnect all
profiles."
  (interactive (bluetooth--choose-uuid) bluetooth-mode)
  (let ((alias (bluetooth-device-property
                (bluetooth-devatpt)
                "Alias")))
    (if uuid
        (progn
          (bluetooth-lib-dbus-method (bluetooth--make-path :device) "DisconnectProfile"
                                     :device #'bluetooth--show-error
                                     (cl-first uuid))
          (message "Disconnecting profile %s of %s"
                   (cl-second (cl-second uuid)) alias))
      (bluetooth-lib-dbus-method (bluetooth--make-path :device) "Disconnect"
                                 :device #'bluetooth--show-error)
      (message "Disconnecting %s" alias))))

(defun bluetooth-connect-profile ()
  "Ask for a Bluetooth profile and connect the device at point to it."
  (interactive nil bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (let ((prefix-arg (list 4)))
    (command-execute #'bluetooth-connect)))

(defun bluetooth-disconnect-profile ()
  "Ask for a Bluetooth profile and disconnect the device at point from it."
  (interactive nil bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (let ((prefix-arg (list 4)))
    (command-execute #'bluetooth-disconnect)))

(defmacro bluetooth-defun-method (method api docstring &rest body)
  "Make a function calling a Bluetooth METHOD using API.
The function will have DOCSTRING as its documentation and is
implemented by BODY."
  (declare (doc-string 3) (indent 2))
  (let ((name (bluetooth-lib-make-function-name method)))
    (cl-with-gensyms (gmethod gapi)
      `(defun ,(intern name) () ,docstring
              (interactive nil bluetooth-mode)
              (bluetooth--barf-if-uninitialized)
              (let ((,gmethod ,method)
                    (,gapi ,api))
                (condition-case err
                    (bluetooth-lib-dbus-method (bluetooth--make-path ,gapi)
                                               ,gmethod
                                               ,gapi
                                               #'bluetooth--show-error)
                  (dbus-error
                   (message "Bluetooth: %s" (nth 2 err))))
                ,@body)))))

(bluetooth-defun-method "StartDiscovery" :adapter
  "Start discovery mode."
  (message "Bluetooth discovery enabled"))

(bluetooth-defun-method "StopDiscovery" :adapter
  "Stop discovery mode."
  (message "Bluetooth discovery disabled"))

(bluetooth-defun-method "Pair" :device
  "Pair with device at point."
  (let ((alias (bluetooth-device-property
                (bluetooth-devatpt)
                "Alias")))
    (message "Attempting to pair with %s" alias)))

(defmacro bluetooth-defun-toggle (property api docstring &rest body)
  "Make a function to toggle a PROPERTY of a device using API.
The function will have DOCSTRING as its documentation."
  (declare (doc-string 3) (indent 2))
  (let ((name (bluetooth-lib-make-function-name property "-toggle")))
    (cl-with-gensyms (gproperty gapi)
      `(defun ,(intern name) () ,docstring
              (interactive nil bluetooth-mode)
              (bluetooth--barf-if-uninitialized)
              (let ((,gproperty ,property)
                    (,gapi ,api))
                (condition-case err
                    (bluetooth-lib-dbus-toggle (bluetooth--make-path ,gapi)
                                               ,gproperty
                                               ,gapi)
                  (dbus-error
                   (message "Bluetooth: %s" (nth 2 err))))
                ,@body)))))

(bluetooth-defun-toggle "Blocked" :device
  "Toggle the ‘blocked’ property of the Bluetooth device at point."
  (let ((blocked (bluetooth-lib-query-property "Blocked"
                                               (bluetooth--make-path :device)
                                               :device)))
    (message "Bluetooth device is %s" (if blocked "blocked" "not blocked"))))

(bluetooth-defun-toggle "Trusted" :device
  "Toggle the ‘trusted’ property of the Bluetooth device at point."
  (let ((trusted (bluetooth-lib-query-property "Trusted"
                                               (bluetooth--make-path :device)
                                               :device)))
    (message "Bluetooth device is %s" (if trusted "trusted" "not trusted"))))
(bluetooth-defun-toggle "Powered" :adapter
  "Toggle the power supply of the Bluetooth adapter."
  (let ((powered (bluetooth-lib-query-property "Powered"
                                               (bluetooth--make-path :adapter)
                                               :adapter)))
    (message "Bluetooth power supply %s" (if powered "enabled" "disabled"))))
(bluetooth-defun-toggle "Discoverable" :adapter
  "Toggle discoverable mode."
  (let ((discoverable (bluetooth-lib-query-property "Discoverable"
                                                    (bluetooth--make-path :adapter)
                                                    :adapter)))
    (message "Bluetooth host is %s" (if discoverable
                                        "discoverable"
                                      "not discoverable"))))
(bluetooth-defun-toggle "Pairable" :adapter
  "Toggle pairable mode."
  (let ((pairable (bluetooth-lib-query-property "Pairable"
                                                (bluetooth--make-path :adapter)
                                                :adapter)))
    (message "Bluetooth host is %s" (if pairable
                                        "pairable"
                                      "not pairable"))))
(defun bluetooth-set-alias (name)
  "Set the alias of the Bluetooth device at point to NAME."
  (interactive "MAlias (empty to reset): " bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (bluetooth-lib-dbus-set (bluetooth--make-path :device) "Alias" name :device))

(defun bluetooth-remove-device (&optional dev-id)
  "Remove the Bluetooth device at point or specified by DEV-ID.
Calling this function will unpair device and host."
  (interactive nil bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (when-let* ((device (bluetooth-device (or dev-id (tabulated-list-get-id)))))
    (message "Removing and unpairing %s"
             (bluetooth-device-property device "Alias"))
    (bluetooth-lib-dbus-method (bluetooth-device-property device "Adapter")
                               "RemoveDevice"
                               :adapter
                               #'bluetooth--show-error
                               :object-path
                               (bluetooth-device-path device))
    (run-with-timer bluetooth-update-interval nil #'bluetooth--update-print)))

(defun bluetooth-end-of-list ()
  "Move point to the last list element."
  (interactive nil bluetooth-mode)
  (let ((column (current-column)))
    (goto-char (point-max))
    (forward-line -1)
    (goto-char (+ (point)
                  (- column (current-column))))))

(defun bluetooth-beginning-of-list ()
  "Move point to the first list element."
  (interactive nil bluetooth-mode)
  (let ((column (current-column)))
    (goto-char (point-min))
    (goto-char (+ (point)
                  (- column (current-column))))))

(define-minor-mode bluetooth-info-follow-mode
  "Bluetooth info follow minor mode.
If enabled, the device info display follows the selected device entry."
  :group 'bluetooth :init-value nil :lighter " Fol"
  (if (not bluetooth-info-follow-mode)
      (remove-hook 'post-command-hook #'bluetooth-info-follow-mode-hook t)
    (add-hook 'post-command-hook #'bluetooth-info-follow-mode-hook nil t)
    (make-local-variable 'bluetooth--follow-last-id)))

(defun bluetooth-info-follow-mode-hook ()
  (unless (equal bluetooth--follow-last-id (tabulated-list-get-id))
    (setf bluetooth--follow-last-id (tabulated-list-get-id))
    (condition-case nil
        (bluetooth-show-device-info)
      (error t))))


;;;; keymap and menu
(declare-function bluetooth-menu "bluetooth.el")
(defvar bluetooth-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map [?c] #'bluetooth-connect)
    (define-key map [?d] #'bluetooth-disconnect)
    (define-key map [?b] #'bluetooth-toggle-blocked)
    (define-key map [?t] #'bluetooth-toggle-trusted)
    (define-key map [?a] #'bluetooth-set-alias)
    (define-key map [?r] #'bluetooth-start-discovery)
    (define-key map [?R] #'bluetooth-stop-discovery)
    (define-key map [?s] #'bluetooth-toggle-powered)
    (define-key map [?Q] #'bluetooth-shutdown)
    (define-key map [?P] #'bluetooth-pair)
    (define-key map [?D] #'bluetooth-toggle-discoverable)
    (define-key map [?x] #'bluetooth-toggle-pairable)
    (define-key map [?i] #'bluetooth-show-device-info)
    (define-key map [?A] #'bluetooth-show-adapter-info)
    (define-key map [?k] #'bluetooth-remove-device)
    (define-key map [?<] #'bluetooth-beginning-of-list)
    (define-key map [?>] #'bluetooth-end-of-list)
    (define-key map [?F] #'bluetooth-info-follow-mode)
    (define-key map [?M] #'bluetooth-menu)

    (define-key map [menu-bar bluetooth]
                (cons "Bluetooth" (make-sparse-keymap "Bluetooth")))
    (define-key map [menu-bar bluetooth shutdown]
                '(menu-item "Shutdown" bluetooth-shutdown
                            :help "Shutdown bluetooth mode"))
    (define-key map [menu-bar bluetooth follow]
                '(menu-item "Info follow mode" bluetooth-info-follow-mode
                            :button (:toggle
                                     . (and (boundp 'bluetooth-info-follow-mode)
                                            bluetooth-info-follow-mode))
                            :help "Toggle device info follow mode"))


    (define-key map [menu-bar bluetooth device]
                (cons "Device" (make-sparse-keymap "Device")))
    (define-key map [menu-bar bluetooth stop-discovery]
                '(menu-item "Stop discovery" bluetooth-stop-discovery
                            :help "Stop discovery"))
    (define-key map [menu-bar bluetooth start-discovery]
                '(menu-item "Start discovery" bluetooth-start-discovery
                            :help "Start discovery"))
    (define-key map [menu-bar bluetooth toggle-discoverable]
                '(menu-item "Discoverable"
                            bluetooth-toggle-discoverable
                            :button (:toggle
                                     . (bluetooth-lib-adapter-property
                                        (cl-first
                                         (bluetooth-lib-query-adapters))
                                        "Discoverable"))
                            :help "Toggle discoverable mode"))
    (define-key map [menu-bar bluetooth toggle-pairable]
                '(menu-item "Pairable" bluetooth-toggle-pairable
                            :button (:toggle
                                     . (bluetooth-lib-adapter-property
                                        (cl-first
                                         (bluetooth-lib-query-adapters))
                                        "Pairable"))
                            :help "Toggle pairable mode"))
    (define-key map [menu-bar bluetooth toggle-powered]
                '(menu-item "Powered" bluetooth-toggle-powered
                            :button (:toggle
                                     . (bluetooth-lib-adapter-property
                                        (cl-first
                                         (bluetooth-lib-query-adapters))
                                        "Powered"))
                            :help "Toggle power supply of adapter"))
    (define-key map [menu-bar bluetooth show-adapter-info]
                '(menu-item "Show adapter info" bluetooth-show-adapter-info
                            :help "Show bluetooth adapter info"))

    (define-key map [menu-bar bluetooth device show-info]
                '(menu-item "Show device info" bluetooth-show-device-info
                            :help "Show bluetooth device info"))
    (define-key map [menu-bar bluetooth device set-alias]
                '(menu-item "Set device alias" bluetooth-set-alias
                            :help "Set device alias"))
    (define-key map [menu-bar bluetooth device toggle-trusted]
                '(menu-item "Toggle trusted" bluetooth-toggle-trusted
                            :help "Trust/untrust bluetooth device"))
    (define-key map [menu-bar bluetooth device toggle-blocked]
                '(menu-item "Toggle blocked" bluetooth-toggle-blocked
                            :help "Block/unblock bluetooth device"))
    (define-key map [menu-bar bluetooth device disconnect-profile]
                '(menu-item "Disconnect profile" bluetooth-disconnect-profile
                            :help "Disconnect bluetooth device profile"))
    (define-key map [menu-bar bluetooth device disconnect]
                '(menu-item "Disconnect" bluetooth-disconnect
                            :help "Disconnect bluetooth device"))
    (define-key map [menu-bar bluetooth device connect-profile]
                '(menu-item "Connect profile" bluetooth-connect-profile
                            :help "Connect bluetooth device profile"))
    (define-key map [menu-bar bluetooth device connect]
                '(menu-item "Connect" bluetooth-connect
                            :help "Connect bluetooth device"))
    (define-key map [menu-bar bluetooth device remove]
                '(menu-item "Remove" bluetooth-remove-device
                            :help "Remove bluetooth device"))
    (define-key map [menu-bar bluetooth device pair]
                '(menu-item "Pair" bluetooth-pair
                            :help "Pair bluetooth device"))

    map)
  "The Bluetooth mode keymap.")


;;;; mode definition

(define-derived-mode bluetooth-mode tabulated-list-mode
  bluetooth--mode-name
  "Major mode for managing Bluetooth devices."
  (setq tabulated-list-format
        (cl-map 'vector (lambda (x) (append x '(t))) bluetooth-list-format)
        tabulated-list-entries #'bluetooth--list-entries
        tabulated-list-padding 0
        tabulated-list-sort-key (cons "Alias" nil))
  (add-hook 'tabulated-list-revert-hook #'bluetooth--update-with-callback
            nil 'local)
  (tabulated-list-init-header)
  (tabulated-list-print)
  (hl-line-mode))


;;;; device and adapter info display

(defun bluetooth--ins-heading (heading)
  "Insert HEADING in info view."
  (insert (propertize heading 'face
                      'bluetooth-info-heading)))

(defun bluetooth-ins-line (attr text)
  "Insert attribute ATTR and corresponding TEXT in info view."
  (insert (propertize (format "%21s" attr)
                      'face
                      'bluetooth-info-attribute)
          ": " text "\n"))

(defun bluetooth--ins-attr (props attr)
  "Insert information on attribute ATTR in properties alist PROPS."
  (let ((value (cl-rest (assoc attr props))))
    (bluetooth-ins-line attr
                        (cond ((stringp value) value)
                              ((numberp value)
                               (number-to-string value))
                              ((consp value)
                               (mapconcat #'identity value ", "))
                              ((null value) "no")
                              (t "yes")))))

(defun bluetooth--ins-classes (props)
  "Insert device classes from properties alist PROPS."
  (when-let* ((class (cl-rest (assoc "Class" props))))
    (let ((p-class (bluetooth-uuid-parse-class class)))
      (bluetooth--ins-heading "\nService and device classes\n")
      (--map (cl-destructuring-bind (type value) it
               (if (listp value)
                   (bluetooth-ins-line type
                                       (mapconcat #'identity
                                                  value
                                                  ", "))
                 (bluetooth-ins-line type value)))
             p-class))))

(defun bluetooth--ins-services (props)
  "Insert device services from properties alist PROPS."
  (when-let* ((uuids (cl-rest (assoc "UUIDs" props))))
    (bluetooth--ins-heading "\nServices (UUIDs)\n")
    (mapc (lambda (id-pair)
            (cl-mapc (lambda (fmt pair)
                       (insert (format fmt pair)))
                     '("%36s  " "%s " "(%s)")
                     (cl-second id-pair))
            (insert "\n"))
          (bluetooth-uuid-interpret uuids))))

(defun bluetooth--ins-rf-info (props)
  "Insert RF information from properties alist PROPS."
  (let* ((rssi (cl-rest (assoc "RSSI" props)))
         (tx-power (cl-rest (assoc "TxPower" props)))
         (loss (when (and rssi tx-power) (- tx-power rssi))))
    (--zip-with (when other
                  (bluetooth-ins-line (cl-first it)
                                      (format (cl-second it) other)))
                '(("RSSI" "%4d dBm") ("Tx Power" "%4d dBm")
                  ("Path loss" "%4d dB"))
                (list rssi tx-power loss))))

(defun bluetooth--ins-mfc-info (props)
  "Insert manufacturer information from properties alist PROPS."
  (when-let* ((mf-info (cl-second (assoc "ManufacturerData" props))))
    (bluetooth-ins-line "Manufacturer"
                        (or (bluetooth-uuid-manufacturer-from-id
                             (cl-first mf-info))
                            "unknown"))))

(defun bluetooth-show-device-info ()
  "Show detailed information on the device at point."
  (interactive nil bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (when-let* ((device (bluetooth-devatpt)))
    (with-current-buffer-window bluetooth-info-buffer-name nil nil
      (let ((props (bluetooth-device-properties device)))
        (bluetooth--ins-heading "Bluetooth device info\n\n")
        (mapc (lambda (it) (bluetooth--ins-attr props it))
              '("Alias" "Address" "AddressType" "Paired" "Bonded"
                "Trusted" "Blocked" "LegacyPairing" "Connected"
                "Modalias" "ServicesResolved" "WakeAllowed"
                "Adapter"))
        (funcall (-juxt #'bluetooth--ins-rf-info
                        #'bluetooth--ins-mfc-info
                        #'bluetooth--ins-classes
                        #'bluetooth--ins-services)
                 props)
        (bluetooth--ins-heading "\nOther device information\n")
        (bluetooth-plugin-insert-infos device)
        (special-mode)))))

(defun bluetooth-show-adapter-info ()
  "Show detailed information on the (first) Bluetooth adapter."
  (interactive nil bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (let* ((adapter (cl-first (bluetooth-lib-query-adapters)))
         (props (bluetooth-lib-adapter-properties adapter)))
    (with-current-buffer-window bluetooth-info-buffer-name nil nil
      (bluetooth--ins-heading "Bluetooth adapter info\n\n")
      (mapc (lambda (it) (bluetooth--ins-attr props it))
            '("Alias" "Address" "AddressType" "Powered" "Discoverable"
              "DiscoverableTimeout" "Pairable" "PairableTimeout"
              "Discovering" "Roles" "Modalias"))
      (bluetooth-ins-line "Adapter" (bluetooth-lib-path adapter))
      (funcall (-juxt #'bluetooth--ins-mfc-info
                      #'bluetooth--ins-classes
                      #'bluetooth--ins-services)
               props)
      (special-mode))))


;;;; mode entry command

;;;###autoload
(defun bluetooth-init ()
  "Initialize Bluetooth mode."
  ;; make sure D-Bus is (made) available
  (unless (dbus-ping bluetooth-bluez-bus bluetooth-service bluetooth-timeout)
    (error "The bluetooth service “%s” is not available" bluetooth-service))
  (add-hook 'dbus-event-error-functions #'bluetooth--show-error)
  (bluetooth-pa-register-agent)
  ;; FIXME This is done here to avoid multiple plugin menu entries after
  ;; re-initialisation.  Is there a better way?
  (transient-define-prefix bluetooth-menu ()
    "Bluetooth mode menu."
    ["Bluetooth menu\n"
     ["Device"
      ("c" "connect" bluetooth-connect)
      ("d" "disconnect" bluetooth-disconnect)
      ("b" "toggle blocked" bluetooth-toggle-blocked)
      ("t" "toggle trusted" bluetooth-toggle-trusted)
      ("a" "set alias" bluetooth-set-alias)
      ("P" "pair" bluetooth-pair)
      ("k" "remove device" bluetooth-remove-device)
      ("i" "show device information" bluetooth-show-device-info)]
     ["Adapter"
      ("r" "start discovery" bluetooth-start-discovery)
      ("R" "stop discovery" bluetooth-stop-discovery)
      ("s" "toggle power supply" bluetooth-toggle-powered)
      ("D" "toggle discoverable" bluetooth-toggle-discoverable)
      ("x" "toggle pairable" bluetooth-toggle-pairable)
      ("A" "show adapter information" bluetooth-show-adapter-info)]])
  (bluetooth-plugin-init 'bluetooth-menu)
  (bluetooth-device-init #'bluetooth--print-list)
  (setf bluetooth--initialized t))

;;;###autoload
(defun bluetooth-list-devices ()
  "Display a list of Bluetooth devices.
This function starts Bluetooth mode which offers an interface
offering device management functions, e.g. pairing, connecting,
scanning the bus, displaying device info etc."
  ;; four cases:
  ;; 1. initialized and bluetooth-mode
  ;;    - do nothing
  ;; 2. not initialized and bluetooth-mode
  ;;    - initialize
  ;;    - build buffer
  ;; 3. not initialized and not bluetooth-mode
  ;;    - initialize
  ;;    - erase buffer
  ;;    - build buffer
  ;; 4. initialized and not bluetooth-mode
  ;;    - erase buffer
  ;;    - build buffer
  (interactive)
  (unless bluetooth--initialized
    (bluetooth-init))
  (with-current-buffer (switch-to-buffer bluetooth-buffer-name)
    (unless (derived-mode-p 'bluetooth-mode)
      (erase-buffer))
    (bluetooth-mode)
    (cl-pushnew bluetooth--mode-info mode-line-process)
    (setq imenu-create-index-function #'bluetooth--create-imenu-index)
    (bluetooth--initialize-mode-info)
    (setq bluetooth--adapter-signal
          (bluetooth-lib-register-props-signal nil
                                               (bluetooth-lib-path
                                                (cl-first (bluetooth-lib-query-adapters)))
                                               #'bluetooth--handle-prop-change
                                               :arg-namespace
                                               (bluetooth-lib-interface :adapter)))))

(defun bluetooth-shutdown (&optional arg)
  "Shutdown Bluetooth mode.
This command will unregister any agents and plugins and free
D-Bus resources.  If called interactively, it will ask for
confirmation before shutting down."
  (interactive "p" bluetooth-mode)
  (bluetooth--barf-if-uninitialized)
  (let ((do-shutdown (if (and arg
                              (not (y-or-n-p "Shutdown Bluetooth mode?")))
                         nil
                       t)))
    (when do-shutdown
      (unwind-protect
          (progn (bluetooth-device-cleanup)
                 (bluetooth-plugin-unregister-all)
                 (bluetooth-pa-unregister-agent)
                 (bluetooth--cleanup))
        (setf bluetooth--initialized nil)))))

(provide 'bluetooth)

;;; bluetooth.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
