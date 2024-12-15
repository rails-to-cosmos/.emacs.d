;;; bluetooth-lib.el --- Library functions for Emacs Bluetooth mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Free Software Foundation, Inc.

;; Author: Raffael Stocker <r.stocker@mnet-mail.de>
;; Keywords: hardware
;; URL: https://codeberg.org/rstocker/emacs-bluetooth

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file collects utility functions for use in the bluetooth package or
;; plugins.

;;; Code:

(require 'dbus)

(defcustom bluetooth-bluez-bus :system
  "D-Bus bus that Bluez is registered on.
This is usually `:system' if bluetoothd runs as a system service, or
`:session' if it runs as a user service."
  :type '(symbol)
  :group 'bluetooth)

(defcustom bluetooth-timeout 5000
  "Default timeout for Bluez D-Bus access."
  :type '(natnum)
  :group 'bluetooth)

;; Bluez service name as defined by the Bluez API
(defcustom bluetooth-service "org.bluez"
  "D-Bus service name of Bluez."
  :type '(string)
  :group 'bluetooth)

;; Bluez root path as defined by the Bluez API
(defcustom bluetooth-root "/org/bluez"
  "D-Bus path root for Bluez."
  :type '(string)
  :group 'bluetooth)

(eval-and-compile
  (defun bluetooth-lib-make-function-name (name &optional prefix)
    "Make a function name out of NAME and PREFIX.
The generated function name has the form ‘bluetoothPREFIX-NAME’."
    (let ((case-fold-search nil))
      (concat "bluetooth"
              prefix
              (replace-regexp-in-string "[[:upper:]][[:lower:]]+"
                                        (lambda (x) (concat "-" (downcase x)))
                                        name t)))))

(defconst bluetooth-lib--apis
  '(
    :adapter "org.bluez.Adapter1"
    :advert-monitor "org.bluez.AdvertisementMonitor1"
    :advert-monitor-manager "org.bluez.AdvertisementMonitorManager1"
    :agent "org.bluez.Agent1"
    :agent-manager "org.bluez.AgentManager1"
    :battery "org.bluez.Battery1"
    :battery-provider "org.bluez.BatteryProvider1"
    :battery-provider-manager "org.bluez.BatteryProviderManager1"
    :device "org.bluez.Device1"
    :device-set "org.bluez.DeviceSet1"
    :gatt-characteristic "org.bluez.GattCharacteristic1"
    :gatt-descriptor "org.bluez.GattDescriptor1"
    :gatt-manager "org.bluez.GattManager1"
    :gatt-profile "org.bluez.GattProfile1"
    :gatt-service "org.bluez.GattService1"
    :health-channel "org.bluez.HealthChannel1"
    :health-device "org.bluez.HealthDevice1"
    :health-manager "org.bluez.HealthManager1"
    :input "org.bluez.Input1"
    :le-advert "org.bluez.LEAdvertisement1"
    :le-advert-manager "org.bluez.LEAdvertisingManager1"
    :media "org.bluez.Media1"
    :media-control "org.bluez.MediaControl1"
    :media-endpoint "org.bluez.MediaEndpoint1"
    :media-folder "org.bluez.MediaFolder1"
    :media-item "org.bluez.MediaItem1"
    :media-player "org.bluez.MediaPlayer1"
    :media-transport "org.bluez.MediaTransport1"
    :network "org.bluez.Network1"
    :network-server "org.bluez.NetworkServer1"
    :profile "org.bluez.Profile1"
    :profile-manager "org.bluez.ProfileManager1"
    :properties "org.freedesktop.DBus.Properties"
    :sim-access "org.bluez.SimAccess1"
    :thermometer "org.bluez.Thermometer1"
    :thermometer-manager "org.bluez.ThermometerManager1"
    :thermometer-watcher "org.bluez.ThermometerWatcher1")
  "Bluez D-Bus APIs and their interface names.")

(defun bluetooth-lib-interface (api)
  "Return Bluez interface name for API."
  (plist-get bluetooth-lib--apis api))

(defun bluetooth-lib-path (&rest nodes)
  "Construct path from NODES, prefixed by ‘bluetooth-root’."
  (mapconcat #'identity (cons bluetooth-root nodes) "/"))

(defun bluetooth-lib-query-adapters ()
  "Return a list of Bluetooth adapters."
  (dbus-introspect-get-node-names
   bluetooth-bluez-bus bluetooth-service bluetooth-root))

(defun bluetooth-lib-query-devices (adapter)
  "Return a list of Bluetooth devices connected to ADAPTER."
  (dbus-introspect-get-node-names bluetooth-bluez-bus bluetooth-service
                                  (bluetooth-lib-path adapter)))

(defun bluetooth-lib-query-properties (path api)
  "Return all properties of object on PATH using API."
  (dbus-get-all-properties bluetooth-bluez-bus bluetooth-service
                           path
                           (bluetooth-lib-interface api)))

(defun bluetooth-lib-query-property (property path api)
  "Return the value of PROPERTY of object on PATH using API."
  (let ((props (bluetooth-lib-query-properties path api)))
    (alist-get property props nil nil #'equal)))

(defun bluetooth-lib-adapter-properties (adapter)
  "Return the properties of Bluetooth ADAPTER.
This function evaluates to an alist of attribute/value pairs."
  (bluetooth-lib-query-properties (bluetooth-lib-path adapter) :adapter))

(defun bluetooth-lib-adapter-property (adapter property)
  "For the ADAPTER, return the value of PROPERTY."
  (cl-rest (assoc property (bluetooth-lib-adapter-properties adapter))))

(defun bluetooth-lib--call-method (path api method &rest args)
  "For PATH and using API, invoke the D-Bus METHOD, passing ARGS."
  (let ((interface (bluetooth-lib-interface api)))
    (when path
      (apply method bluetooth-bluez-bus bluetooth-service path interface args))))

(defun bluetooth-lib-dbus-method (path method api &optional handler &rest args)
  "For PATH, invoke METHOD on D-Bus API, passing ARGS.
The method is called asynchronously with the timeout specified by
‘bluetooth-timeout’."
  (apply #'bluetooth-lib--call-method path api
         #'dbus-call-method-asynchronously method
         handler :timeout bluetooth-timeout args))

(defun bluetooth-lib-dbus-sync-method (path method api &rest args)
  "For PATH, invoke METHOD on D-Bus API, passing ARGS.
The method is called synchronously."
  (apply #'bluetooth-lib--call-method path api
         #'dbus-call-method method
         args))

(defun bluetooth-lib-dbus-toggle (path property api)
  "For the D-Bus object PATH, toggle boolean PROPERTY on D-Bus API."
  (let ((value (bluetooth-lib--call-method path api
                                           #'dbus-get-property property)))
    (bluetooth-lib--call-method path api #'dbus-set-property property
                                (not value))))

(defun bluetooth-lib-dbus-set (path property arg api)
  "For the D-Bus object PATH, set PROPERTY to ARG on D-Bus API."
  (bluetooth-lib--call-method path api #'dbus-set-property property arg))

(defun bluetooth-lib-register-props-signal (service path handler-fn &rest args)
  "Register a signal handler to be notified when properties change.
The argument SERVICE specifies the bluetooth service,
e.g. ‘bluetooth-service’, or is nil for adapter signals.  The
argument PATH specifies the path to the D-Bus object holding the
property.

HANDLER-FN is signal handler function taking three arguments:
 INTERFACE-NAME: name of the interface with property changes
 CHANGED-PROPERTIES: an alist of changed properties
 INVALIDATED-PROPERTIES: an alist of changed properties without
 values.

ARGS can contain further arguments passed to
‘dbus-register-signal’.

The return value is a D-Bus object that should be unregistered
with ‘dbus-unregister-object’ when not needed anymore."
  (apply #'dbus-register-signal bluetooth-bluez-bus
         service path (bluetooth-lib-interface :properties)
         "PropertiesChanged" handler-fn args))

(defun bluetooth-lib-get-objects (path)
  "Get all Bluez objects on PATH."
  (dbus-get-all-managed-objects bluetooth-bluez-bus bluetooth-service path))

(defun bluetooth-lib-match (objects key &optional value)
  "Find all Bluez OBJECTS matching KEY and VALUE.
The argument OBJECTS must be a list of Bluez objects as obtained
by a call to ‘bluetooth-lib-get-objects’.  This function returns
a list of objects that contain KEY, a regexp, optionally filtered
for VALUE, a regexp or something that can be compared with ‘eql’.

The first element of every object in the list is a path to that
object.  It can be used for further analysis using, e. g.,
‘bluetooth-lib-query-properties’."
  (cl-labels ((matches (obj what)
                (let ((case-fold-search nil))
                  (cond ((and (stringp obj)
                              (stringp what))
                         (string-match-p what obj))
                        ((atom obj) (eql obj what))
                        ((and (consp obj)
                              (atom (car obj))
                              (atom (cdr obj)))
                         (and (matches (car obj) key)
                              (if value
                                  (matches (cdr obj) value)
                                t)))
                        ((consp obj)
                         (or (matches (car obj) key)
                             (matches (cdr obj) key)))
                        (t nil)))))
    (let (result)
      (dolist (obj objects)
        (when (matches obj key)
          (push obj result)))
      result)))

(provide 'bluetooth-lib)
;;; bluetooth-lib.el ends here

;; Local Variables:
;; indent-tabs-mode: nil
;; tab-width: 4
;; End:
