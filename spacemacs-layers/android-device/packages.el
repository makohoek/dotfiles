;;; packages.el --- android-device layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; Commentary:

;; android-device is a layer to control android based devices.
;; It is intended for platform/aosp developers that want to interact
;; with their development boards from within Emacs.

;;; Code:

(defconst android-device-packages
  '((log-tools
     :location (recipe
                :fetcher github
                :repo jeremy-compostella/log-tools))
    (device-control
     :location (recipe
                :fetcher github
                :repo jeremy-compostella/device-control))))

(defun android-device/init-log-tools ()
  "Log tools allow adb logcat and serial logs via UART."
  (use-package log-tools
    :commands log-tools
    :config
    (progn
      (use-package lt-logcat)
      (use-package lt-serial
        :custom
        (lt-serial-default-port "ttyUSB1" "default serial port is /dev/ttyUSB1"))
      (use-package lt-serial-kernel))))

(defun android-device/init-device-control ()
  "Flash via fastboot or use adb to interact with the device."
  (use-package device-control
    :commands device-control
    :config
    (use-package dctrl-android
      :defer t)))

;;; packages.el ends here
