;;; packages.el --- android-platform layer packages file for Spacemacs.
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

;; android-platform is a layer to control android based devices.
;; It is intended for platform/aosp developers that want to interact
;; with their development boards from within Emacs.

;;; Code:

(defconst android-platform-packages
  '((log-tools
     :location (recipe
                :fetcher github
                :repo jeremy-compostella/log-tools))
    (device-control
     :location (recipe
                :fetcher github
                :repo jeremy-compostella/device-control))))

(defun android-platform/init-log-tools ()
  "Log tools allow adb logcat and serial logs via UART."
  (use-package log-tools
    :commands log-tools
    :config
    (progn
      (use-package lt-logcat)
      (use-package lt-serial
        :custom
        (lt-serial-default-port "ttyUSB0" "default serial port is /dev/ttyUSB0")
        (lt-serial-default-speed 921600 "default serial baud rate is 921600"))
      (use-package lt-serial-kernel))))

(defun android-platform/init-device-control ()
  "Flash via fastboot or use adb to interact with the device."
  (use-package device-control
    :commands device-control
    :config
    (use-package dctrl-android)))

;;; packages.el ends here
