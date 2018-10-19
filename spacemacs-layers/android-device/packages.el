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

;;; Code:

(defconst android-device-packages
  '((log-tools :location (recipe :fetcher github :repo jeremy-compostella/log-tools))
    (device-control :location (recipe :fetcher github :repo jeremy-compostella/device-control))))

(defun android-device/init-log-tools ()
  (use-package log-tools
    :commands log-tools
    :config
    (progn
      (use-package lt-logcat)
      (use-package lt-serial
        :config
        (customize-set-variable 'lt-serial-default-port "ttyUSB1"))
      (use-package lt-serial-kernel))))

(defun android-device/init-device-control ()
  (use-package device-control
  :commands device-control
  :config
  (use-package dctrl-android)))

;;; packages.el ends here
