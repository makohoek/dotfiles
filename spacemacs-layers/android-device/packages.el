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
    :config
    (progn
      (require 'lt-logcat)
      (require 'lt-serial)
      (require 'lt-serial-kernel)
      (customize-set-variable 'lt-serial-default-port "ttyUSB1"))))

(defun android-device/init-device-control ()
  (use-package device-control)
  :config
  (use-package dctrl-android))

;;; packages.el ends here
