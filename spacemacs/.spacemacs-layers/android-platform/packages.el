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
  '((device-control
     :location (recipe
                :fetcher github
                :repo jeremy-compostella/device-control))
     (soong-mode
      :location (recipe
                 :fetcher github
                 :repo makohoek/emacs-soong-mode))))

(defun android-platform/init-device-control ()
  "Flash via fastboot or use adb to interact with the device."
  (use-package device-control
    :commands device-control
    :config
    (use-package dctrl-android)))

(defun android-platform/init-soong-mode()
  (use-package soong-mode))

;;; packages.el ends here
