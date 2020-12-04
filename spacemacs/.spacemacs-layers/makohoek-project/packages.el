;;; packages.el --- makohoek-project layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; Briefly, each package to be installed or configured by this layer should be
;; added to `makohoek-project-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `makohoek-project/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `makohoek-project/pre-init-PACKAGE' and/or
;;   `makohoek-project/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(defconst makohoek-project-packages
  '(counsel-projectile
    projectile))

;; projectile is owned by 'spacemacs-project'
(defun makohoek-project/post-init-projectile ()
  ;; always run dired after switching project
  ;; this is for performance reasons:
  ;; projectile-find-file is a bit slow over/tramp in the linux kernel directory
  (use-package projectile
    :defer t
    :config
    (setq projectile-switch-project-action #'projectile-dired)
    (projectile-register-project-type
     'linux-kernel
     '(".projectile-type-linux")
     :compilation-dir "../"
     :compile 'makohoek-project/android/compile-kernel
     :test 'makohoek-project/android/flash-kernel)
    (projectile-register-project-type
     'android-system
     '("art")
     :compile 'makohoek-project/android/compile-system)
    (projectile-register-project-type
     'android-vendor
     '(".projectile-type-android-vendor")
     :compilation-dir "../../../.."
     :compile 'makohoek-project/android/compile-vendor)
    (projectile-register-project-type
     'u-boot
     '(".projectile-type-u-boot")
     :compile 'makohoek-project/uboot/compile)
    :custom
    (projectile-git-command "fd . -0" "faster indexing")
    (projectile-git-submodule-command nil "disable submodule indexing")
  ))

;; counsel-projectile is owned by 'spacemacs-layouts'
(defun makohoek-project/post-init-counsel-projectile ()
  ;; always run dired after switching project
  ;; this is for performance reasons:
  ;; projectile-find-file is a bit slow over/tramp in the linux kernel directory
  (use-package counsel-projectile
    :defer t
    :config
    (counsel-projectile-modify-action
     'counsel-projectile-switch-project-action
     '((default counsel-projectile-switch-project-action-dired)))))

;;; packages.el ends here
