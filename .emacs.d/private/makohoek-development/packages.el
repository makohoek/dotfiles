;;; packages.el --- makohoek-development layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Mattijs Korpershoek
;;
;; Author:  <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/Makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `makohoek-development-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `makohoek-development/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `makohoek-development/pre-init-PACKAGE' and/or
;;   `makohoek-development/post-init-PACKAGE' to customize the package as it is loaded.


(defconst makohoek-development-packages
  '(dtrt-indent
    ag
    projectile)
  "The list of Lisp packages required by the makohoek-development layer."
  )

(defun makohoek-development/init-dtrt-indent ()
  ;; enable dtrt-indent for c development
  (add-hook 'c-mode-common-hook
            (lambda() (require 'dtrt-indent)
              (dtrt-indent-mode t))))

(defun makohoek-development/init-ag ()
  ;; nothing to configure here
  )

(defun makohoek-development/post-init-projectile ()
    ;; do not run find-file after a project switch
    (setq projectile-switch-project-action 'projectile-dired)

    ;; specific per-project compile commands
    (defun my-switch-project-hook ()
      "Perform some action after switching Projectile projects."
      (message "Switched to project: %s" (projectile-project-name))

      (cond
       ((string= (projectile-project-name) "nbl-android")
        (setq projectile-project-compilation-cmd "./gradlew :wear:assembleDebug")
        (setq projectile-project-test-cmd "cd wear/build/outputs/apk && make")
        )
       ((string= (projectile-project-name) "audio-hal")
        (setq projectile-project-compilation-cmd
              "/bin/bash -c 'cd /build/mkorperx/ndg-android/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               m audio.primary.merrifield'")
        (setq projectile-project-test-cmd
              "ssh mako@acers5.tl.intel.com \
               'cd /home/mako/tools/install-ndg-android && make audiohal'")
        )
       ((string= (projectile-project-name) "robby")
        (setq projectile-project-compilation-cmd
              "/bin/bash -c 'cd /build/mkorperx/ndg-android/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               cd device/intel/robby/audio && mm'")
        (setq projectile-project-test-cmd
              "ssh mako@acers5.tl.intel.com \
               'cd /home/mako/tools/install-ndg-android && make pfw'")
        )
       ((string= (projectile-project-name) "kernel")
        (setq projectile-project-compilation-cmd
              "/bin/bash -c 'cd /build/mkorperx/ndg-android/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               mbimg -j32'")
        (setq projectile-project-test-cmd
              "ssh mako@acers5.tl.intel.com \
               'cd /home/mako/tools/install-ndg-android && make kernel'")
        )
       )
      )
    (add-hook 'projectile-after-switch-project-hook #'my-switch-project-hook)
  )

;;; packages.el ends here
