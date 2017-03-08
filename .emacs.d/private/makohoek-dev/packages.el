;;; packages.el --- makohoek-dev layer packages file for Spacemacs.
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
;; added to `makohoek-dev-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `makohoek-dev/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `makohoek-dev/pre-init-PACKAGE' and/or
;;   `makohoek-dev/post-init-PACKAGE' to customize the package as it is loaded.


(defconst makohoek-dev-packages '(dtrt-indent ag ediff projectile magit whitespace)
  "The list of Lisp packages required by the makohoek-dev layer.")

(defun makohoek-dev/init-dtrt-indent ()
  ;; enable dtrt-indent for c development
  (add-hook 'c-mode-common-hook
            (lambda ()
              (require 'dtrt-indent)
              (dtrt-indent-mode t))))

(defun makohoek-dev/init-ag ()
  ;; nothing to configure here
  )

(defun makohoek-dev/post-init-projectile ()
  ;; do not run find-file after a project switch
  (setq projectile-switch-project-action 'projectile-dired)
  ;; specific per-project compile commands
  (defun my-switch-project-hook ()
    "Perform some action after switching Projectile projects."
    (message "Switched to project: %s"
             (projectile-project-name))
    (cond
     ((string= (projectile-project-name)
               "nbl-android")
      (setq projectile-project-compilation-cmd "./gradlew :wear:assembleDebug")
      (setq projectile-project-test-cmd "cd wear/build/outputs/apk && make"))
     ((string= (projectile-project-name)
               "audio-hal")
      (setq projectile-project-compilation-cmd "/bin/bash -c 'cd /build/mkorperx/ndg-android/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               m audio.primary.merrifield'")
      (setq projectile-project-test-cmd "ssh mako@acers5.tl.intel.com \
               'cd /home/mako/tools/install-ndg-android && make audiohal'"))
     ((string= (projectile-project-name)
               "robby")
      (setq projectile-project-compilation-cmd "/bin/bash -c 'cd /build/mkorperx/ndg-android/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               cd device/intel/robby/audio && mm'")
      (setq projectile-project-test-cmd "ssh mako@acers5.tl.intel.com \
               'cd /home/mako/tools/install-ndg-android && make pfw'"))
     ((string= (projectile-project-name)
               "bt")
      (setq projectile-project-compilation-cmd "/bin/bash -c 'cd ~/code/android/ndg-android-f44/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               cd system/bt/audio_a2dp_hw && mm'")
      (setq projectile-project-test-cmd "cd ~/code/android/ndg-android-f44/out/target/product/anthracite && \
               adb root && adb remount && \
               adb push system/lib/hw/lib/audio.a2dp.default.so /system/lib/hw"))
     ((string= (projectile-project-name)
               "kernel")
      (setq projectile-project-compilation-cmd "/bin/bash -c 'cd ~/code/android/ndg-android-f44/ && \
               source build/envsetup.sh && \
               lunch anthracite-userdebug && \
               mbimg -j3'")
      (setq projectile-project-test-cmd "cd ~/code/android/ndg-android-f44/out/target/product/anthracite && \
               adb reboot bootloader && \
               fastboot flash boot boot.img && \
               fastboot continue"))))
  (add-hook 'projectile-after-switch-project-hook
            #'my-switch-project-hook))

(defun makohoek-dev/post-init-magit ()
  (with-eval-after-load 'magit
    ;; performance tricks for magit (useful in kernel tree)
    ;; remove tag entry for magit status
    (setq magit-status-headers-hook (remove 'magit-insert-tags-header magit-status-headers-hook))
    ;; remove revision header in magit-diff
    (setq magit-revision-sections-hook (remove 'magit-insert-revision-headers magit-revision-sections-hook))
    ;; don't display --graph on magit-log
    (setq magit-log-arguments '("-n30" "--decorate"))
    ;; signed-off by default
    (setq magit-commit-arguments (quote ("--signoff")))
    (setq magit-revert-arguments (quote ("--signoff")))
    ;; better rebase
    (setq magit-rebase-arguments (quote ("--autosquash" "--autostash")))
    ;; title must be no longer than 50
    (setq git-commit-summary-max-length 50)
    ;; line must not be longer than 72
    (setq git-commit-fill-column 72)
    ;; support Tracked-On: + Change-Id: pseudo-headers
    (add-to-list 'git-commit-known-pseudo-headers "Tracked-On")
    (add-to-list 'git-commit-known-pseudo-headers "Changed-Id")
    ;; spell check when commiting
    (add-hook 'git-commit-setup-hook 'git-commit-turn-on-flyspell)))

(defun makohoek-dev/post-init-whitespace ()
  ;; whitespace mode
  (with-eval-after-load 'whitespace
    (setq whitespace-style '(face spaces tabs newline indentation tab-mark))
    ;; for tabs in between lines
    (set-face-attribute 'whitespace-tab nil :background nil
                        :foreground "DimGray")
    ;; for tabs as indentation
    (set-face-attribute 'whitespace-indentation
                        nil :background nil
                        :foreground "DimGray")
    ;; change tab displayed char to »
    (setq whitespace-display-mappings '((tab-mark 9
                                                  [187 9]
                                                  [92 9]) ; 9:tab, 187:»
                                        )))
  ;; enable whitespace mode in C and Cpp

  (add-hook 'c-mode-hook
            (function whitespace-mode))
  (add-hook 'c++-mode-hook
            (function whitespace-mode))
  ;; enable whitespace mode in elisp
  (add-hook 'emacs-lisp-mode-hook
            (function whitespace-mode))
  ;; enable whitespace mode in python
  (add-hook 'python-mode-hook
            (function whitespace-mode)))

(defun makohoek-dev/post-init-ediff ()
  ;; ediff customization: show char based diff
  (with-eval-after-load 'ediff
    (setq-default ediff-forward-word-function
                  'forward-char)))
;;; packages.el ends here
