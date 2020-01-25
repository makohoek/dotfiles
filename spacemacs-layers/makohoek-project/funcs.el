(when (configuration-layer/package-used-p 'projectile)
  ;; default values, must be overriden
  (setq makohoek-project/android/allowed-targets '("aosp_dragon" "aosp_shamu"))
  (setq makohoek-project/uboot/allowed-targets '("aosp_dragon" "aosp_shamu"))

  (cl-defstruct android/kernel-build
    dist-folder
    sh-config
    aosp-out-folder)

  (setq makohoek-project/android/kernel-build-info-for-target
        '(("aosp_dragon" . (make-android/kernel-build
                            :dist-folder "~/src/aosp/device/google/dragon-kernel"
                            :sh-config "common/build.config.gki.aarch64"
                            :aosp-out-folder "out/target/product/dragon"))
          ("aosp_shamu"  . (make-android/kernel-build
                            :dist-folder "~/src/aosp/device/google/shamu-kernel"
                            :sh-config "common/build.config.gki.aarch64"
                            :aosp-out-folder "out/target/product/shamu"))))

  (setq makohoek-project/uboot/env-out-for-target
        '(("aosp_dragon" . "~/aosp/device/google/dragon/u-boot-env")
          ("aosp_shamu" . "~/aosp/device/google/shamu/u-boot-env")))

  (setq makohoek-project/uboot/fip-out-for-target
        '(("aosp_dragon" . "~/aosp/device/google/dragon/fip.bin")
          ("aosp_shamu" . "~/aosp/device/google/shamu/fip.bin")))

  ;; override project definitions, if they are present
  (load "~/.dotfiles-private/spacemacs-layers/makohoek-project/projects.el" 't)

  (defun makohoek-project/projectile-clear-compilation-cmd-cache ()
    "Clears the compilation command cache
This is useful when switching between different lunch targets."
    ;; project is in our database: we don't want to use the "cached compilation cmd"
    ;; FIXME: should remove only the key/value for this project, not all
    (interactive)
    (clrhash projectile-compilation-cmd-map)
    (clrhash projectile-test-cmd-map))

  (defun makohoek-project/android/compile-prefix (lunch-target)
    "Returns a string representing the begin of an Android compilation command"
    (concat
     "/bin/bash -c '"
     "source build/envsetup.sh"        " && "
     "lunch " lunch-target"-userdebug" " && "))

  (defun makohoek-project/android/compile-suffix ()
    "Returns a string representing the end of an Android compilation command"
    "'")

  (defun makohoek-project/android/compile-command (command)
    "Forges an Android compilation command
    Examples:
    (makohoek-project/android/compile-command \"make bootimage\")
    (makohoek-project/android/compile-command \"make systemimage\")
    (makohoek-project/android/compile-command \"m libbluetooth\")"
    (setq target (completing-read "Target: " makohoek-project/android/allowed-targets))
    (concat (makohoek-project/android/compile-prefix target)
            command
            (makohoek-project/android/compile-suffix)))

  (defun makohoek-project/android/compile-kernel ()
    "Returns a String representing how to compile kernel in Android"
    (setq target (completing-read "Target: " makohoek-project/android/allowed-targets))
    (let ((build-info (eval (cdr (assoc target makohoek-project/android/kernel-build-info-for-target))))
          (aosp-src-dir "~/src/aosp/"))
          (concat "/bin/bash -c '"
                  " DIST_DIR=" (android/kernel-build-dist-folder build-info)
                  " BUILD_CONFIG=" (android/kernel-build-sh-config build-info)
                  " SKIP_MRPROPER=1 build/build.sh" " && "
                  "cd " aosp-src-dir " && "
                  "source build/envsetup.sh && lunch " target "-userdebug && "
                  "croot && "
                  "make bootimage vendorimage " (android/kernel-build-aosp-out-folder build-info) "/dtbo.img"
                  (makohoek-project/android/compile-suffix))))

  (defun makohoek-project/android/flash-kernel ()
    "Returns a String representing how to flash kernel in Android"
    (setq target (completing-read "Target: " makohoek-project/android/allowed-targets))
    (let ((build-info (eval (cdr (assoc target makohoek-project/android/kernel-build-info-for-target))))
          (aosp-src-dir "~/src/aosp/"))
      (concat "/bin/bash -c '"
              "cd " aosp-src-dir " && "
              "cd " (android/kernel-build-aosp-out-folder build-info) " && "
              "python2 flashimage.py --boot"
              (makohoek-project/android/compile-suffix))))

  (defun makohoek-project/android/compile-system ()
    "Returns a String representing how to compile system in Android"
    (makohoek-project/android/compile-command "make systemimage"))

  (defun makohoek-project/android/compile-vendor ()
    "Returns a String representing how to compile system in Android"
    (makohoek-project/android/compile-command "make vendorimage"))

  (defun makohoek-project/uboot/compile-prefix (config_name)
    "Returns a string representing the begin of an Uboot compilation command"
    (concat
     "/bin/bash -c '"
     "ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu- "
     "make " config_name "_defconfig" " && "
     "ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu- " ))

  (defun makohoek-project/uboot/compile-suffix (env-out-file fip-image)
    "Returns a string representing the end of an Uboot compilation command"
    (concat
     " && ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu- "
     "scripts/get_default_envs.sh " " > "
     env-out-file
     " && " "fiptool update "
     fip-image
     " --nt-fw u-boot.bin"
     "'"))

  (defun makohoek-project/uboot/compile-command (command)
    (setq target (completing-read "Target: " makohoek-project/uboot/allowed-targets))
    (setq env-out-file (cdr (assoc target makohoek-project/uboot/env-out-for-target)))
    (setq fip-out-file (cdr (assoc target makohoek-project/uboot/fip-out-for-target)))
    (concat (makohoek-project/uboot/compile-prefix target)
            command
            (makohoek-project/uboot/compile-suffix env-out-file fip-out-file)))

  (defun makohoek-project/uboot/compile ()
    "Returns a String representing how to compile Uboot"
    (makohoek-project/uboot/compile-command "make")))
