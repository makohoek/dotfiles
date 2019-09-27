(when (configuration-layer/package-used-p 'projectile)
  ;; default values, must be overriden
  (setq makohoek-project/android/allowed-targets '("aosp_dragon" "aosp_shamu"))
  (setq makohoek-project/uboot/allowed-targets '("aosp_dragon" "aosp_shamu"))

  ;; override project definitions, if they are present
  (load "~/.dotfiles-private/spacemacs-layers/makohoek-project/projects.el" 't)

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

  (defun makohoek-project/clear-compilation-cmd-cache ()
    "Clears the compilation command cache
This is useful when switching between different lunch targets."
    ;; project is in our database: we don't want to use the "cached compilation cmd"
    ;; FIXME: should remove only the key/value for this project, not all
    (interactive)
    (clrhash projectile-compilation-cmd-map)
    (clrhash projectile-test-cmd-map))

  (defun makohoek-project/android/compile-kernel ()
    "Returns a String representing how to compile kernel in Android"
    (makohoek-project/android/compile-command "make bootimage"))

  (defun makohoek-project/android/compile-system ()
    "Returns a String representing how to compile system in Android"
    (makohoek-project/android/compile-command "make systemimage"))

  (defun makohoek-project/uboot/compile-prefix (config_name)
    "Returns a string representing the begin of an Uboot compilation command"
    (concat
     "/bin/bash -c '"
     "ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu- "
     "make " config_name "_defconfig" " && "
     "ARCH=arm64 CROSS_COMPILE=aarch64-linux-gnu- " ))

  (defun makohoek-project/uboot/compile-suffix ()
    "Returns a string representing the end of an Uboot compilation command"
    "'")

  (defun makohoek-project/uboot/compile-command (command)
    (setq target (completing-read "Target: " makohoek-project/uboot/allowed-targets))
    (concat (makohoek-project/uboot/compile-prefix target)
            command
            (makohoek-project/uboot/compile-suffix)))

  (defun makohoek-project/uboot/compile ()
    "Returns a String representing how to compile Uboot"
    (makohoek-project/uboot/compile-command "make")))
