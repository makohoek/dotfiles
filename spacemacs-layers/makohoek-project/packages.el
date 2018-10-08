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

(cl-defstruct makohoek-project-struct
  name            ; name of the projectile project . This is matched with the git folder name
  compile-command ; compile command for this project
  test-command    ; test command for this project
  (android nil))  ; is this project part of the android tree? if yes, commands will be run from root dir

(setq makohoek-project-list '())

;; default values, must be overriden
(setq private-android-code-directory nil)
(setq private-android-allowed-targets '("aosp_dragon" "aosp_shamu"))

;; Load private projects, if they exist
(setq private-projects "~/.dotfiles-private/spacemacs-layers/makohoek-project/projects.el")
(load private-projects 't)

(defun makohoek-project-make-android-prefix (target)
  (setq root-directory private-android-code-directory)
  (concat
   "/bin/bash -c 'cd " root-directory " && "
   "source build/envsetup.sh"         " && "
   "lunch " target "-userdebug"       " && "))

;; projectile is owned by 'spacemacs-project'
(defun makohoek-project/post-init-projectile ()
  ;; specific per-project compile commands
  (defun my-switch-project-hook ()
    "Perform some action after switching Projectile projects."
    (dolist (proj makohoek-project-list)
      ;; project is in our database: we don't want to use the "cached compilation cmd"
      ;; FIXME: should remove only the key/value for this project, not all
      (clrhash projectile-compilation-cmd-map)
      (clrhash projectile-test-cmd-map)
      (when (string= (projectile-project-name) (makohoek-project-name proj))
        ;; if project exists, check if it is an android project
        (if (makohoek-project-android proj)
            ;; if it is an android project, ask for target + prefix&postfix the compile command
            (progn
              (setq allowed-targets private-android-allowed-targets)
              (setq selected-target (ivy-completing-read "target: " allowed-targets))
              (setq projectile-project-compilation-cmd
                    (concat
                     (makohoek-project-make-android-prefix selected-target)
                     (makohoek-project-compile-command proj)
                     "'"))
              (setq projectile-project-test-cmd (makohoek-project-test-command proj))
              )
          ;; else, just set the variables
          (progn
            (setq projectile-project-compilation-cmd (makohoek-project-compile-command proj))
            (setq projectile-project-test-cmd (makohoek-project-test-command proj)))))))

  (add-hook 'projectile-after-switch-project-hook
            #'my-switch-project-hook))

(defun makohoek-project/post-init-counsel-projectile ()
    ;; do not run find-file after a project switch
    (with-eval-after-load 'counsel-projectile
      (counsel-projectile-modify-action
       'counsel-projectile-switch-project-action
       '((default counsel-projectile-switch-project-action-dired)))))

;;; packages.el ends here
