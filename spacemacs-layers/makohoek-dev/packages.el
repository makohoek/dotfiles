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


(defconst makohoek-dev-packages '(dtrt-indent ag ediff projectile magit whitespace pydoc xcscope (log-tools :location local) (lt-logcat :location local) (lt-serial :location local) (lt-serial-kernel :location local))
  "The list of Lisp packages required by the makohoek-dev layer.")

(defun makohoek-dev/init-dtrt-indent ()
  ;; enable dtrt-indent for c development
  (add-hook 'c-mode-common-hook
            (lambda ()
              (require 'dtrt-indent)
              (dtrt-indent-mode t))))

(defun makohoek-dev/init-ag ()
  ;; nothing to configure here
  (spacemacs/declare-prefix "i" "intel-tools")
  (spacemacs/set-leader-keys "is" 'ag)
  )

(defun logtools-run-logcat ()
  (interactive)
  (log-tools "logcat"))

(defun logtools-run-kernel ()
  (interactive)
  (log-tools "serial-kernel"))

(defun makohoek-dev/init-log-tools()
  ;; nothing to configure here
  (require 'log-tools)
  (spacemacs/declare-prefix "il" "intel-log-tools")
  (spacemacs/set-leader-keys "ill" 'logtools-run-logcat)
  (spacemacs/set-leader-keys "ilk" 'logtools-run-kernel)
  )

(defun makohoek-dev/init-lt-logcat()
  ;; nothing to configure here
  (require 'lt-logcat)
  )

(defun makohoek-dev/init-pydoc()
  ;; python documentation remap to pydoc-at-point
  (require 'pydoc)
  (with-eval-after-load 'pydoc
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "hh" 'pydoc-at-point))
  )

(defun makohoek-dev/init-lt-serial()
  ;; nothing to configure here
  (require 'lt-serial)
  (customize-set-variable 'lt-serial-default-port "ttyUSB1"))

(defun makohoek-dev/init-lt-serial-kernel()
  ;; nothing to configure here
  (require 'lt-serial-kernel))

(defun makohoek-dev/init-xcscope()
  (with-eval-after-load 'xcscope
    ;; The -q option in cscope: use an inverted database index. Takes
    ;; longer to build, but results in faster lookups. Useful for very
    ;; large codebases
    (setq cscope-option-use-inverted-index t)))

(cl-defstruct makohoek-project
  name            ; name of the projectile project . This is matched with the git folder name
  compile-command ; compile command for this project
  test-command    ; test command for this project
  (android nil))  ; is this project part of the android tree? if yes, commands will be run from root dir

(setq makohoek-project-list '())

;; default values, must be overriden
(setq private-android-code-directory nil)
(setq private-android-allowed-targets '("aosp_dragon" "aosp_shamu"))

;; Load private projects, if they exist
(setq private-projects "~/dotfiles-private/spacemacs/makohoek-dev/projects.el")
(load private-projects 't)

(defun makohoek-project-make-android-prefix (target)
  (setq root-directory private-android-code-directory)
  (concat
   "/bin/bash -c 'cd " root-directory " && "
   "source build/envsetup.sh"         " && "
   "lunch " target "-userdebug"       " && "))

(defun makohoek-project-make-android-test-prefix (target)
  (setq root-directory private-android-code-directory)
  (concat
    "/bin/bash -c 'export PATH=$PATH:/home/mako/bin/platform-tools && "
    "cd " root-directory " && "
    "source build/envsetup.sh"         " && "
    "lunch " target "-userdebug"       " && "
    "croot && cd out/target/product/" target "/ && "))

(defun makohoek-dev/post-init-projectile ()
  ;; do not run find-file after a project switch
  (setq projectile-switch-project-action 'projectile-dired)
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
              (setq projectile-project-test-cmd
                    (concat
                     (makohoek-project-make-android-test-prefix selected-target)
                     (makohoek-project-test-command proj)
                     "'"))
              )
          ;; else, just set the variables
          (progn
            (setq projectile-project-compilation-cmd (makohoek-project-compile-command proj))
            (setq projectile-project-test-cmd (makohoek-project-test-command proj)))))))

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
    (add-to-list 'git-commit-known-pseudo-headers "Change-Id")

    ;; magit push gerrit
    ;; from JM-config: https://github.com/JulienMasson/jm-config
    (defun magit-git-push-gerrit (branch target args)
      (run-hooks 'magit-credential-hook)
      (-let [(remote . target)
             (magit-split-branch-name target)]
        (magit-run-git-async "push" "-v" args remote
                             (format "%s:refs/for/%s" branch target))))

    (defun magit-push-gerrit (source target args)
      "Push an arbitrary branch or commit somewhere.
  Both the source and the target are read in the minibuffer."
      (interactive
       (let ((source (magit-read-local-branch-or-commit "Push")))
         (list source
               (magit-read-remote-branch (format "Push %s to" source) nil
                                         (magit-get-upstream-branch source)
                                         source 'confirm)
               (magit-push-arguments))))
      (magit-git-push-gerrit source target args))

    (magit-define-popup-action 'magit-push-popup
      ?g "gerrit" 'magit-push-gerrit)
  ))

(defun makohoek-dev/post-init-whitespace ()
  ;; whitespace mode
  (with-eval-after-load 'whitespace
    (setq whitespace-style '(face spaces tabs newline indentation trailing tab-mark))
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
  ;; enable whitespace mode in go
  (add-hook 'go-mode-hook
            (function whitespace-mode))
  ;; enable whitespace mode in makefile mode
  (add-hook 'makefile-mode-hook
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
