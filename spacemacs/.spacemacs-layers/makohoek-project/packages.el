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
    (setq projectile-switch-project-action #'projectile-dired)))

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
