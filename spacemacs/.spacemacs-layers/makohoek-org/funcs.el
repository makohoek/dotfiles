;;; funcs.el --- makohoek-org functions file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (configuration-layer/package-used-p 'org)
  (defun makohoek-org/split-25mins-pomodoro ()
    (interactive)
    (org-clock-split nil "25m"))

  (defun makohoek-org/mark-task-as-done()
    (interactive)
    (org-todo "DONE")))


