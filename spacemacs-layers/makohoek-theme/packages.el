;;; packages.el --- makohoek-theme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:
(setq makohoek-theme-packages '(base16-theme color-theme-sanityinc-tomorrow
                                             seoul256-theme solarized-theme spacegray-theme
                                             zenburn-theme))

;; programmatically defin the init functions
(dolist (pkg makohoek-theme-packages)
  (eval `(defun ,(intern (format "makohoek-theme/init-%S" pkg)) nil)))
;;; packages.el ends here
;;;
