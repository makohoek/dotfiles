;;; packages.el --- makohoek-theme layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
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
;; added to `makohoek-theme-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `makohoek-theme/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `makohoek-theme/pre-init-PACKAGE' and/or
;;   `makohoek-theme/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:
(setq makohoek-theme-packages
  '(
    seoul256-theme
    base16-theme
    ))
;; programmatically defin the init functions
(dolist (pkg makohoek-theme-packages)
  (eval `(defun ,(intern (format "makohoek-theme/init-%S" pkg)) nil)))
;;; packages.el ends here
;;;
