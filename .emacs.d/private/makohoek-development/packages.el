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
    ag)
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

;;; packages.el ends here
