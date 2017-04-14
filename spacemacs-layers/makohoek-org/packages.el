;;; packages.el --- makohoek-org layer packages file for Spacemacs.
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
;; added to `makohoek-org-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `makohoek-org/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `makohoek-org/pre-init-PACKAGE' and/or
;;   `makohoek-org/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst makohoek-org-packages '(org))

(defun makohoek-org/post-init-org ()
  ;; point towards default reveal directory
  ;; FIXME: reveal.js should probably be a submodule of this repo
  (setq org-reveal-root "file:///home/mako/code/js/reveal.js-master/")
  (with-eval-after-load 'org
    ;; org todo keywords
    (setq org-todo-keywords '((sequence "TODO" "IN PROGRESS" "REVIEW" "|"
                                        "DONE")))
    ;; org todo keywords colors
    (setq org-todo-keyword-faces '(("TODO" . org-warning)
                                   ("IN PROGRESS" . "orange")
                                   ("REVIEW" . "orange")))
    ;; better shortcut for org-toggle-checkbox (WHY C-c C-x C-b????)
    (spacemacs/set-leader-keys-for-major-mode
      'org-mode "k" 'org-toggle-checkbox)
    ;; org-pomodoro notification once pomodoro is completed
    (defun pomodoro-completed ()
      (notifications-notify :title "Pomodoro completed"
                            :body "Go take a break"
                            :timeout 0))
    (defun pomodoro-break-completed ()
      (notifications-notify :title "Break done"
                            :body "Go fix some code"
                            :timeout 0))
    (add-hook 'org-pomodoro-finished-hook
              (function pomodoro-completed))
    (add-hook 'org-pomodoro-break-finished-hook
              (function pomodoro-break-completed))))

;;; packages.el ends here
