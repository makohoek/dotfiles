;;; packages.el --- makohoek-window layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst makohoek-window-packages
  '(window-purpose)
  "The list of Lisp packages required by the makohoek-window layer.")

;; spacemacs-purpose is owned by the spacemacs-purpose layer
(defun makohoek-window/post-init-window-purpose ()
  (use-package window-purpose
    :config
    ;; not really window-purpose related, but still window related
    ;; never split vertically (one above another)
    (setq split-height-threshold nil)

    ;; See: https://github.com/syl20bnr/spacemacs/issues/6649#issuecomment-234490739
    ;; replace default entries
    (setcdr (assoc "*compilation*" popwin:special-display-config)
            '(:dedicated t :position right :stick t :noselect t :width 0.3))

    ;; add new entries
    (push '("*ripgrep-search*" :dedicated t :position right :stick t :noselect nil :width 0.3)
          popwin:special-display-config)

    ;; sync popwin:special-display-config to pupo as documented
    (pupo/update-purpose-config)))

;;; packages.el ends here
