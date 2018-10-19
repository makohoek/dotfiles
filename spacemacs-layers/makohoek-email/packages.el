;;; packages.el --- makohoek-email layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:
(defconst makohoek-email-packages '(mu4e))

;; mu4e is owned by the mu4e layer
(defun makohoek-email-post-init-mu4e()
  (use-package mu4e
    ;; defer loading, only when we invoke mu4e
    :command mu4e
    :init
    ;; Set up some common mu4e variables
    (setq mu4e-maildir "~/.email"
          mu4e-sent-folder "/gmail/[Gmail]/Sent Mail"
          mu4e-drafts-folder "/gmail/[Gmail]/Drafts"
          mu4e-trash-folder "/gmail/[Gmail]/Trash"
          mu4e-refile-folder "/gmail/archive"
          mu4e-get-mail-command "mbsync -a"
          mu4e-update-interval nil
          mu4e-compose-signature-auto-include nil
          mu4e-view-show-images t
          mu4e-view-show-addresses t)

    ;; this is a fix for mbsync errors
    ;; http://pragmaticemacs.com/emacs/fixing-duplicate-uid-errors-when-using-mbsync-and-mu4e/
    (setq mu4e-change-filenames-when-moving t)

    ;; change headers in mail view
    (setq mu4e-headers-date-format "%a %b %d %R %Y")
    ;; Fri Jan 26 09:48:44 2018

    (setq mu4e-headers-fields '((:date    .  23)
                                (:from    .  28)
                                (:subject .  nil)))
    (setq mu4e-view-fields '(:from :to :cc :subject :date :mailing-list :attachments :signature))

    ;; enable inline images
    (setq mu4e-view-show-images t)

    ;; body display on the html-version
    (setq mu4e-view-prefer-html t)

    ;; show full addresses in view message
    (setq mu4e-view-show-addresses 't)
    :config
    (add-to-list 'mu4e-bookmarks
                 (make-mu4e-bookmark
                  :name  "Inbox"
                  :query "maildir:/gmail/Inbox"
                  :key ?i))))
