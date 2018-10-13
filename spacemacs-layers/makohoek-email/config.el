;; mu4e
;;; Set up some common mu4e variables
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

(with-eval-after-load 'mu4e
(add-to-list 'mu4e-bookmarks
             (make-mu4e-bookmark
              :name  "Inbox"
              :query "maildir:/gmail/Inbox"
              :key ?i)))
