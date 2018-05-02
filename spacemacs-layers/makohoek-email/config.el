;; mu4e
;;; Set up some common mu4e variables
(setq mu4e-maildir "~/.mail"
      mu4e-sent-folder "/intel/Sent Items"
      mu4e-draft-folder "/intel/Drafts"
      mu4e-trash-folder "/intel/Deleted Items"
      mu4e-refile-folder "/Archive"
      mu4e-get-mail-command "offlineimap"
      mu4e-update-interval nil
      mu4e-compose-signature-auto-include nil
      mu4e-view-show-images t
      mu4e-view-show-addresses t)

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
