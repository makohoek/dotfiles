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
  ;; org-sync + makefile from jgoerzen (thanks !)
  ;; https://github.com/jgoerzen/public-snippets/blob/master/emacs/org-tools/emacs-config.org
  (defun makohoek-org/org-sync-sentinel (_process retcode)
    "Handle output from the org-sync"
    (if (equal retcode "finished\n")
        (progn (org-revert-all-org-buffers)
               (org-id-update-id-locations)
               (org-save-all-org-buffers)
               (switch-to-buffer "*Org Sync Output*")
               (make-process
                :name "org-sync"
                :buffer "*Org Sync Output*"
                :command (cl-list* "make" (list "-C" "~/org" "push"))
                ))))

  (defun makohoek-org/org-sync ()
    "Sync org."
    (interactive)
    (org-save-all-org-buffers)
    (switch-to-buffer "*Org Sync Output*")
    (erase-buffer)
    (make-process
     :name "org-sync"
     :buffer "*Org Sync Output*"
     :command (cl-list* "make" (list "-C" "~/org"))
     :sentinel 'makohoek-org/org-sync-sentinel)))

(defun makohoek-org/ews-work-calendar ()
  (interactive)
  (let ((default-directory "~/bin/ews-orgmode/"))
    (async-shell-command "python ews-fetch-calendar.py > ~/org/work-cal.org" "*org-work-cal*")))

(defun makohoek-org/fetch-all-calendars ()
  (interactive)
  (makohoek-org/ews-work-calendar)
  (org-gcal-fetch))
