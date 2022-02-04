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


;; FIXME: might need to check for org-gcal-fetch being available
(when (configuration-layer/package-used-p 'org)
  (defun makohoek-org/ews-work-calendar-sentinel (process event)
    (princ
     (format "%s: %s" process event)))
  (defun makohoek-org/ews-work-calendar ()
    (interactive)
    (let ((default-directory "~/bin/ews-orgmode/"))
      (set-process-sentinel
       (start-process-shell-command "org-work-cal" "*org-work-cal*"
                                    "python"
                                    "ews-fetch-calendar.py > ~/org/work-cal.org")
       'makohoek-org/ews-work-calendar-sentinel)))

  (defun makohoek-org/fetch-all-calendars ()
    (interactive)
    (makohoek-org/ews-work-calendar)
    (org-gcal-fetch))

  (defun makohoek-org/split-25mins-pomodoro ()
    (interactive)
    (org-clock-split nil "25m"))

  (defun makohoek-org/mark-task-as-done()
    (interactive)
    (org-todo "DONE")))


