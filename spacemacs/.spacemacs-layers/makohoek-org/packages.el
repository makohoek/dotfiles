;;; packages.el --- makohoek-org layer packages file for Spacemacs.
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

(defconst makohoek-org-packages '(org
                                  org-gcal
                                  org-wunderlist
                                  org-plus-contrib))
                                  ;; ox-confluence is installed by `org-plus-contrib'

(defun makohoek-org/init-org-gcal()
  (use-package org-gcal
    :commands org-gcal-fetch
    :config
    ;; XXX: auth-source-pass is emacs 26 only
    ;; (use-package auth-source-pass
    ;;   :config
    ;;   (auth-source-pass-enable))
    ;; FIXME: re-enable this when I use it again
    (setq org-gcal-client-id (auth-source-pass-get "client-id" "org-gcal-makohoek")
          org-gcal-client-secret (auth-source-pass-get "client-secret" "org-gcal-makohoek")
          org-gcal-file-alist
          '(("mattijs.korpershoek@gmail.com" .  "~/home/org/calendars/gcal-main.org")
            ("ghitimou3nseap7k05iskembpk@group.calendar.google.com" .  "~/home/org/calendars/gcal-shared.org")))))

(defun makohoek-org/init-org-wunderlist()
  (use-package org-wunderlist
    :commands org-wunderlist-fetch
    :config
    ;; XXX: auth-source-pass is emacs 26 only
    (use-package auth-source-pass
      :config
      (auth-source-pass-enable))
    (setq org-wunderlist-client-id (auth-source-pass-get "client-id" "org-wunderlist-makohoek")
          org-wunderlist-token (auth-source-pass-get "token" "org-wunderlist-makohoek")
          org-wunderlist-file  "~/org/wunderlist.org"
          org-wunderlist-dir "~/org/wunderlist/")))

;; org-plus-contrib is owned by the org layer
(defun makohoek-org/post-init-org-plus-contrib()
  (use-package ox-confluence
    :commands org-confluence-export-as-confluence))

;; org is owned by the org layer
(defun makohoek-org/post-init-org ()
  (use-package org
    :defer t
    :init
    ;; org todo keywords
    (setq org-todo-keywords
          '((sequence "TODO(t!)"
                      "ONGOING(i!)"
                      "WAIT(w@/!)" "|"
                      "DONE(d!)" "CANCELLED(c!)" "NOTE(n)")
                              ))
    ;; org todo keywords colors
    (setq org-todo-keyword-faces
          '(("TODO"      . org-todo)
            ("ONGOING"   . "orange")
            ("WAIT"      . "orange")
            ("NOTE"      . font-lock-comment-face)
            ("DONE"      . org-done)
            ("CANCELLED" . org-done)))
    (setq org-default-notes-file "~/work/org/baylibre/inbox.org")
    ;; org-agenda files
    :config
    (setq org-agenda-files
     '("~/work/org/baylibre/"
       "~/work/org/calendars/"
       "~/home/org/calendars/"))
    (setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("p" "Pomodoro" entry (file+headline "~/work/org/pomodoro.org" "Pomodoro")
         "* %t [/][\%]\n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n- [ ] \n")))
    ;; org-pomodoro notification once pomodoro is completed
    (defun pomodoro-completed ()
      (notifications-notify :title "Pomodoro completed"
                            :body "Go take a break"
                            :timeout 0))
    (defun pomodoro-break-completed ()
      (notifications-notify :title "Break done"
                            :body "Time for another pomodoro"
                            :timeout 0))
    (defun pomodoro-started ()
      (write-region (format-time-string "%H:%M" org-pomodoro-end-time) nil "~/.org-pomodoro/end-time"))
    (add-hook 'org-pomodoro-finished-hook
              (function pomodoro-completed))
    (add-hook 'org-pomodoro-break-finished-hook
              (function pomodoro-break-completed))
    (add-hook 'org-pomodoro-started-hook
              (function pomodoro-started))
    :hook
    ((org-agenda-mode . emojify-mode))))


;;; packages.el ends here
