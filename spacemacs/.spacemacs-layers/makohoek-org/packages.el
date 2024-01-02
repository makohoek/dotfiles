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

(defconst makohoek-org-packages
  '(deft
     org
     org-clock-split
     (org-gcal :location local)
     org-plus-contrib))
;; ox-confluence is installed by `org-plus-contrib'

(defun makohoek-org/init-org-clock-split()
  (use-package org-clock-split
    :commands org-clock-split))

(defun makohoek-org/init-org-gcal()
  (use-package org-gcal
    :commands org-gcal
    :config
    ;; add work calendar
    (auth-source-pass-enable)
    (org-gcal-register-account :name "work"
                               :directory "~/.emacs.d/.cache/org/gcal-work/"
                               :client-id (auth-source-pass-get "client-id" "baylibre/org-gcal-bl")
                               :client-secret (auth-source-pass-get "client-secret" "baylibre/org-gcal-bl")
                               :calendars '(("mkorpershoek@baylibre.com" . "work.org")))
    (add-to-list 'org-agenda-files "~/.emacs.d/.cache/org/gcal-work/work.org")
    (org-gcal-register-account :name "mattijs"
                               :directory "~/.emacs.d/.cache/org/gcal-mattijs/"
                               :client-id (auth-source-pass-get "client-id" "org-gcal-makohoek")
                               :client-secret (auth-source-pass-get "client-secret" "org-gcal-makohoek")
                               :calendars '(("mattijs.korpershoek@gmail.com" . "mattijs.org")
                                            ("ghitimou3nseap7k05iskembpk@group.calendar.google.com" . "couple.org")
                                            ("jhtiaatk3uqnei6e67ikrjfts8@group.calendar.google.com" . "family.org")))
    (add-to-list 'org-agenda-files "~/.emacs.d/.cache/org/gcal-mattijs/mattijs.org")
    (add-to-list 'org-agenda-files "~/.emacs.d/.cache/org/gcal-mattijs/couple.org")
    (add-to-list 'org-agenda-files "~/.emacs.d/.cache/org/gcal-mattijs/family.org")))

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
          '((sequence "TODO(t!)" "ONGOING(i!)" "WAIT(w@/!)" "|"
                      "DONE(d!)" "CANCELLED(c!)" "NOTE(n)")))
    ;; org todo keywords colors
    (setq org-todo-keyword-faces
          '(("TODO"      . org-todo)
            ("ONGOING"   . "orange")
            ("WAIT"      . "orange")
            ("NOTE"      . font-lock-comment-face)
            ("DONE"      . org-done)
            ("CANCELLED" . org-done)))
    (setq org-default-notes-file "~/work/org/deft/inbox.org")
    ;; org-agenda files
    :config
    (add-to-list 'org-agenda-files "~/work/org/deft")
    (setq org-agenda-prefix-format '((agenda . "%-13i %?-12t") (tags . "%-14i")))
    (setq org-capture-templates
      `(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("p" "Pomodoro" entry (file+olp+datetree "~/work/org/deft/pomodoro/2024.org" "Pomodoro")
         ,(concat "* stats [/][\%]\n:PROPERTIES:\n:LOGGING: nil\n:END:\n" (string-join (make-list 14 "\n** TODO ")))
         :tree-type week
         :jump-to-captured 't)))
    ;; org-pomodoro notification once pomodoro is completed
    :hook
    ((org-pomodoro-finished . pomodoro-completed)
     (org-pomodoro-break-finished . pomodoro-break-completed))))

;; deft is owned by the deft layer
(defun makohoek-org/pre-init-deft ()
  (use-package deft
    :commands deft
    :init
    (setq deft-directory "~/work/org/deft")))
;;; packages.el ends here
