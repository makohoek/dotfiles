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
     org-plus-contrib))
;; ox-confluence is installed by `org-plus-contrib'

(defun makohoek-org/init-org-clock-split()
  (use-package org-clock-split
    :commands org-clock-split))

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
    (setq org-default-notes-file "~/work/org/inbox.org")
    ;; org-agenda files
    :config
    (setq org-agenda-files
     '("~/work/org/deft"))
    (setq org-capture-templates
      `(("t" "Todo" entry (file+headline org-default-notes-file "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("p" "Pomodoro" entry (file+olp+datetree "~/work/org/deft/pomodoro/2023.org" "Pomodoro")
         ,(concat "* stats [/][\%]\n:PROPERTIES:\n:LOGGING: nil\n:END:\n" (string-join (make-list 15 "\n** TODO ")))
         :tree-type week
         :jump-to-captured 't)))
    ;; org-pomodoro notification once pomodoro is completed
    (defun pomodoro-completed ()
      (notifications-notify :title "Pomodoro completed"
                            :body "Go take a break"
                            :timeout 0))
    (defun pomodoro-break-completed ()
      (notifications-notify :title "Break done"
                            :body "Time for another pomodoro"
                            :timeout 0))
    (add-hook 'org-pomodoro-finished-hook
              (function pomodoro-completed))
    (add-hook 'org-pomodoro-break-finished-hook
              (function pomodoro-break-completed))
    :hook
    ((org-agenda-mode . emojify-mode))))

;; deft is owned by the deft layer
(defun makohoek-org/pre-init-deft ()
  (use-package deft
    :commands deft
    :init
    (setq deft-directory "~/work/org/deft")))
;;; packages.el ends here
