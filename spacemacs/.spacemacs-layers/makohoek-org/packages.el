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
;; ox-jira is installed by `org'

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
            ;; Use let to create a dynamic filename for Pomodoro entries
            ,(let ((filename (concat "~/work/org/deft/pomodoro/"
                                     (format-time-string "%Y-%m")
                                     ".org")))
               `("p" "Pomodoro" entry (file+olp+datetree ,filename)
                 ,(concat "* stats [/][%]\n:PROPERTIES:\n:LOGGING: nil\n:END:\n"
                          (string-join (make-list 14 "\n** TODO ")))
                 :tree-type week
                 :jump-to-captured 't))))
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
