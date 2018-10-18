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

(defconst makohoek-org-packages '(org org-gcal))

(defun makohoek-org/init-org-gcal()
  (use-package org-gcal
    :commands org-gcal-fetch
    :config
    ;; XXX: auth-source-pass is emacs 26 only
    (use-package auth-source-pass
      :config
      (auth-source-pass-enable))
    (setq org-gcal-client-id (auth-source-pass-get "client-id" "org-gcal-makohoek")
          org-gcal-client-secret (auth-source-pass-get "client-secret" "org-gcal-makohoek")
          org-gcal-file-alist '(("mattijs.korpershoek@gmail.com" .  "~/org/gcal-main.org")))))

;; org is owned by the org layer
(defun makohoek-org/post-init-org ()
  (use-package org
    :init
    ;; org todo keywords
    (setq org-todo-keywords
          '((sequence "TODO(t!)"
                      "ONGOING(i!)"
                      "WAIT(w@/!)" "|"
                      "DONE(d)" "CANCELLED(c)")
                              ))
    ;; org todo keywords colors
    (setq org-todo-keyword-faces
          '(("TODO"      . org-todo)
            ("ONGOING"   . "orange")
            ("WAIT"      . "orange")
            ("DONE"      . org-done)
            ("CANCELLED" . org-done)))
    ;; org-agenda files
    (setq org-agenda-files
          '("~/org/work.org"
            "~/org/Notes.org"
            "~/org/calendar.org"
            "~/org/gcal-main.org"))
    :config
    ;; better shortcut for org-toggle-checkbox (WHY C-c C-x C-b????)
    (spacemacs/set-leader-keys-for-major-mode
      'org-mode "k" 'org-toggle-checkbox)
    ;; org-pomodoro notification once pomodoro is completed
    (defun pomodoro-completed ()
      (notifications-notify :title "Pomodoro completed"
                            :body "Go take a break"
                            :timeout 0))
    (defun pomodoro-break-completed ()
      (notifications-notify :title "Break done"
                            :body "Go fix some code"
                            :timeout 0))
    (add-hook 'org-pomodoro-finished-hook
              (function pomodoro-completed))
    (add-hook 'org-pomodoro-break-finished-hook
              (function pomodoro-break-completed))

  ;; org-sync + makefile from jgoerzen (thanks !)
  ;; https://github.com/jgoerzen/public-snippets/blob/master/emacs/org-tools/emacs-config.org
  (defun jgoerzen/org-sync-sentinel (_process retcode)
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
  (defun jgoerzen/org-sync ()
    "Sync org."
    (interactive)
    (org-save-all-org-buffers)
    (switch-to-buffer "*Org Sync Output*")
    (erase-buffer)
    (make-process
     :name "org-sync"
     :buffer "*Org Sync Output*"
     :command (cl-list* "make" (list "-C" "~/org"))
     :sentinel 'jgoerzen/org-sync-sentinel))

  (spacemacs/set-leader-keys "is" 'jgoerzen/org-sync)))

;;; packages.el ends here
