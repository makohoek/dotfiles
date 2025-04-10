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
  (defun makohoek-org/split-25mins-pomodoro ()
    (interactive)
    (org-clock-split nil "25m"))

  (defun makohoek-org/mark-task-as-done()
    (interactive)
    (org-todo "DONE"))

  (defun pomodoro-completed ()
    (notifications-notify :title "Pomodoro completed"
                          :body "Go take a break" :timeout 0))
  (defun pomodoro-break-completed ()
    (notifications-notify :title "Break done"
                          :body "Time for another pomodoro" :timeout 0)))

(defun makohoek-org/count-tags-in-subtree ()
  "Count occurrences of each tag in the current subtree."
  (interactive)
  (let ((tags-count (make-hash-table :test 'equal))
        (current-heading (point)))
    (org-map-entries
     (lambda ()
       (let ((tags (org-get-tags)))
         (dolist (tag tags)
           (puthash tag (1+ (gethash tag tags-count 0)) tags-count))))
     t 'tree)
    (with-output-to-temp-buffer "*Tag Statistics*"
      (princ "Tag statistics in current subtree:\n\n")
      (maphash (lambda (tag count)
                 (princ (format "%-20s %d\n" tag count)))
               tags-count))))


