;;; packages.el --- makohoek-git layer packages file for Spacemacs.
;;
;; Copyright (c) 2020 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:

(defconst makohoek-git-packages
  '(magit sqlite3)
  "The list of Lisp packages required by the makohoek-git layer.")

(defun makohoek-git/init-sqlite3 ()
  (use-package sqlite3))

;; magit is owned by layer 'git'
(defun makohoek-git/post-init-magit ()
  (use-package magit
    :after sqlite3
    :config
    ;; performance tricks for magit (useful in kernel tree)
    ;; remove tag entry for magit status
    (setq magit-status-headers-hook (remove 'magit-insert-tags-header magit-status-headers-hook))
    ;; remove revision header in magit-diff
    (setq magit-revision-sections-hook (remove 'magit-insert-revision-headers magit-revision-sections-hook))
    ;; signed-off by default
    ;; set signoff by default
    (defvar transient-default-values '((magit-commit "--signoff")
                                       (magit-revert "--signoff")
                                       (magit-log:magit-log-mode "-n256" "--decorate")
                                       (magit-rebase "--autosquash" "--autostash")))
    (setq transient-values transient-default-values)

    (setq magit-diff-refine-hunk 'all)

    ;; display buffer below selected window
    (setq transient-display-buffer-action '(display-buffer-below-selected))
    (setq window-combination-limit #'display-buffer)

    ;; title must be no longer than 50
    (setq git-commit-summary-max-length 50)
    ;; line must not be longer than 72
    (setq git-commit-fill-column 72)
    ;; support Bug: + Change-Id: pseudo-headers
    (add-to-list 'git-commit-trailers "Bug")
    (add-to-list 'git-commit-trailers "Fixes")
    (add-to-list 'git-commit-trailers "Suggested-by")
    (add-to-list 'git-commit-trailers "Change-Id")))
;;; packages.el ends here
