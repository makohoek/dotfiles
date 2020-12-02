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
  '(magit
    (forge :location (recipe
                      :fetcher github
                      :repo JulienMasson/forge
                      :branch "code-review-support"
                      :files ("*.el" "lisp/*.el"))))
  "The list of Lisp packages required by the makohoek-git layer.")

(defun makohoek-git/init-forge ()
  (use-package forge
    :after magit))

;; magit is owned by layer 'git'
(defun makohoek-git/post-init-magit ()
  (use-package magit
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

    ;; title must be no longer than 50
    (setq git-commit-summary-max-length 50)
    ;; line must not be longer than 72
    (setq git-commit-fill-column 72)
    ;; support Bug: + Change-Id: pseudo-headers
    (add-to-list 'git-commit-known-pseudo-headers "Bug")
    (add-to-list 'git-commit-known-pseudo-headers "Fixes")
    (add-to-list 'git-commit-known-pseudo-headers "Change-Id")))


;;; packages.el ends here
