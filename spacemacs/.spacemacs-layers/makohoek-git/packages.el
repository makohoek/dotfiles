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
  '(magit sqlite3 display-fill-column-indicator)
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
    ;; Show signoff in transient
    ;; See: https://github.com/magit/magit/issues/2993#issuecomment-2700472389
    (transient-set-default-level 'magit:--signoff 1)

    (setq magit-diff-refine-hunk 'all)

    ;; display buffer below selected window
    (setq transient-display-buffer-action '(display-buffer-below-selected))
    (setq window-combination-limit #'display-buffer)

    (setq git-commit-summary-max-length 70)
    ;; support Bug: + Change-Id: pseudo-headers
    (add-to-list 'git-commit-trailers "Bug")
    (add-to-list 'git-commit-trailers "Fixes")
    (add-to-list 'git-commit-trailers "Suggested-by")
    (add-to-list 'git-commit-trailers "Change-Id")))

;; display-fill-column-indicator is owned by layer 'spacemacs-visual'
(defun makohoek-git/post-init-display-fill-column-indicator ()
  (use-package display-fill-column-indicator
    :when (>= emacs-major-version 28)
    :config
    (set-face-attribute 'fill-column-indicator nil
                        :foreground "gray30"
                        :background "gray30")
    (add-hook 'git-commit-setup-hook
              (lambda ()
                (setq fill-column 72) ; was 70
                (display-fill-column-indicator-mode 1)))))
;;; packages.el ends here
