;;; packages.el --- makohoek-dev layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Mattijs Korpershoek
;;
;; Author: Mattijs Korpershoek <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/Makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
;;; Code:

(defconst makohoek-dev-packages '(ag
                                  ediff
                                  info
                                  dtrt-indent
                                  magit
                                  ripgrep
                                  whitespace
                                  )
  "The list of Lisp packages required by the makohoek-dev layer.")

(defun makohoek-dev/init-dtrt-indent ()
  "Enable dtrt-indent for c development."
  (use-package dtrt-indent
    :hook (c-mode-common . dtrt-indent-mode)))

;; nothing to configure. We still need to init it so that
;; it is available
(defun makohoek-dev/init-ag ()
  (use-package ag))

;; nothing to configure. We still need to init it so that
;; it is available
(defun makohoek-dev/init-ripgrep ()
  (use-package ripgrep
    :config (spacemacs/set-leader-keys "srr" 'ripgrep-regexp)))

(defun makohoek-dev/init-info()
  (use-package info
    :config
    ;; add to list of directories to search for Info documentation files.
    ;; to add libc:
    ;; $ cd ~/info
    ;; $ wget https://www.gnu.org/software/libc/manual/info/libc-info.tar.gz
    ;; $ tar zxvf libc-info.tar.gz
    ;; $ install-info --info-dir=/home/julienm/info/ /home/julienm/info/libc.info
    (customize-set-variable 'Info-additional-directory-list '("~/info"))))

;; magit is owned by layer 'git'
(defun makohoek-dev/post-init-magit ()
  (with-eval-after-load 'magit
    ;; performance tricks for magit (useful in kernel tree)
    ;; remove tag entry for magit status
    (setq magit-status-headers-hook (remove 'magit-insert-tags-header magit-status-headers-hook))
    ;; remove revision header in magit-diff
    (setq magit-revision-sections-hook (remove 'magit-insert-revision-headers magit-revision-sections-hook))
    ;; don't display --graph on magit-log
    (setq magit-log-arguments '("-n30" "--decorate"))
    ;; signed-off by default
    (setq magit-commit-arguments (quote ("--signoff")))
    (setq magit-revert-arguments (quote ("--signoff")))
    ;; better rebase
    (setq magit-rebase-arguments (quote ("--autosquash" "--autostash")))
    ;; title must be no longer than 50
    (setq git-commit-summary-max-length 50)
    ;; line must not be longer than 72
    (setq git-commit-fill-column 72)
    ;; support Tracked-On: + Change-Id: pseudo-headers
    (add-to-list 'git-commit-known-pseudo-headers "Tracked-On")
    (add-to-list 'git-commit-known-pseudo-headers "Change-Id")

    ;; magit push gerrit
    ;; from JM-config: https://github.com/JulienMasson/jm-config
    (defun magit-git-push-gerrit (branch target args)
      (run-hooks 'magit-credential-hook)
      (-let [(remote . target)
             (magit-split-branch-name target)]
        (magit-run-git-async "push" "-v" args remote
                             (format "%s:refs/for/%s" branch target))))

    (defun magit-push-gerrit (source target args)
      "Push an arbitrary branch or commit somewhere.
  Both the source and the target are read in the minibuffer."
      (interactive
       (let ((source (magit-read-local-branch-or-commit "Gerrit")))
         (list source
               (magit-read-remote-branch (format "Push %s to" source) nil
                                         (magit-get-upstream-branch source)
                                         source 'confirm)
               (magit-push-arguments))))
      (magit-git-push-gerrit source target args))

    (magit-define-popup-action 'magit-push-popup
      ?g "gerrit" 'magit-push-gerrit)
  ))

;; whitespace is owned by 'spacemacs-defaults' layer
(defun makohoek-dev/post-init-whitespace ()
  (use-package whitespace
    :config
    (setq whitespace-style
          '(face spaces tabs newline indentation trailing tab-mark))
    (set-face-attribute 'whitespace-tab nil
                        :background nil :foreground "DimGray")
    (set-face-attribute 'whitespace-indentation nil
                        :background nil :foreground "DimGray")
    ;; change tab displayed char to »
    ;; 9:tab, 187:»
    (setq whitespace-display-mappings
          '((tab-mark 9 [187 9] [92 9])))
    :hook
    ((c-mode-common   . whitespace-mode)
     (emacs-lisp-mode . whitespace-mode)
     (makefile-mode   . whitespace-mode)
     (python-mode     . whitespace-mode))))

;; ediff is owned by 'spacemacs-base' layer
(defun makohoek-dev/post-init-ediff ()
  (use-package ediff
    :config
    ;; ediff customization: show char based diff
    (setq-default ediff-forward-word-function 'forward-char)))

;; xcscope is owned by 'cscope' layer
(defun makohoek-dev/post-init-xcscope()
  (use-package xcscope
    :config
    ;; The -q option in cscope: use an inverted database index. Takes
    ;; longer to build, but results in faster lookups. Useful for very
    ;; large codebases
    (setq cscope-option-use-inverted-index t)))
;;; packages.el ends here

