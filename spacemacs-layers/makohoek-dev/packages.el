;;; packages.el --- makohoek-dev layer packages file for Spacemacs.
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

(defconst makohoek-dev-packages
  '(dtrt-indent
    ediff
    info
    hide-lines
    magit
    forge
    dts-mode
    ripgrep
    whitespace)
  "The list of Lisp packages required by the makohoek-dev layer.")

(defun makohoek-dev/init-dtrt-indent ()
  "Enable dtrt-indent for c development."
  (use-package dtrt-indent
    :hook ((c-mode-common . dtrt-indent-mode)
           (dts-mode . dtrt-indent-mode))))

(defun makohoek-dev/init-hide-lines ()
  (use-package hide-lines
    :commands hide-lines))

(defun makohoek-dev/init-dts-mode()
  (use-package dts-mode))

;; nothing to configure. We still need to init it so that
;; it is available
(defun makohoek-dev/init-ripgrep ()
  (use-package ripgrep
    :config (spacemacs/set-leader-keys "srr" 'ripgrep-regexp)))

(defun makohoek-dev/init-info()
  (use-package info
    :custom
    ;; add to list of directories to search for Info documentation files.
    ;; to add libc:
    ;; $ cd ~/info
    ;; $ wget https://www.gnu.org/software/libc/manual/info/libc-info.tar.gz
    ;; $ tar zxvf libc-info.tar.gz
    ;; $ install-info --info-dir=/home/julienm/info/ /home/julienm/info/libc.info
    (Info-additional-directory-list '("~/.info"))))

(defun makohoek-dev/init-forge ()
  (use-package forge
    :after magit))

;; magit is owned by layer 'git'
(defun makohoek-dev/post-init-magit ()
  (when (configuration-layer/package-used-p 'magit)
    (spacemacs|use-package-add-hook magit
      :post-config
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
      (add-to-list 'git-commit-known-pseudo-headers "Change-Id"))))

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
     (python-mode     . whitespace-mode)
     (yaml-mode       . whitespace-mode)
     (dts-mode        . whitespace-mode))))

;; ediff is owned by 'spacemacs-base' layer
(defun makohoek-dev/post-init-ediff ()
  (use-package ediff
    :defer t
    :config
    ;; ediff customization: show char based diff
    (setq-default ediff-forward-word-function 'forward-char)))

;; xcscope is owned by 'cscope' layer
(defun makohoek-dev/post-init-xcscope()
  (use-package xcscope
    :custom
    (cscope-option-use-inverted-index t)))
;;; packages.el ends here

