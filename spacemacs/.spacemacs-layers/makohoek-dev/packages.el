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
  '(bitbake
    ediff
    info
    undo-tree
    undo-fu
    ripgrep
    exec-path-from-shell
    whitespace)
  "The list of Lisp packages required by the makohoek-dev layer.")

(defun makohoek-dev/init-bitbake ()
  "Use bitbake for yocto developpment"
  (use-package bitbake))

(defun makohoek-dev/init-undo-fu ()
  "default undo for spacemacs, seems not included"
  (use-package undo-fu))

(defun makohoek-dev/init-undo-tree()
  (use-package undo-tree
    :config
    (global-undo-tree-mode)))

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

;; whitespace is owned by 'spacemacs-defaults' layer
(defun makohoek-dev/post-init-whitespace ()
  (use-package whitespace
    :config
    (setq-default whitespace-style
          '(face tabs newline indentation trailing tab-mark))
    (set-face-attribute 'whitespace-tab nil
                        :background nil :foreground "DimGray")
    (set-face-attribute 'whitespace-indentation nil
                        :background nil :foreground "DimGray")
    ;; change tab displayed char to »
    ;; 9:tab, 187:»
    (setq whitespace-display-mappings
          '((tab-mark 9 [187 9] [92 9])))
    ;; align with kernel length for line length
    (setq whitespace-line-column 100)
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

(defun makohoek-dev/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :config
    (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
      (add-to-list 'exec-path-from-shell-variables var))
    (exec-path-from-shell-initialize)))

;;; packages.el ends here

