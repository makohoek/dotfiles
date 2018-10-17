;;; packages.el --- makohoek-dev layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Mattijs Korpershoek
;;
;; Author:  <mattijs.korpershoek@gmail.com>
;; URL: https://github.com/Makohoek/dotfiles
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `makohoek-dev-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `makohoek-dev/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `makohoek-dev/pre-init-PACKAGE' and/or
;;   `makohoek-dev/post-init-PACKAGE' to customize the package as it is loaded.


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
  (add-hook 'c-mode-common-hook
            (lambda ()
              (require 'dtrt-indent)
              (dtrt-indent-mode t))))

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
       (let ((source (magit-read-local-branch-or-commit "Push")))
         (list source
               (magit-read-remote-branch (format "Push %s to" source) nil
                                         (magit-get-upstream-branch source)
                                         source 'confirm)
               (magit-push-arguments))))
      (magit-git-push-gerrit source target args))

    (magit-define-popup-action 'magit-push-popup
      ?g "gerrit" 'magit-push-gerrit)
  ))

;; ediff is owned by 'spacemacs-base' layer
(defun makohoek-dev/post-init-whitespace ()
  ;; whitespace mode
  (with-eval-after-load 'whitespace
    (setq whitespace-style '(face spaces tabs newline indentation trailing tab-mark))
    ;; for tabs in between lines
    (set-face-attribute 'whitespace-tab nil :background nil
                        :foreground "DimGray")
    ;; for tabs as indentation
    (set-face-attribute 'whitespace-indentation
                        nil :background nil
                        :foreground "DimGray")
    ;; change tab displayed char to »
    (setq whitespace-display-mappings '((tab-mark 9
                                                  [187 9]
                                                  [92 9]) ; 9:tab, 187:»
                                        ))
    ;; enable whitespace mode in C and Cpp
    (add-hook 'c-mode-hook (function whitespace-mode))
    (add-hook 'c++-mode-hook (function whitespace-mode))
    ;; enable whitespace mode in elisp
    (add-hook 'emacs-lisp-mode-hook (function whitespace-mode))
    ;; enable whitespace mode in go
    (add-hook 'go-mode-hook (function whitespace-mode))
    ;; enable whitespace mode in makefile mode
    (add-hook 'makefile-mode-hook (function whitespace-mode))
    ;; enable whitespace mode in python
    (add-hook 'python-mode-hook (function whitespace-mode))))

;; ediff is owned by 'spacemacs-base' layer
(defun makohoek-dev/post-init-ediff ()
  ;; ediff customization: show char based diff
  (with-eval-after-load 'ediff
    (setq-default ediff-forward-word-function
                  'forward-char)))

;; xcscope is owned by 'cscope' layer
(defun makohoek-dev/post-init-xcscope()
  (with-eval-after-load 'xcscope
    ;; The -q option in cscope: use an inverted database index. Takes
    ;; longer to build, but results in faster lookups. Useful for very
    ;; large codebases
    (setq cscope-option-use-inverted-index t)))

;;; packages.el ends here

