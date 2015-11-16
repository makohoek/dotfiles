;; Other packages source list for package-list
(require 'package)

(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))

(setq package-enable-at-startup nil)

;; Auto install not installed packages
(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; Make sure to have downloaded archive description.
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

;; Activate installed packages
(package-initialize)

;; List of packages which should be fetched from elpa/melpa
(ensure-package-installed
 'base16-theme
 'evil
 'evil-exchange
 'evil-leader
 'evil-magit
 'evil-surround
 'evil-visualstar
 'fiplr
 'magit
 'powerline-evil
 'multi-term
 'git-gutter+
)


;; For manual packages which are not on elpa/melpa
(add-to-list 'load-path "~/.emacs.d/custom-submodules")
(let ((default-directory "~/.emacs.d/custom-submodules"))
  (normal-top-level-add-subdirs-to-load-path))

;; do not show ugly emacs UI menu bar
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(indent-tabs-mode nil)
 '(menu-bar-mode nil)
 '(tool-bar-mode nil))

(require 'whitespace)
(setq whitespace-style
   (quote (face trailing indentation::tab space-before-tab empty space-after-tab tab-mark)))

;; Theme specific stuff
;; use cursorline as in vim
(global-hl-line-mode)
;; colorscheme
(load-theme 'base16-tomorrow-dark t)

;; no splash screen
(setq inhibit-splash-screen t)

;; remove scroll bar
(scroll-bar-mode -1)

;; enable ido mode
(ido-mode 1)

;; font
(add-to-list 'default-frame-alist '(font . "Inconsolata-10.7" ))
(set-face-attribute 'default nil :font "Inconsolata-10.7")

;; similar to tpope's surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; similar to Tom McDonald's exchange plugin
(require 'evil-exchange)
;;change default key bindings
(setq evil-exchange-key (kbd "gx"))
(evil-exchange-install)

;; fixup scroll-up for evil mode
(global-set-key (kbd "C-u") 'evil-scroll-up)

;; jumping around the splits with C-M-hjkl
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

;; powerline setup to show evil status at the left
(require 'powerline-evil)
(powerline-evil-vim-color-theme)

;; virtual desktops
(require 'virtual-desktops)
(setq virtual-desktops-display-mode-line nil)
(virtual-desktops-mode 1)

;; create default virtual desktops and switch back to first
(dotimes (i 4)
  (virtual-desktops-add 1))
(virtual-desktops-goto 1)

;; bindings for virtual desktops
(global-set-key (kbd "M-<left>") (lambda () (interactive) (virtual-desktops-prev) (status-update)))
(global-set-key (kbd "M-<right>") (lambda () (interactive) (virtual-desktops-next) (status-update)))

;; statusbar in echo area
(require 'cl) ;; sadly, status depends on cl but does not require it :(
(require 'status)
(status-add-to-right 'status-date)
(status-add-to-left 'status-virtual-desktops)
(turn-on-status)

;; log tools for android development
(require 'log-tools)
(require 'lt-serial-kernel)
(require 'lt-logcat)
(require 'lt-serial)

;; magit/evil integration
(require 'magit)
(require 'evil-magit)

; gnus rebindings
; group mode
; TODO: add window controls C-W by default
(eval-after-load 'gnus
  '(progn
     (defvar gnus-group-mode-map)
     (evil-make-overriding-map gnus-group-mode-map 'normal)
     (evil-define-key 'normal gnus-group-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "RET" 'gnus-topic-select-group)
     (evil-set-initial-state 'gnus-group-mode 'normal)
     ))

; summary mode
(eval-after-load 'gnus
  '(progn
     (defvar gnus-summary-mode-map)
     (evil-make-overriding-map gnus-summary-mode-map 'normal)
     (evil-define-key 'normal gnus-summary-mode-map
       "j" 'evil-next-line
       "k" 'evil-previous-line
       "RET" 'gnus-summary-scroll-up)
     (evil-set-initial-state 'gnus-summary-mode 'normal)
     ))

;; magit push gerrit
(defun magit-push-gerrit (branch remote &optional remote-branch args)
  "Push a branch to gerrit"
  (interactive (magit-push-read-args t))
  (magit-run-git-async-no-revert
   "push" "-v" args remote
   (if remote-branch
       (format "%s:refs/for/%s" branch remote-branch)
     branch)))

(magit-define-popup-action 'magit-push-popup
  ?g "Gerrit" 'magit-push-gerrit)

;; escape quits everything
;; esc quits
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

(require 'evil)
(evil-mode t)

;; evil leader
(global-evil-leader-mode)
(evil-leader/set-leader "SPC")
(setq evil-leader/in-all-states 1)

;; leader related keybidings
(evil-leader/set-key "x" 'execute-extended-command)
(evil-leader/set-key "gl" 'magit-log-all-branches)
(evil-leader/set-key "gs" 'magit-status)
(evil-leader/set-key "gr" 'global-git-gutter+-mode)
(evil-leader/set-key "h" 'help)
(evil-leader/set-key "b" 'ido-switch-buffer)
(evil-leader/set-key "ff" 'fiplr-find-file)
(evil-leader/set-key "fd" 'fiplr-find-directory)

;; git gutter maps
(eval-after-load 'git-gutter+
  '(progn
     ;;; Jump between hunks
     (define-key git-gutter+-mode-map (kbd "]c") 'git-gutter+-next-hunk)
     (define-key git-gutter+-mode-map (kbd "[c") 'git-gutter+-previous-hunk)))

;; j/k for browsing wrapped lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

;; word delimiting like in vim: _ is part of a word
;; this fixes * and yiw behaviour for instance
;; https://bitbucket.org/lyro/evil/wiki/Home
(modify-syntax-entry ?_ "w")

(put 'dired-find-alternate-file 'disabled nil)
