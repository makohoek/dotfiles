;; Other packages source list for package-list
(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
  ("marmalade" . "http://marmalade-repo.org/packages/")
  ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

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
 '(custom-safe-themes
   (quote
    ("1abda075ebacaa3795d675bb2be0a905322ac856f9c0c259da63f9ccfe1962ec" "3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" "9f3a4edb56d094366afed2a9ba3311bbced0f32ca44a47a765d8ef4ce5b8e4ea" "75c0b1d2528f1bce72f53344939da57e290aa34bea79f3a1ee19d6808cb55149" default)))
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

;; remove scroll bar
(scroll-bar-mode -1)

;; enable ido mode
(ido-mode 1)

;; font
(add-to-list 'default-frame-alist '(font . "Inconsolata-10.7" ))
(set-face-attribute 'default nil :font "Inconsolata-10.7")

;; enable evil mode
(require 'evil)
(evil-mode 1)

;; similar to tpope's surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;; similar to Tom McDonald's exchange plugin
(require 'evil-exchange)
;;change default key bindings
(setq evil-exchange-key (kbd "gx"))
(evil-exchange-install)

;; magit log global mapping
(global-set-key (kbd "C-x g") 'magit-status)

;; fixup scroll-up for evil mode
(global-set-key (kbd "C-u") 'evil-scroll-up)

;; jumping around the splits with C-M-hjkl
(global-set-key (kbd "C-M-h") 'windmove-left)
(global-set-key (kbd "C-M-j") 'windmove-down)
(global-set-key (kbd "C-M-k") 'windmove-up)
(global-set-key (kbd "C-M-l") 'windmove-right)

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

;; magit/evil integration copy paste of:
;; jixiuf comment from Jul 18 on  https://github.com/magit/magit/issues/1968
(require 'magit)
(dolist (map (list
	      ;; Mode maps
	      magit-blame-mode-map
	      magit-cherry-mode-map
	      magit-diff-mode-map
	      magit-log-mode-map
	      magit-log-select-mode-map
	      magit-mode-map
	      ;; No evil keys for the popup.
	      ;; magit-popup-help-mode-map
	      ;; magit-popup-mode-map
	      ;; magit-popup-sequence-mode-map
	      magit-process-mode-map
	      magit-reflog-mode-map
	      magit-refs-mode-map
	      magit-revision-mode-map
	      magit-stash-mode-map
	      magit-stashes-mode-map
	      magit-status-mode-map
	      ;; Section submaps
	      magit-branch-section-map
	      magit-commit-section-map
	      magit-file-section-map
	      magit-hunk-section-map
	      magit-module-commit-section-map
	      magit-remote-section-map
	      magit-staged-section-map
	      magit-stash-section-map
	      magit-stashes-section-map
	      magit-tag-section-map
	      magit-unpulled-section-map
	      magit-unpushed-section-map
	      magit-unstaged-section-map
	      magit-untracked-section-map))
  ;; Move current bindings for movement keys to their upper-case counterparts.
  (dolist (key (list "k" "j" "h" "l"))
    (let ((binding (lookup-key map key)))
      (when binding
	(define-key map (upcase key) binding) (define-key map key nil))))
  (evil-add-hjkl-bindings map 'emacs
    (kbd "v") 'evil-visual-char
    (kbd "V") 'evil-visual-line
    (kbd "C-v") 'evil-visual-block
    (kbd "C-w") 'evil-window-map))
(dolist (mode (list 'magit-blame-mode
		    'magit-cherry-mode
		    'magit-diff-mode
		    'magit-log-mode
		    'magit-log-select-mode
		    'magit-mode
		    'magit-popup-help-mode
		    'magit-popup-mode
		    'magit-popup-sequence-mode
		    'magit-process-mode
		    'magit-reflog-mode
		    'magit-refs-mode
		    'magit-revision-mode
		    'magit-stash-mode
		    'magit-stashes-mode
		    'magit-status-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(put 'dired-find-alternate-file 'disabled nil)
