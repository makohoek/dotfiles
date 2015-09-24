;; Other packages source list for package-list
(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
  ("marmalade" . "http://marmalade-repo.org/packages/")
  ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(require 'whitespace)

;; Theme specific stuff
;; use cursorline as in vim
(global-hl-line-mode)
;; colorscheme
(load-theme 'base16-eighties-dark)

;; remove scroll bar
(scroll-bar-mode -1)

;; enable ido mode
(ido-mode 1)

;; font
(add-to-list 'default-frame-alist '(font . "Inconsolata-12" ))
(set-face-attribute 'default t :font "Inconsolata-12" )
(set-face-attribute 'default nil :height 105)

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

;; powerline setup to show evil status at the left
(require 'powerline-evil)
(powerline-evil-vim-color-theme)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   (quote
    ("3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" default)))
 '(menu-bar-mode nil)
 '(tool-bar-mode nil)
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab empty space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
