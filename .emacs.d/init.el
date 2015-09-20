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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("3539b3cc5cbba41609117830a79f71309a89782f23c740d4a5b569935f9b7726" default)))
 '(whitespace-style
   (quote
    (face tabs trailing space-before-tab empty space-after-tab tab-mark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
