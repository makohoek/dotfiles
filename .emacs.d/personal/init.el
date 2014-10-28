;;; Code:
; Theme related settings
(load-theme 'solarized-light t)

; Thanks to http://juanjoalvarez.net/es/detail/2014/sep/19/vim-emacsevil-chaotic-migration-guide/
; for his guide. This helped my a lot for starting with emacs

;;;;;;;;;;;;;;;;;;;;;;;;;
; Evil related settings ;
;;;;;;;;;;;;;;;;;;;;;;;;;
; leader should always be activated
; (setq evil-leader/in-all-states 1)
; leader settings for evil
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

; Search highlight as in vim
; color scheme is not really respected, sadly
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)

; Clean search highlighting shortcut
(evil-leader/set-key "," 'evil-search-highlight-persist-remove-all)

(evil-leader/set-key "o" 'newline-and-indent)

;;; Quit everything with Escape
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(provide 'init)
;;; init.el ends here
