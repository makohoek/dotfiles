;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-term-color-vector
   [unspecified "#2d2d2d" "#f2777a" "#99cc99" "#ffcc66" "#6699cc" "#cc99cc" "#6699cc" "#d3d0c8"] t)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   (quote
    ("ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "6fc0ae7cc2abd82d8add1140874ccf8773feaaae73a704981d52fdf357341038" "25c242b3c808f38b0389879b9cba325fb1fa81a0a5e61ac7cae8da9a32e2811b" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "d8f76414f8f2dcb045a37eb155bfaa2e1d17b6573ed43fb1d18b936febc7bbc2" "f245c9f24b609b00441a6a336bcc556fe38a6b24bfc0ca4aedd4fe23d858ba31" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "b6db49cec08652adf1ff2341ce32c7303be313b0de38c621676122f255ee46db" default)))
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   (quote
    (("#eee8d5" . 0)
     ("#B4C342" . 20)
     ("#69CABF" . 30)
     ("#69B7F0" . 50)
     ("#DEB542" . 60)
     ("#F2804F" . 70)
     ("#F771AC" . 85)
     ("#eee8d5" . 100))))
 '(hl-bg-colors
   (quote
    ("#DEB542" "#F2804F" "#FF6E64" "#F771AC" "#9EA0E5" "#69B7F0" "#69CABF" "#B4C342")))
 '(hl-fg-colors
   (quote
    ("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3")))
 '(lt-serial-default-port "ttyUSB1")
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(package-selected-packages
   (quote
    (copy-as-format go-guru go-eldoc go-mode yapfify xterm-color xcscope which-key wgrep web-mode web-beautify use-package toc-org tagedit spaceline spacegray-theme smex smeargle slim-mode shell-pop seoul256-theme scss-mode sass-mode pyvenv pytest pyenv-mode py-isort pug-mode popwin pip-requirements persp-mode pcre2el ox-reveal orgit org-projectile org-present org-pomodoro org-jira org-download org-bullets multi-term mmm-mode markdown-toc magit-gitflow macrostep livid-mode live-py-mode less-css-mode json-mode js2-refactor js-doc ivy-hydra insert-shebang hy-mode htmlize hl-todo help-fns+ helm-make golden-ratio gnuplot gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md flyspell-correct-ivy flx fish-mode fill-column-indicator eyebrowse exec-path-from-shell evil-visualstar evil-magit evil-escape eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav dtrt-indent disaster cython-mode counsel-projectile coffee-mode cmake-mode clang-format bind-map base16-theme auto-highlight-symbol auto-dictionary auto-compile anaconda-mode ag ace-window)))
 '(paradox-github-token t)
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#c85d17")
     (60 . "#be730b")
     (80 . "#b58900")
     (100 . "#a58e00")
     (120 . "#9d9100")
     (140 . "#959300")
     (160 . "#8d9600")
     (180 . "#859900")
     (200 . "#669b32")
     (220 . "#579d4c")
     (240 . "#489e65")
     (260 . "#399f7e")
     (280 . "#2aa198")
     (300 . "#2898af")
     (320 . "#2793ba")
     (340 . "#268fc6")
     (360 . "#268bd2"))))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   (quote
    (unspecified "#fdf6e3" "#eee8d5" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#657b83" "#839496")))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
