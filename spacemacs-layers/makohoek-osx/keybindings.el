;; system clipboard copy/paste feature
(spacemacs/declare-prefix "o" "clipboard functions")
(spacemacs/set-leader-keys "o y" 'copy-to-clipboard)
(spacemacs/set-leader-keys "o p" 'paste-from-clipboard)
