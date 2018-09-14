;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs-base
   dotspacemacs-enable-lazy-installation 'unused
   dotspacemacs-ask-for-lazy-installation t
   dotspacemacs-configuration-layer-path '("~/.spacemacs-layers/")
   dotspacemacs-configuration-layers
   '(
     ;; layers from spacemacs-all
     spacemacs-completion
     spacemacs-layouts
     spacemacs-org
     (spacemacs-ui-visual :packages (not neotree fancy-battery))
     (spacemacs-editing-visual :packages auto-highlight-symbol)
     ivy
     ;; additional spacemacs layers
     (auto-completion :variables
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t
                      :disabled-for org erc)
     git
     markdown
     (org :variables
          org-enable-reveal-js-support t)
     (shell :variables
        shell-default-height 30
        shell-default-position 'bottom)
     cscope
     c-c++
     emacs-lisp
     shell-scripts
     go
     docker
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     python
     javascript
     html
     osx
     vimscript
     yaml
     (mu4e :variables mu4e-account-alist t)
     ;; own, private layers
     makohoek-theme
     makohoek-dev
     makohoek-email
     makohoek-org
     makohoek-osx
     makohoek-work
     )
   dotspacemacs-additional-packages '(org-jira ox-reveal copy-as-format xclip realgud)
   dotspacemacs-frozen-packages '()
   dotspacemacs-excluded-packages '(helm-cscope rainbow-delimiters neotree company-mode smartparens linum-mode tern)
  dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  (setq-default
   dotspacemacs-elpa-https nil
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update 't
   dotspacemacs-elpa-subdirectory nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists '((recents . 10)
                                (projects . 10)
                                (bookmarks . 10))
   dotspacemacs-startup-buffer-responsive t
   dotspacemacs-scratch-mode 'text-mode
   dotspacemacs-themes '(zenburn
             spacegray
             base16-eighties
             spacemacs-dark
             solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Hack"
                   :size 12.0
                   :weight normal
                   :width ultra-condensed
                   :powerline-scale 1.0)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-command-key "SPC"
   dotspacemacs-ex-command-key ":"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-remap-Y-to-y$ nil
   dotspacemacs-retain-visual-state-on-shift t
   dotspacemacs-visual-line-move-text nil
   dotspacemacs-ex-substitute-global nil
   dotspacemacs-default-layout-name "Home"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-large-file-size 1
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-helm-use-fuzzy 'always
   dotspacemacs-enable-paste-transient-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar t
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-show-transient-state-title t
   dotspacemacs-show-transient-state-color-guide t
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling nil
   dotspacemacs-line-numbers nil
   dotspacemacs-folding-method 'evil
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-smart-closing-parenthesis nil
   dotspacemacs-highlight-delimiters 'current
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   dotspacemacs-emacs-command-key "SPC"
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  ;; additional files
  ;; don't pollute my spacemacs file, add it to a custom.el file
  (load "~/.config/spacemacs/custom")

  ;; work related stuff: do not report errors if file do not exist
  (load "~/intel-tools/spacemacs/makohoek-intel/proxy" 't)

  ;; stop warning about this!
  ;; If non-nil, warn if variables are being set in the wrong shell startup files.
  ;; Environment variables should be set in .profile or .zshenv rather than
  ;; .bashrc or .zshrc.
  (setq exec-path-from-shell-check-startup-files nil))

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."
  ;; never prompt, always follow symlinks
  (setq vc-follow-symlinks t)

  ;; enable cscope minor mode on startup in C and C++
  (cscope-setup)

  ;; tab indentation: default to userspace mode
  ;; see coding-style-kernel for switching to kernel style
  (setq-default tab-width 4)
  (setq-default indent-tabs-mode nil)

  ;; use smaller powerline seperator
  (setq powerline-default-separator 'bar)

  ;; coding style for kernel/userspace
  (defun coding-style-kernel()
    "Set coding style to tabs/tabwidth=8"
    (interactive)
    (setq-default tab-width 8)
    (setq-default indent-tabs-mode 't))
  (defun coding-style-userspace()
    "Set coding style to spaces/tabwidth=4"
    (interactive)
    (setq-default tab-width 4)
    (setq-default indent-tabs-mode nil))

  ;; call SPC bB with SPC bb
  (spacemacs/set-leader-keys "bb" 'spacemacs-layouts/non-restricted-buffer-list-ivy)

  ;; always prefer horizontal splitting to vertical
  (setq split-height-threshold nil)

  ;; fringe style: equivalent of "half-width"
  (set-fringe-style 4)

  (with-eval-after-load 'tramp
    (setq tramp-default-method "ssh")
    (setq tramp-completion-reread-directory-timeout nil)
    (setq tramp-verbose 1)
    (add-to-list 'tramp-remote-path "~/bin")
    (add-to-list 'exec-path "~/bin")
    ))
