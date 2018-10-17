# Makohoek's dotfiles

:dragon: here be dragons :dragon:

## Prerequisite
My dotfiles are managed through `rcm`.
To install `rcm`, visit https://github.com/thoughtbot/rcm#installation


## Installation
- Clone the repository:
```sh
git clone https://github.com/Makohoek/dotfiles.git ~/.dotfiles
```

- Then, initialize the submodules:
```sh
cd ~/.dotfiles && git submodule update --init --recursive --remote --depth=1
```

- Then, use `rcm` to create the symlinks:
```sh
cd && env RCRC=$HOME/.dotfiles/rcrc rcup
```

## Post-install/more tools

- Spacemacs (`develop branch`)
```sh
git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
```sh

- Neovim (usually vim is enough for basics)
See https://github.com/neovim/neovim/releases/

## More information on `rcm`
- http://thoughtbot.github.io/rcm/rcm.7.html
- https://robots.thoughtbot.com/rcm-for-rc-files-in-dotfiles-repos

[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)
