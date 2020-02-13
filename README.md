# makohoek's dotfiles

:dragon: here be dragons :dragon:

[![Build Status](https://travis-ci.org/makohoek/dotfiles.svg?branch=master)](https://travis-ci.org/makohoek/dotfiles)
[![Built with Spacemacs](https://cdn.rawgit.com/syl20bnr/spacemacs/442d025779da2f62fc86c2082703697714db6514/assets/spacemacs-badge.svg)](http://spacemacs.org)

## Prerequisite
My dotfiles are managed through GNU `stow`.
To install `stow`, do:

- For debian based:

```sh
sudo apt update && sudo apt install stow
```

- For MacOS:
```sh
brew install stow
```

## Installation
- Clone the repository:
```sh
git clone https://github.com/makohoek/dotfiles.git ~/.dotfiles
```

- Then, initialize the submodules:
```sh
cd ~/.dotfiles && git submodule update --init --recursive --remote --depth=1
```

- Then, use `stow` to create the symlinks:
```sh
cd ~/.dotfiles && setup.sh
```

## Post-install/more tools

- Spacemacs (`develop branch`)
```sh
git clone -b develop https://github.com/syl20bnr/spacemacs ~/.emacs.d
```

- Neovim (usually vim is enough for basics)
See https://github.com/neovim/neovim/releases/
Otherwise, minimum vim 8.0

## More information on managing dotfiles with GNU `stow`
- https://dev.to/writingcode/how-i-manage-my-dotfiles-using-gnu-stow-4l59
- https://alexpearce.me/2016/02/managing-dotfiles-with-stow
