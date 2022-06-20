#!/usr/bin/env bash

# clone bash_it, a bash framework
# https://github.com/Bash-it/bash-it
git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it

# No need for running install, we already have a valid config in the dotfiles repo
# ~/.bash_it/install.sh

# install oh-my-zsh
[[ -d $HOME/.oh-my-zsh/ ]] || {
    echo "oh-my-zsh is not installed, installing ...";

    export RUNZSH='no'
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)";

    # Oh my zsh backs up any user related zshell files such as:
    # - .zshrc.pre-oh-my-zsh => .dotfiles/zsh/.zshrc.pre-oh-my-zsh
    # - .zshrc => .dotfiles/zsh/.zshrc
    # since we already have those as part of this repository, move them away to avoid
    # (rightful) complaints from stow
    mv ~/.zshrc ~/.zshrc.default
    mv ~/.zshrc.pre-oh-my-zsh ~/.zshrc.pre-oh-my-zsh.default
}


# TODO: emacs/neovim package install
