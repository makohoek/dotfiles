#!/usr/bin/env bash

# This should be ran PRIOR to running stow

command_exists() {
    command -v "$@" >/dev/null 2>&1
}

# install brew
command_exists brew || {
    echo "homebrew is not installed, installing ...";
    /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)";
}

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
