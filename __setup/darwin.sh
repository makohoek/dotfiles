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
    sh -c "$(curl -fsSL https://raw.githubusercontent.com/ohmyzsh/ohmyzsh/master/tools/install.sh)";
}
