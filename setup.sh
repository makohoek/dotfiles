#!/usr/bin/env bash
# shamelessy stolen from:
# https://dev.to/writingcode/how-i-manage-my-dotfiles-using-gnu-stow-4l59

# make sure we have pulled in and updated any submodules
git submodule init
git submodule update

# what directories should be installable by all machines
base=(
    git
    spacemacs
    vim
    tmux
    email
    misc
)

darwin_only=(
    zsh
    darwin
)

linux_only=(
    bash
    i3
    linux
)

# run the stow command for the passed in directory ($2) in location $1
stowit() {
    local usr
    local app
    usr=$1
    app=$2
    stow --verbose --restow --target=${usr} ${app}
}

echo ""
echo "Stowing apps for user: ${whoami}"

# install apps available to local users
for app in ${base[@]}; do
    stowit "${HOME}" $app
done

echo ""
echo "Stowing apps for $(uname)"

# install only user for darwin (MacOS)
if [[ $(uname) = 'Darwin' ]]; then
    for app in ${darwin_only[@]}; do
        stowit "${HOME}" $app
    done
fi

# install only for Linux
if [[ $(uname) = 'Linux' ]]; then
    for app in ${linux_only[@]}; do
        stowit "${HOME}" $app
    done
fi

echo ""
echo "##### ALL DONE"
