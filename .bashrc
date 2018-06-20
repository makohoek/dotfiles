#!/usr/bin/env bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# Path to the bash it configuration
export BASH_IT="$HOME/.bash_it"

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME="$HOME/.bash_it_custom/makohoek.theme"

# (Advanced): Change this to the name of your remote repo if you
# cloned bash-it with a remote other than origin such as `bash-it`.
# export BASH_IT_REMOTE='bash-it'

# Your place for hosting Git repos. I use this for private repos.
export GIT_HOSTING='git@git.domain.com'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

# Set this to the command you use for todo.txt-cli
export TODO="t"

# Set this to false to turn off version control status checking within the prompt for all themes
export SCM_CHECK=true

# Set Xterm/screen/Tmux title with only a short hostname.
# Uncomment this (or set SHORT_HOSTNAME to something else),
# Will otherwise fall back on $HOSTNAME.
#export SHORT_HOSTNAME=$(hostname -s)

# Set Xterm/screen/Tmux title with only a short username.
# Uncomment this (or set SHORT_USER to something else),
# Will otherwise fall back on $USER.
#export SHORT_USER=${USER:0:8}

# Set Xterm/screen/Tmux title with shortened command and directory.
# Uncomment this to set.
#export SHORT_TERM_LINE=true

# Set vcprompt executable path for scm advance info in prompt (demula theme)
# https://github.com/djl/vcprompt
#export VCPROMPT_EXECUTABLE=~/.vcprompt/bin/vcprompt

# (Advanced): Uncomment this to make Bash-it reload itself automatically
# after enabling or disabling aliases, plugins, and completions.
# export BASH_IT_AUTOMATIC_RELOAD_AFTER_CONFIG_CHANGE=1

# Uncomment this to make Bash-it create alias reload.
# export BASH_IT_RELOAD_LEGACY=1

# Load Bash It
source "$BASH_IT"/bash_it.sh

# additional settings
# disable flow control (C-s and C-q) since I use tmux for that feature
stty -ixon

# Shell Options
######################################################################

# append to the history file, don't overwrite it
shopt -s histappend

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# ignore case
shopt -s nocaseglob

# correct file typos
shopt -s cdspell

# do host completion when encountering a @ command
shopt -s hostcomplete

# allow extended globs such as !()
shopt -s extglob

# Environment variables
######################################################################
export EDITOR=vim
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'

# History stuff
######################################################################

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# Extra includes
######################################################################

function source_if_exists()
{
    local file_to_source="$1"
    if [ -f "$file_to_source" ]; then
        source $file_to_source
    else
        >&2 echo "$file_to_source is not available!"
    fi
}

source_if_exists ~/.bash_aliases
source_if_exists /etc/bash_completion

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
# remove the bindings fzf creates because i don't want them all
bind -r '\ec'

export PATH="$HOME/.cargo/bin:$PATH"

# Functions
######################################################################

# Colored man
# see man termcap for the variables
man() {
    env \
        LESS_TERMCAP_mb=$(printf "\e[1;31m") \
        LESS_TERMCAP_md=$(printf "\e[1;34m") \
        LESS_TERMCAP_me=$(printf "\e[0m") \
        LESS_TERMCAP_se=$(printf "\e[0m") \
        LESS_TERMCAP_so=$(printf "\e[1;41;27m") \
        LESS_TERMCAP_ue=$(printf "\e[0m") \
        LESS_TERMCAP_us=$(printf "\e[1;32m") \
        man "$@"
}

cscope_generate_files()
{
    find $(pwd) -name '*.c' -o -name '*.h' -o -name '*.cpp' -o -name '*.hpp' > cscope.files
}

cscope_rebuild_index()
{
    cscope -b -q -k
}

# for git dotfiles in ~/
# https://developer.atlassian.com/blog/2016/02/best-way-to-store-dotfiles-git-bare-repo/
function git() {
    if [[ "$PWD" == "$HOME" ]]; then
        /usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME "$@"
    else
        /usr/bin/git "$@"
    fi
}
# alias config='/usr/bin/git --git-dir=$HOME/.cfg/ --work-tree=$HOME'

# great trick from:
# http://yazgoo.github.io/blag/neovim/terminal/multiplexer/tmux/2017/11/29/neovim-one-week-without-tmux.html
function cd() {
    builtin cd "$@";
    # do a vim :tcd if we managed to cd and we are withing neovim
    if [[ -n ${NVIM_LISTEN_ADDRESS} ]]; then
        nvr --remote-send "<esc>:tcd $@<cr>i"
    fi
}
export cd

function e() {
    nvr --remote "$@"
}

function split() {
    nvr -o "$@"
}

function vsplit() {
    nvr -O "$@"
}

# Private includes
######################################################################
source_if_exists  ~/work/.work.bash
