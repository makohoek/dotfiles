#!/usr/bin/env bash

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# if we are emacs, don't do anything
[[ $TERM = "dumb" ]] && return

# Path to the bash it configuration
export BASH_IT="$HOME/.bash_it"

# Lock and Load a custom theme file
# location /.bash_it/themes/
export BASH_IT_THEME="$HOME/.bash_it_custom/makohoek.theme"

# (Advanced): Change this to the name of your remote repo if you
# cloned bash-it with a remote other than origin such as `bash-it`.
# export BASH_IT_REMOTE='bash-it'

# Don't check mail when opening terminal.
unset MAILCHECK

# Change this to your console based IRC client of choice.
export IRC_CLIENT='irssi'

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
export SHORT_TERM_LINE=true

# Load Bash It
source "$BASH_IT"/bash_it.sh


#######################
# additional settings #
#######################
# disable flow control (C-s and C-q) since I use tmux for that feature
stty -ixon

#################
# Shell Options #
#################
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


#########################
# Environment variables #
#########################
export EDITOR=vim
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'
export BAT_THEME="zenburn"
export BAT_STYLE="plain"
export TERM=screen-256color


#################
# History stuff #
#################
# don't put duplicate lines in the history.
# See bash(1) for more options
export HISTCONTROL=erasedups

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
export HISTSIZE=10000
export HISTFILESIZE=20000

# Use arrow keys to complete commands from history
bind '"\e[A"':history-search-backward
bind '"\e[B"':history-search-forward


##################
# Extra includes #
##################
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
source_if_exists ~/.fzf.bash
# remove the bindings fzf creates because i don't want them all
bind -r '\ec'
source_if_exists ~/.neovim.bash

## git completion in Arch
source_if_exists /usr/share/git/completion/git-prompt.sh


##########################
# PATH extra directories #
##########################
. "$HOME/.cargo/env"

export PATH="$HOME/bin:$PATH"
# Homebrew
export PATH="/usr/local/bin:$PATH"

# python local modules
export PATH="$HOME/.local/bin:$PATH"

#############
# Functions #
#############
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

# fasd
fasd_cache="$HOME/.fasd-init-bash"
if [ "$(command -v fasd)" -nt "$fasd_cache" -o ! -s "$fasd_cache" ]; then
    fasd --init posix-alias bash-hook bash-ccomp bash-ccomp-install >| "$fasd_cache"
fi
source "$fasd_cache"
unset fasd_cache

# fasd + fzf
alias j='cd $(z -l | fzf)'

# tab completion for jira
if [ "$(command -v jira)" ]; then
    eval "$(jira --completion-script-bash)"
fi

# ripgrep
export RIPGREP_CONFIG_PATH=~/.ripgreprc

####################
# Private includes #
####################
source_if_exists ~/work/work.bash
