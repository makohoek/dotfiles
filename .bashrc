# If not running interactively, don't do anything
[ -z "$PS1" ] && return

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
export VISUAL=vim
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'

# History stuff
######################################################################

# don't put duplicate lines or lines starting with space in the history.
# See bash(1) for more options
HISTCONTROL=ignoreboth:erasedups

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=10000
HISTFILESIZE=20000

# Save command history before each prompt
export PROMPT_COMMAND='history -a'

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

# this prompt must be sourced to use __git_ps1
if [[ $TERM != "eterm-color" ]] && [[ $TERM != "dumb" ]]; then
    source_if_exists /usr/share/git-core/contrib/completion/git-prompt.sh
    source_if_exists ~/.bash_git_prompt
fi
source_if_exists ~/.bash_aliases
source_if_exists /usr/local/share/chruby/chruby.sh
source_if_exists /etc/bash_completion
source_if_exists ~/dotfiles/bin/shell_bookmarks.sh

# Functions
######################################################################

# Alert alias for long running commands.
# Usage example: sleep 10; alert
alert()
{
    icon="$( [ $? = 0 ] && echo terminal || echo error )"
    text="$( history | tail -n1 | sed 's/^\s*[0-9]\+\s*//' | sed 's/[;&|]\s*alert$//' )"
    notify-send --urgency=low --icon "$icon" "$text"
}

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

# Private includes
######################################################################
source_if_exists  ~/.bash_work

[ -f ~/.fzf.bash ] && source ~/.fzf.bash
