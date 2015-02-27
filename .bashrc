# If not running interactively, don't do anything
[ -z "$PS1" ] && return


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
HISTCONTROL=ignoreboth

# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=2000

# Extra includes
######################################################################



# Bash prompt with git
if [ -f ~/.bash_git_prompt ]; then
    . ~/.bash_git_prompt
else
    echo ".bash_git_prompt is not available!"
fi

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
else
    echo ".bash_aliases is not available!"
fi

# chruby for easy ruby version changing
if [ -f /usr/local/share/chruby/chruby.sh ]; then
    . /usr/local/share/chruby/chruby.sh
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

# Functions
######################################################################

soundalert()
{
    cvlc --play-and-exit ~/Musique/notifications/Bell\ of\ Victory.mp3 >& /dev/null
}

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

# Private includes
######################################################################

if [ -f ~/.bash_work ]; then
    . ~/.bash_work
fi

