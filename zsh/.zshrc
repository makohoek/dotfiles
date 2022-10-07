# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="lambda"

DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git colored-man-pages autojump)

# Oh my zsh
source $ZSH/oh-my-zsh.sh

######################
# User configuration #
######################
# disable flow control (C-s and C-q) since I use tmux for that feature
stty -ixon


#########################
# Environment variables #
#########################
export EDITOR=vim
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'
export BAT_THEME="zenburn"
export BAT_STYLE="plain"
export DISPLAY=:0
export TERM=screen-256color


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

source_if_exists ~/.fzf.zsh
# remove the bindings fzf creates because i don't want them all
bindkey -r '\ec'


##########################
# PATH extra directories #
##########################
export PATH="$HOME/.cargo/bin:$PATH"
# Homebrew
export PATH="/usr/local/bin:$PATH"

# ~/.local/bin
export PATH="$HOME/.local/bin:$PATH"

# beta testing of b4 submit
alias b4='/home/mkorpershoek/work/upstream/b4/b4.sh'

####################
# Private includes #
####################
source_if_exists ~/work/work.zsh
