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
plugins=(git colored-man-pages)

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
export PATH="$HOME/bin:$PATH"
# for swi prolog
PATH="$PATH:/Applications/SWI-Prolog.app/Contents/MacOS"
# for pdflatex
PATH="$PATH:/usr/local/texlive/2015/bin/universal-darwin/"


##########
# Neovim #
##########
# great trick from:
# http://yazgoo.github.io/blag/neovim/terminal/multiplexer/tmux/2017/11/29/neovim-one-week-without-tmux.html
function cd() {
    builtin cd "$@";
    # do a vim :tcd if we managed to cd and we are withing neovim
    if [[ -n ${NVIM_LISTEN_ADDRESS} ]]; then
        neovim-cmd cd "$@"
    fi
}
export cd

function e() {
    if [[ -n "$NVIM_LISTEN_ADDRESS" ]]; then
        neovim-cmd edit "$@"
    else
        vim "$@"
    fi
}

function split() {
    if [[ -n "$NVIM_LISTEN_ADDRESS" ]]; then
        neovim-cmd edit "$@"
    else
        vim "$@"
    fi
}

function vsplit() {
    if [[ -n "$NVIM_LISTEN_ADDRESS" ]]; then
        neovim-cmd edit "$@"
    else
        vim "$@"
    fi
}

# no nested nvim instances
if [[ -n "$NVIM_LISTEN_ADDRESS" ]]; then
    if [[ -x "$(command -v neovim-cmd)" ]]; then
        alias nvim='neovim-cmd edit'
        alias vi='neovim-cmd edit'
        alias vim='neovim-cmd edit'
    else
        alias nvim='echo "No nesting of vim!"'
        alias vi='echo "No nesting of vim!"'
        alias vim='echo "No nesting of vim!"'
    fi
fi


####################
# Private includes #
####################
source_if_exists ~/work/work.zsh
