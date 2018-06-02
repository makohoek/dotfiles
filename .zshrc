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

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
# export MANPATH="/usr/local/man:$MANPATH"

source /usr/local/share/chruby/chruby.sh

export EDITOR=vim

# Setting for the new UTF-8 terminal support in Lion
export LC_ALL=fr_FR.UTF-8
export LC_CTYPE=fr_FR.UTF-8

# par variable from manual
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'

# for swi prolog
PATH="$PATH:/Applications/SWI-Prolog.app/Contents/MacOS"

# for pdflatex
PATH="$PATH:/usr/local/texlive/2015/bin/universal-darwin/"

alias nvi="~/dotfiles/neovim_helpers/open_file_in_left_split.py"
alias gcommit="~/dotfiles/neovim_helpers/run_fugitive_commit.py"
alias nvim='NVIM_LISTEN_ADDRESS=/tmp/nvim nvim'
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

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
