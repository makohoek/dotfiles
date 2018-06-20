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

export EDITOR=vim

# par variable from manual
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'

# for swi prolog
PATH="$PATH:/Applications/SWI-Prolog.app/Contents/MacOS"

# for pdflatex
PATH="$PATH:/usr/local/texlive/2015/bin/universal-darwin/"

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

# great trick from:
# http://yazgoo.github.io/blag/neovim/terminal/multiplexer/tmux/2017/11/29/neovim-one-week-without-tmux.html
function cd() {
    builtin cd "$@";
    # if the parent process is nvim, do a vim cd
    (ps -o comm= $PPID | grep nvim > /dev/null) && \
        nvr --remote-send "<esc>:tcd $@<cr>i"
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

[ -f work/.work.zsh ] && source work/.work.zsh
