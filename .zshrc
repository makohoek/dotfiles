# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="robbyrussell"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git colored-man)

# Oh my zsh
source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin"
# export MANPATH="/usr/local/man:$MANPATH"
#
source $ZSH/oh-my-zsh.sh
source /usr/local/share/chruby/chruby.sh

export EDITOR=vim

# par variable from manual
export PARINIT='rTbgqR B=.,?_A_a Q=_s>|'

alias nvi="~/dotfiles/neovim_helpers/open_file_in_left_split.py"
alias gcommit="~/dotfiles/neovim_helpers/run_fugitive_commit.py"
alias nvim='NVIM_LISTEN_ADDRESS=/tmp/nvim nvim'
