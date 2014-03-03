# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="duellj"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Uncomment this to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment following line if you want to  shown in the command execution time stamp 
# in the history command output. The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|
# yyyy-mm-dd
# HIST_STAMPS="mm/dd/yyyy"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git colored-man sudo)

source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/home/kir0gawa/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games"
# export MANPATH="/usr/local/man:$MANPATH"

# # Preferred editor for local and remote sessions
 if [[ -n $SSH_CONNECTION ]]; then
   export EDITOR='vim'
 else
   export EDITOR='mvim'
 fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/dsa_id"

alias helpcvim='less ~/Documents/reminderSnipmate'
alias pycharm='~/bin/pycharm-2.7.3/bin/pycharm.sh'
alias webstorm='~/bin/WebStorm-129.664/bin/webstorm.sh'
alias intellij='~/bin/idea-IC-129.713/bin/idea.sh'
alias getedt='cd ~/Development/EdtCamsi/src/ && python Main.py'
alias vpn='cd ~/Documents/OpenVPN/NequaliaLinux/ && sudo ./startOpenvpn'

# be greeted by great motivationalQuotes
nbOfLines=$( cat .motivationalQuotes | wc -l ); randline=$(( $RANDOM % $(( nbOfLines + 1 )) + 1 )); tail -$randline .motivationalQuotes | head -1 | cowsay -f stegosaurus

# show vim cheats every time because i have to learn them :)
#cat ~/.cheatsVIM

alias wakerpi='sudo etherwake b8:27:eb:5f:8d:f8'

export LD_LIBRARY_PATH=~/Intel/pfw/lib
