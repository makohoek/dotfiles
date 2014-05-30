# Base16 shell colors
BASE16_SCHEME="solarized"
BASE16_SHELL="$HOME/.config/base16-shell/base16-$BASE16_SCHEME.dark.sh"
[[ -s $BASE16_SHELL ]] && . $BASE16_SHELL

# Path to your oh-my-zsh configuration.
ZSH=$HOME/.oh-my-zsh

ZSH_THEME="3den"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git colored-man sudo)

# Oh my zsh
source $ZSH/oh-my-zsh.sh

# User configuration

export PATH="/home/kir0gawa/bin:/usr/lib/lightdm/lightdm:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games"
# export MANPATH="/usr/local/man:$MANPATH"

export EDITOR=vim

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

alias pycharm='~/bin/pycharm-2.7.3/bin/pycharm.sh'
alias webstorm='~/bin/WebStorm-129.664/bin/webstorm.sh'
alias intellij='~/bin/idea-IC-129.713/bin/idea.sh'
alias getedt='cd ~/Development/EdtCamsi/src/ && python Main.py'
alias vpn='cd ~/Documents/OpenVPN/NequaliaLinux/ && sudo ./startOpenvpn'

# be greeted by great motivationalQuotes
#nbOfLines=$( cat ~/.motivationalQuotes | wc -l ); randline=$(( $RANDOM % $(( nbOfLines + 1 )) + 1 )); tail -$randline .motivationalQuotes | head -1 | cowsay -f stegosaurus

alias tmux='export TERM="screen-256color"; tmux -2'
alias vims='vim --servername VIM'
alias vie='vim --remote'
alias vit='vim --remote-tab'
