# ls
alias ls='ls --color=auto'
alias lal='ls -alF'
alias ll='ls -lF'
alias la='ls -A'

# grep
alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'

# info is far better with vi keys
alias info='info --vi-keys'

# small alias to copy pwd into tmux buffer
alias pwdcp='pwd; tmux copy-mode \; send-keys k ^ v $ y'

# tmux alias to fixup vim-solarized in tmux
alias tmux='export TERM="screen-256color"; tmux -2'

# neovim specific stuff
alias nvimd='NVIM_LISTEN_ADDRESS=/tmp/nvim nvim'

# vim: filetype=sh syntax=sh
