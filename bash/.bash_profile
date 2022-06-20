# if we are emacs, don't do anything
[[ $TERM = "dumb" ]] && return

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi

. "$HOME/.cargo/env"
