# if we are emacs, don't do anything
[[ $TERM = "dumb" ]] && return

# this is needed for rofi to find alacritty
export PATH="$HOME/.cargo/bin:$PATH"

if [ -f ~/.bashrc ]; then
    . ~/.bashrc
fi
