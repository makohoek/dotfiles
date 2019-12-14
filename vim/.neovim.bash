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
