# ssh/gpg

# This is only enabled on local sessions
# On remote sessions, we rely on ssh forwarding for ssh authentication

if [ -n $SSH_CONNECTION ]; then
    export SSH_AUTH_SOCK=$(gpgconf --list-dirs agent-ssh-socket)
    gpgconf --launch gpg-agent
fi
