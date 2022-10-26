(spacemacs/set-leader-keys "atss" 'linux-kernel/serial-term)

(spacemacs/set-leader-keys-for-major-mode 'notmuch-message-mode
  "pr" 'linux-kernel/reviewed-by)
