(spacemacs/declare-prefix "aa" "Android")
(spacemacs/set-leader-keys "aal" 'uart-logs)
(when (configuration-layer/package-used-p 'device-control)
  (spacemacs/set-leader-keys "aad" 'device-control))
