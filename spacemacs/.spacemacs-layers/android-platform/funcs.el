(defun uart-logs ()
  (interactive)
  (serial-term "/dev/ttyUSB0" 921600))
