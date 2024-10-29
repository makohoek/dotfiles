(defun linux-kernel/reviewed-by ()
  (interactive)
  (insert (concat "Reviewed-by: " user-full-name " <" user-mail-address ">")))

(defun linux-kernel/tested-by ()
  (interactive)
  (insert (concat "Tested-by: " user-full-name " <" user-mail-address ">")))

(defun linux-kernel/serial-term ()
  (interactive)
  (let ((speed (completing-read "Speed (b/s) "
                             linux-kernel-serial-term-default-speeds)))
    (serial-term
     linux-kernel-serial-term-default-port
     (string-to-number speed))))

