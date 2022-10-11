(defun linux-kernel/reviewed-by ()
  (interactive)
  (insert (concat "Reviewed-by: " user-full-name " <" user-mail-address ">")))
