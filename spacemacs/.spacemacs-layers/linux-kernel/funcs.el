(defun linux-kernel/reviewed-by ()
  (interactive)
  "Get the 'From:' header in Notmuch Message mode."
  (let ((from-header (message-fetch-field "From")))
    (if from-header
        (insert (concat "Reviewed-by: " from-header))
      (message "No From: header found!"))))

(defun linux-kernel/tested-by ()
  (interactive)
  (let ((from-header (message-fetch-field "From")))
    (if from-header
        (insert (concat "Tested-by: " from-header))
      (message "No From: header found!"))))

(defun linux-kernel/greet-submitter ()
  (interactive)
  (let ((to-header (message-fetch-field "To")))
    (if to-header
        (let ((first-name (car (split-string to-header))))
          (insert (concat "Hi " first-name ",\n\nThank you for the patch."))))
    (message "No To: header found!")))

(defun linux-kernel/serial-term ()
  (interactive)
  (let ((speed (completing-read "Speed (b/s) "
                             linux-kernel-serial-term-default-speeds)))
    (serial-term
     linux-kernel-serial-term-default-port
     (string-to-number speed))))

