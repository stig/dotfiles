;; Function for inserting the current date
(defun insert-current-date (arg)
    (interactive "P")
    (insert (format-time-string "%Y-%m-%d")))

