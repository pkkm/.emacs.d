;;; A keybinding for inserting the current date.

(defun insert-current-date (arg)
  (interactive "P")
  (insert (if arg
              (format-time-string "%Y-%m-%d %H:%M")
            (format-time-string "%Y-%m-%d"))))

(bind-key "C-x C-d" #'insert-current-date) ; Shadows binding for listing a directory.

(provide 'conf/other/insert-current-date)
