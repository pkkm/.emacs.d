;;; A keybinding for inserting the current date. -*- lexical-binding: t -*-

(defun insert-current-date (arg)
  (interactive "P")
  (insert (format-time-string (if arg "%Y-%m-%d %H:%M" "%Y-%m-%d"))))

(bind-key "C-x C-d" #'insert-current-date) ; Shadows binding for listing a directory.

(provide 'conf/other/insert-current-date)
