;;; A keybinding for inserting the current date/time. -*- lexical-binding: t -*-

(defun insert-current-date (arg)
  (interactive "P")
  (insert (format-time-string
           (cond
            ((equal '(4) arg) "%H:%M")
            ((equal '(16) arg) "%Y-%m-%d %H:%M")
            (t "%Y-%m-%d")))))

(bind-key "C-x C-d" #'insert-current-date) ; Shadows binding for listing a directory.

(provide 'conf/other/insert-current-date)
