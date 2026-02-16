;;; Buffer utilities. -*- lexical-binding: t -*-

(defun my-buffers-visible-in-windows (&optional all-frames)
  "Return a list of all buffers currently displayed in windows, without duplicates."
  (seq-uniq (mapcar #'window-buffer (window-list-1 nil 'no-minibuffer t))))

(provide 'conf/utils/buffers)
