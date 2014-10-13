;;; Make C-w kill a word when there's no region.

(defun kill-region-or-word (&optional arg)
  "If the region is active, kill it. Otherwise, kill a word backwards."
  (interactive "p")
  (if (use-region-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") #'kill-region-or-word)

(provide 'conf/editing/kill-word-with-C-w)
