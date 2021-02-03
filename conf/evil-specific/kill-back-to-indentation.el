;;; Make `C-u DEL' kill back to indentation in insert mode.

(with-eval-after-load 'evil
  (defun my-kill-back-to-indentation ()
    "Kill from point back to the first non-whitespace character on the line.
If point is between the beginning of line and the first non-whitespace character, kill to beginning of line instead."
    (interactive)
    (let ((prev-pos (point)))
      (back-to-indentation)
      (when (>= (point) prev-pos)
        (beginning-of-line))
      (kill-region (point) prev-pos)))
  (bind-key "C-u" #'my-kill-back-to-indentation evil-insert-state-map))

(provide 'conf/evil-specific/kill-back-to-indentation)
