;;; Make `C-u DEL' kill back to indentation.

(defun my-delete-char-or-kill-back-to-indentation (arg)
  "With one C-u, kill from point to the beginning of the line, excluding indentation. If point is in indentation, kill to beginning of the line instead.
With any other prefix argument (including none), call the function `delete-backward-char'."
  (interactive "P")
  (if (equal arg '(4)) ; Called with one C-u.
      (let ((prev-pos (point)))
        (back-to-indentation)
        (when (>= (point) prev-pos)
          (beginning-of-line))
        (kill-region (point) prev-pos))
    (call-interactively #'delete-backward-char)))

(dolist (fun '(delete-backward-char
               backward-delete-char-untabify))
  (bind-key (vector 'remap fun) #'my-delete-char-or-kill-back-to-indentation))
(with-eval-after-load 'evil
  (bind-key "DEL" #'my-delete-char-or-kill-back-to-indentation evil-insert-state-map))

(provide 'conf/editing/kill-back-to-indentation)
