;;; Insert newline after/before the current line.
;; The wrappers are needed because the Evil functions aren't interactive.

(with-eval-after-load 'evil
  (require 'cl-lib) ; Used: cl-dotimes.

  (defun my-insert-line-below (count)
    (interactive "p")
    (cl-dotimes (unused-var count)
      (evil-insert-newline-below)))
  (bind-key "g o" #'my-insert-line-below evil-normal-state-map)

  (defun my-insert-line-above (count)
    (interactive "p")
    (cl-dotimes (unused-var count)
      (evil-insert-newline-above)))
  (bind-key "g O" #'my-insert-line-above evil-normal-state-map))

(provide 'conf/editing/insert-newline)
