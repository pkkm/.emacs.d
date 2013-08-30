;;; Insert newline after/before the current line.
;;; The wrappers are needed because the Evil functions aren't interactive.

(require 'conf/evil)

(defun my-insert-line-below (count)
  (interactive "p")
  (cl-dotimes (unused-var count)
    (evil-insert-newline-below)))
(define-key evil-normal-state-map (kbd "g o") 'my-insert-line-below)

(defun my-insert-line-above (count)
  (interactive "p")
  (cl-dotimes (unused-var count)
    (evil-insert-newline-above)))
(define-key evil-normal-state-map (kbd "g O") 'my-insert-line-above)

(provide 'conf/operators/insert-newline)
