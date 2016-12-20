;;; Insert newline after/before the current line. -*- lexical-binding: t -*-
;; The wrappers are needed because the Evil functions aren't interactive.
;; TODO feature request to Evil to make them interactive and accepting count.

(with-eval-after-load 'evil
  (require 'cl-lib) ; Used: cl-dotimes.

  (defun my-insert-line-below (count)
    (interactive "p")
    (cl-dotimes (_ count)
      (evil-insert-newline-below)))
  (bind-key "g o" #'my-insert-line-below evil-normal-state-map)

  (defun my-insert-line-above (count)
    (interactive "p")
    (cl-dotimes (_ count)
      (evil-insert-newline-above)))
  (bind-key "g O" #'my-insert-line-above evil-normal-state-map))

(provide 'conf/evil-specific/insert-newline)
