;;; Insert newline after/before the current line. -*- lexical-binding: t -*-
;; The wrappers are needed because the Evil functions aren't interactive.
;; TODO feature request to Evil to make them interactive and accepting count.

(with-eval-after-load 'evil
  (defun my-insert-line-below (count)
    (interactive "p")
    (dotimes (_ count)
      (evil-insert-newline-below)))
  (evil-define-key 'normal 'global (kbd "g o") #'my-insert-line-below)

  (defun my-insert-line-above (count)
    (interactive "p")
    (dotimes (_ count)
      (evil-insert-newline-above)))
  (evil-define-key 'normal 'global (kbd "g O") #'my-insert-line-above))

(provide 'conf/evil-specific/insert-newline)
