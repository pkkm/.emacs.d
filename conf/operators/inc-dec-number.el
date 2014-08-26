;;; Increment/decrement the number at point.

(package-ensure-installed 'evil-numbers)

(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-M-a") 'evil-numbers/dec-at-pt))

(provide 'conf/operators/inc-dec-number)
