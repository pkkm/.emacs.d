;;; Increment/decrement the number at point.

(use-package evil-numbers
  :defer t
  :init
  (with-eval-after-load 'evil
    (bind-key "C-a" #'evil-numbers/inc-at-pt evil-normal-state-map)
    (bind-key "C-M-a" #'evil-numbers/dec-at-pt evil-normal-state-map)))

(provide 'conf/operators/inc-dec-number)
