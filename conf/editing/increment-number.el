;;; Increment the number at point.
;; To decrement, use a negative prefix argument.

(use-package evil-numbers
  :ensure t
  :init
  (with-eval-after-load 'evil
    (bind-key "C-a" #'evil-numbers/inc-at-pt evil-normal-state-map)))

(provide 'conf/editing/increment-number)
