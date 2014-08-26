;;; Increment/decrement the number at point.

(package-ensure-installed 'evil-numbers)

(require 'conf/evil)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-M-a") 'evil-numbers/dec-at-pt)

(provide 'conf/operators/inc-dec-number)
