;;; Increment the number at point. -*- lexical-binding: t -*-
;; To decrement, use a negative prefix argument.

(use-package evil-numbers
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key 'normal 'global (kbd "C-a") #'evil-numbers/inc-at-pt)))

(provide 'conf/editing/increment-number)
