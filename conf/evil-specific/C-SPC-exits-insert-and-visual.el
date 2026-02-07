;;; Exit insert, replace and visual states with C-SPC. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (dolist (key (list "C-SPC" "C-@"))
    (evil-define-key '(insert replace) 'global (kbd key) #'evil-normal-state)
    (evil-define-key 'visual 'global (kbd key) #'evil-exit-visual-state)))

(provide 'conf/evil-specific/C-SPC-exits-insert-and-visual)
