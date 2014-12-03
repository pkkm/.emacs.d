;;; Exit insert, replace and visual states with C-SPC.

(with-eval-after-load 'evil
  (dolist (key (list "C-SPC" "C-@"))
    (bind-key key #'evil-normal-state evil-insert-state-map)
    (bind-key key #'evil-normal-state evil-replace-state-map)
    (bind-key key #'evil-exit-visual-state evil-visual-state-map)))

(provide 'conf/useless-without-evil/C-SPC-exits-insert-and-visual)
