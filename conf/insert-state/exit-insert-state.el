;;; Exit insert/replace state with C-SPC.

(with-eval-after-load 'evil
  (dolist (key (list "C-SPC" "C-@"))
    (bind-key key #'evil-normal-state evil-insert-state-map)
    (bind-key key #'evil-normal-state evil-replace-state-map)))

(provide 'conf/insert-state/exit-insert-state)
