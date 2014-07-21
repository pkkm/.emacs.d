;;; Exit insert/replace state with C-SPC.

(require 'conf/evil)

(dolist (key (list (kbd "C-SPC") (kbd "C-@")))
  (define-key evil-insert-state-map key #'evil-normal-state)
  (define-key evil-replace-state-map key #'evil-normal-state))

(provide 'conf/insert-state/exit-insert-state)
