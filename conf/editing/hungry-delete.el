;;; Hungry delete -- delete all whitespace before point. -*- lexical-binding: t -*-

(use-package hungry-delete
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-define-key '(normal insert) 'global (kbd "M-<backspace>") #'hungry-delete-backward)
    (evil-define-key '(normal insert) 'global (kbd "M-DEL") #'hungry-delete-backward)))

(provide 'conf/editing/hungry-delete)
