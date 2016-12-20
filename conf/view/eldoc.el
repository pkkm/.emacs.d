;;; Eldoc minor mode -- show function arguments in the minibuffer. -*- lexical-binding: t -*-

(use-package eldoc ; Included with Emacs.
  :diminish eldoc-mode
  :config
  (setq eldoc-idle-delay 0.1)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)) ; Show in minibuffer when doing `eval-expression'.

(provide 'conf/view/eldoc)
