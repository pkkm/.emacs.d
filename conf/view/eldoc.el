;;; Eldoc minor mode -- show function arguments in the minibuffer.

(use-package eldoc ; Included with Emacs.
  :diminish eldoc-mode
  :defer t
  :config
  (setq eldoc-idle-delay 0.1)
  (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

(provide 'conf/view/eldoc)
