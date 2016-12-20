;;; Smex -- like Ido mode, for M-x. -*- lexical-binding: t -*-

(use-package smex
  :ensure t
  :bind (("M-x" . smex)
         ("C-x SPC" . smex)))

;; Interesting command: smex-show-unbound-commands -- show frequently called commands that are unbound.

(provide 'conf/minibuffer/M-x)
