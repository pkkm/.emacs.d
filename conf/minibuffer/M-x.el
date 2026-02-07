;;; Amx -- like Ido mode, for M-x. -*- lexical-binding: t -*-

(use-package amx
  :ensure t
  :bind (("M-x" . amx)
         ("C-x SPC" . amx)))

;; Interesting command: amx-show-unbound-commands -- show frequently called commands that are unbound.

(provide 'conf/minibuffer/M-x)
