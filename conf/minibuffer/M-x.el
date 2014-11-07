;;; Smex -- like Ido mode, for M-x.

(use-package smex
  :ensure smex
  :defer t
  :bind (("M-x" . smex)
         ("C-x SPC" . smex)
         ("C-x m" . smex-major-mode-commands))
  :config
  ;; Save smex data in `user-emacs-directory' instead of the home directory.
  (setq smex-save-file (locate-user-emacs-file "smex")))

;; Interesting command: smex-show-unbound-commands -- show frequently called commands that are unbound.

(provide 'conf/minibuffer/M-x)
