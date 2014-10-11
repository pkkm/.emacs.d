;;; Smex -- like Ido mode, for M-x.

(use-package smex
  :ensure smex
  :defer t
  :bind (("M-x" . smex)
         ("C-x SPC" . smex)
         ("C-x m" . smex)) ; TODO change this to `smex-major-mode-commands' when I get used to the C-x SPC binding.
  :config
  ;; Save smex data in `user-emacs-directory' instead of my home directory.
  (setq smex-save-file (locate-user-emacs-file "smex")))

;; Interesting command: smex-show-unbound-commands -- show frequently called commands that are unbound.

(provide 'conf/minibuffer/M-x)
