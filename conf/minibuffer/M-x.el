;;; Smex -- like Ido mode, for M-x.

(use-package smex
  :ensure smex
  :defer t
  :bind (("M-x" . smex)
         ("C-x m" . smex))
  :config
  ;; Save smex data in `user-emacs-directory' instead of my home directory.
  (setq smex-save-file (locate-user-emacs-file "smex")))

;; Unbound, interesting Smex commands:
;;  * smex-major-mode-commands -- limit to the commands relevant to the active major mode (try with Dired or Magit).
;;  * smex-show-unbound-commands -- show frequently called commands that are unbound.

(provide 'conf/minibuffer/M-x)
