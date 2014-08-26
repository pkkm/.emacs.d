;;; Smex -- like Ido mode, for M-x.

(use-package smex
  :ensure smex
  :defer t
  :bind (("M-x" . smex)
         ("C-x m" . smex))
  :config

  ;; History.
  (setq smex-history-length 100)
  (setq smex-save-file (expand-file-name "M-x-history" my-savefile-dir)) ; Savefile: my-savefile-dir/smex-items.

  (setq smex-prompt-string "M-x "))

;; Unbound, interesting Smex commands:
;;  * smex-major-mode-commands -- limit to the commands relevant to the active major mode (try with Dired or Magit).
;;  * smex-show-unbound-commands -- show frequently called commands that are unbound.

(provide 'conf/minibuffer/M-x)
