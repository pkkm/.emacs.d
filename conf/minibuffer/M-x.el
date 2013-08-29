;;; Smex -- like Ido mode, for M-x.

(require 'conf/packages)
(package-ensure-installed 'smex)

;; Use "C-x m" for Smex, disable M-x (to ease changing habits).
(global-set-key (kbd "C-x m") 'smex)
(global-set-key (kbd "M-x") (lambda ()
                              (interactive)
                              (error "Use C-x m.")))

;; Unbound, interesting Smex commands:
;;  * smex-major-mode-commands -- limit to the commands relevant to the active major mode (try with Dired or Magit).)
;;  * smex-show-unbound-commands -- show frequently called commands that are unbound.

;; History.
(setq smex-history-length 10)
(setq smex-save-file (expand-file-name "M-x-history" my-savefile-dir)) ; Savefile: my-savefile-dir/smex-items.

(setq smex-prompt-string "M-x ")

;; Initialize in advance. Without this, Smex is initialized on the first run (there might be a minimal delay).
;;(smex-initialize)

(provide 'conf/minibuffer/M-x)
