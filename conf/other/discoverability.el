;;; Discoverability features. -*- lexical-binding: t -*-

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :init
  (which-key-mode)
  :config
  (which-key-setup-side-window-right-bottom) ; Show on right if there's enough space, on bottom otherwise.
  (setq which-key-idle-delay 0.5))

(use-package helpful
  :ensure t
  :init
  ;; Replace built-in keybindings.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command))

;; C-h C-m -- show documentation for major mode commands (nicer than C-h m).
(use-package discover-my-major
  :ensure t
  :bind ("C-h C-m" . discover-my-major))

;; I tried to use discover.el but it was too intrusive (e.g. bound M-s to its own command in every major mode's map) and I haven't found a way to override its bindings.

(provide 'conf/other/discoverability)
