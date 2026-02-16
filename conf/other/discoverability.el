;;; Discoverability features. -*- lexical-binding: t -*-

(use-package which-key ; Bundled with Emacs 30+.
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
  (bind-key "C-h f" #'helpful-callable)
  (bind-key "C-h v" #'helpful-variable)
  (bind-key "C-h k" #'helpful-key)
  (bind-key "C-h x" #'helpful-command))

(provide 'conf/other/discoverability)
