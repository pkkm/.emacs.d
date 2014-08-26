;;; Compilation-mode -- for displaying results of compilation, grep, search, etc.

(require 'conf/evil)

(use-package compile ; Bundled with Emacs.
  :defer t
  :config
  (evil-define-key 'motion compilation-mode-map "r" #'recompile)
  (evil-define-key 'normal compilation-mode-map "r" #'recompile) ; Necessary to shadow the usual binding.
  (evil-normalize-keymaps)) ; So that the bindings take effect immediately.

(provide 'conf/mode-specific/compilation)
