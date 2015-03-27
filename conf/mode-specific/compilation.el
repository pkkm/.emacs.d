;;; Compilation-mode -- for displaying results of compilation, grep, search, etc.

(use-package compile ; Bundled with Emacs.
  :defer t
  :config
  (with-eval-after-load 'evil
    ;; Make compilation buffers start in Emacs mode.
    (evil-set-initial-state 'compilation-mode 'emacs)

    ;; Keybindings.
    (evil-define-key 'motion compilation-mode-map "r" #'recompile)
    (evil-define-key 'normal compilation-mode-map "r" #'recompile) ; Necessary to shadow the usual binding.
    (evil-normalize-keymaps))) ; So that the bindings take effect immediately.

(provide 'conf/mode-specific/compilation)
