;;; Compilation-mode -- for displaying results of compilation, grep, search, etc. -*- lexical-binding: t -*-

(use-package compile ; Bundled with Emacs.
  :config
  (with-eval-after-load 'evil
    ;; Make compilation buffers start in Emacs state.
    (evil-set-initial-state 'compilation-mode 'emacs)

    ;; Keybindings.
    (evil-define-key 'motion compilation-mode-map "r" #'recompile)
    (evil-define-key 'normal compilation-mode-map "r" #'recompile) ; Necessary to shadow the usual binding.

    ;; Support ANSI colors.
    (add-hook 'compilation-filter-hook #'ansi-color-compilation-filter)))

(provide 'conf/mode-specific/compilation)
