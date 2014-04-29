;;; Compilation-mode -- for displaying results of compilation, grep, search, etc.

(require 'conf/evil)
(require 'conf/utils/keys) ; Used: evil-define-key-in-states.
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.

(defun my-compilation-mode-bindings ()
  (evil-define-key 'motion compilation-mode-map "r" #'recompile)
  (evil-define-key 'normal compilation-mode-map "r" #'recompile)) ; Necessary to shadow the usual binding.
(add-one-shot-hook 'compilation-mode-hook #'my-compilation-mode-bindings)

(provide 'conf/mode-specific/compilation)
