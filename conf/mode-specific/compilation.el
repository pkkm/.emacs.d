;;; Compilation-mode -- for displaying results of compilation, grep, search, etc.

(require 'conf/evil)
(require 'conf/utils/keys) ; Used: evil-define-key-in-states.
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.

(defun my-compilation-mode-bindings ()
  (evil-define-key-in-states '(motion normal) compilation-mode-map "r" #'recompile))
(add-one-shot-hook 'compilation-mode-hook #'my-compilation-mode-bindings)

(provide 'conf/mode-specific/compilation)
