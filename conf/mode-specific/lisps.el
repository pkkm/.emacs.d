;;; Various lisps.
;; All Lisp modes' keymaps inherit from `lisp-mode-shared-map' (there is no shared hook).
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

(require 'conf/packages)
(require 'conf/utils/hooks) ; Used: add-hooks.

(use-package lisp-mode ; Bundled with Emacs; contains lisp-mode, emacs-lisp-mode and lisp-interaction-mode.
  :defer t
  :config

  (add-hooks '(lisp-mode-hook emacs-lisp-mode-hook)
             #'disable-indent-tabs-mode)
  (add-hooks '(lisp-mode-hook emacs-lisp-mode-hook)
             #'indent-guide-mode))

(provide 'conf/mode-specific/lisps)
