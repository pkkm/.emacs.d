;;; Various lisps.
;; All Lisp modes' keymaps inherit from `lisp-mode-shared-map' (there is no shared hook).
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

(require 'conf/utils/hooks) ; Used: add-hooks.
(require 'conf/view/indent-guides)

(use-package lisp-mode ; Bundled with Emacs; contains lisp-mode, emacs-lisp-mode and lisp-interaction-mode.
  :defer t
  :config

  ;; Indentation.
  (defun my-lisp-indentation ()
    (setq tab-width 8) ; For reading ancient Lisp code.
    (setq indent-tabs-mode nil))
  (add-hooks '(lisp-mode-hook emacs-lisp-mode-hook)
             #'my-lisp-indentation)

  ;; Indent guides.
  (add-hooks '(lisp-mode-hook emacs-lisp-mode-hook)
             #'indent-guide-mode))

(provide 'conf/mode-specific/lisps)
