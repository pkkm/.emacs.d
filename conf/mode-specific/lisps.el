;;; Various lisps.
;; All Lisp modes' keymaps inherit from `lisp-mode-shared-map'.

(require 'conf/utils/hooks) ; Used: add-hooks.

;; To define a key in every Lisp mode, use `lisp-mode-shared-map'.
;; (There is, unfortunately, no shared hook.)

(add-hooks '(lisp-mode-hook lisp-interaction-mode-hook emacs-lisp-mode-hook)
           #'disable-indent-tabs-mode)
(add-hooks '(lisp-mode-hook lisp-interaction-mode-hook emacs-lisp-mode-hook)
           #'indent-guide-mode)

(provide 'conf/mode-specific/lisps)
