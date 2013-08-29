;;; Various lisps.

(require 'conf/utils/hooks) ; Used: add-hooks.

;; To define a key in every Lisp mode, use `lisp-mode-shared-map'.
;; (There is, unfortunately, no shared hook.)

(add-hooks
 '(lisp-mode-hook
   lisp-interaction-mode-hook
   emacs-lisp-mode-hook)
 (lambda ()
   ;; Indentation: use spaces exclusively.
   (setq indent-tabs-mode nil)))

(provide 'conf/lang/lisps)
