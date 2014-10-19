;;; Clojure-specific configuration.

;; Clojure IDE (automatically installs clojure-mode).
(use-package cider
  :ensure cider
  :defer t
  :config

  ;; Eldoc -- show function arguments in the minibuffer.
  ;; (Configuration is in conf/view/eldoc.el.)
  (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode))

;; Auto-complete integration.
;; Disabled for now, as it hangs Emacs when typing (the official solution seems to be to switch to company-mode).
;; (use-package ac-cider
;;   :ensure t
;;   :commands ac-cider-setup
;;   :init
;;   (require 'conf/utils/hooks) ; Used: add-hooks.
;;   (with-eval-after-load 'auto-complete
;;     (add-hooks '(cider-mode-hook cider-repl-mode-hook) 'ac-cider-setup)))

(provide 'conf/mode-specific/clojure)
