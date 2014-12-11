;;; Clojure-specific configuration.

;; Clojure IDE (automatically installs clojure-mode).
(use-package cider
  :ensure cider
  :defer t
  :config

  ;; Eldoc -- show function arguments in the minibuffer.
  ;; (Configuration is in conf/view/eldoc.el.)
  (add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode))

;; YASnippet snippets.
(use-package clojure-snippets
  :ensure clojure-snippets
  :defer t)

;; Auto-complete integration.
;; Disabled for now, as it hangs Emacs when typing (the official solution seems to be to switch to company-mode).
;; (use-package ac-cider
;;   :ensure t
;;   :defer t
;;   :init
;;   (require 'conf/utils/hooks) ; Used: add-hooks.
;;   (with-eval-after-load 'auto-complete
;;     (add-hooks '(cider-mode-hook cider-repl-mode-hook) 'ac-cider-setup)))

;; Clojure cheatsheet.
;; Usage: M-x clojure-cheatsheet.
(use-package clojure-cheatsheet
  :ensure clojure-cheatsheet
  :defer t)

;; Refactoring.
;; Prefix: C-c C-m (overrides the binding for macroexpand).
(use-package clj-refactor
  :ensure clj-refactor
  :defer t
  :diminish clj-refactor-mode
  :init
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(provide 'conf/mode-specific/clojure)
