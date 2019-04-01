;;; Clojure-specific configuration. -*- lexical-binding: t -*-

;; Clojure IDE.
(use-package cider
  :ensure t
  :config

  ;; Eldoc -- show function arguments in the minibuffer.
  ;; (Configured in conf/view/eldoc.el.)
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package clojure-mode
  :ensure t
  :config

  ;; Indent guides.
  (require 'conf/view/indent-guides)
  (add-hook 'clojure-mode-hook #'indent-guide-mode)

  ;; Clojure cheatsheet.
  (with-eval-after-load 'cider
    (bind-key "C-c M-h" #'cider-cheatsheet clojure-mode-map)))

;; YASnippet snippets.
(use-package clojure-snippets
  :ensure t)

;; Refactoring.
;; Prefix: C-c RET (overrides the binding for macroexpand).
(use-package clj-refactor
  :ensure t
  :diminish clj-refactor-mode
  :init
  (add-hook 'clojure-mode-hook #'clj-refactor-mode)
  :config
  (cljr-add-keybindings-with-prefix "C-c RET"))

(provide 'conf/mode-specific/clojure)
