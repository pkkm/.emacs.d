;;; Clojure-specific configuration. -*- lexical-binding: t -*-

;; Clojure IDE.
(use-package cider
  :ensure t
  :config

  ;; Eldoc -- show function arguments in the minibuffer.
  ;; (Configured in conf/view/eldoc.el.)
  (add-hook 'cider-mode-hook #'eldoc-mode))

(use-package clojure-mode ; Installed by CIDER.
  :config

  ;; Indent guides.
  ;; We don't use `with-eval-after-load' because `indent-guide-mode' is autoloaded, so it would never load.
  (require 'conf/view/indent-guides)
  (add-hook 'clojure-mode-hook #'indent-guide-mode))

;; YASnippet snippets.
(use-package clojure-snippets
  :ensure t)

;; Auto-complete integration.
;; Disabled for now, as it hangs Emacs when typing (the official solution seems to be to switch to company-mode).
;; (use-package ac-cider
;;   :ensure t
;;   :init
;;   (require 'conf/utils/hooks) ; Used: add-hooks.
;;   (with-eval-after-load 'auto-complete
;;     (add-hooks '(cider-mode-hook cider-repl-mode-hook) 'ac-cider-setup)))

;; Clojure cheatsheet (C-c M-h).
;; See also: CIDER's "C-c C-d g" binding to look up symbol in Grimoire (online Clojure reference).
(use-package clojure-cheatsheet
  :ensure t
  :init
  (with-eval-after-load 'clojure-mode
    (defun clojure-cheatsheet-or-error ()
      "Start `clojure-cheatsheet' if there's a nREPL connection (which it needs). Otherwise, error out."
      (interactive)
      (if (and (functionp 'nrepl-current-connection-buffer) ; Future-proofing in case the function name changes.
               (not (nrepl-current-connection-buffer t)))
          (error "An nREPL connection is needed!")
        (call-interactively #'clojure-cheatsheet)))
    (bind-key "C-c M-h" #'clojure-cheatsheet-or-error clojure-mode-map)))

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
