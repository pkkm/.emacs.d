;;; Haskell. -*- lexical-binding: t -*-

;; Main package.
(use-package haskell-mode
  :ensure t
  :config

  (add-hook 'haskell-mode-hook #'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook #'eldoc-mode) ; Show type signatures in minibuffer. Uses interactive process when available, hardcoded list otherwise.

  ;; Customizations for haskell-interactive-mode.
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)

  ;; Use Emacs state for error buffers so that they can be quickly closed with "q".
  (with-eval-after-load 'evil
    (evil-set-initial-state 'haskell-error-mode 'emacs)))

;; Auto-configure Flycheck.
(use-package flycheck-haskell
  :ensure t
  :init
  (with-eval-after-load 'haskell-mode ; No need to load if we're not editing any Haskell file.
    (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)))

;; Additional snippets.
(use-package haskell-snippets
  :ensure t)

(provide 'conf/mode-specific/haskell)
