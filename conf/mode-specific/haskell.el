;;; Haskell. -*- lexical-binding: t -*-
;; For IDE features, install Intero or haskell-ide-engine. (As of 2018-11, Intero is more mature but haskell-ide-engine is more advanced.)

;; Main package.
(use-package haskell-mode
  :ensure t
  :config

  (add-hook 'haskell-mode-hook #'interactive-haskell-mode) ; Use the new interactive mode.
  (add-hook 'haskell-mode-hook #'eldoc-mode) ; Show type signatures in minibuffer (eldoc config is in conf/view/eldoc.el). Uses interactive process when available, otherwise hardcoded list.

  ;; Customizations suggested on <https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup>.
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)

  ;; Use Emacs state for error buffers so that they can be quickly closed with "q".
  (evil-set-initial-state 'haskell-error-mode 'emacs))

;; Auto-configure Flycheck.
(use-package flycheck-haskell
  :ensure t
  :init
  (with-eval-after-load 'haskell-mode ; No need to load if we're not editing any Haskell file.
    (with-eval-after-load 'flycheck
      (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))))

;; Additional snippets.
(use-package haskell-snippets
  :ensure t)

(provide 'conf/mode-specific/haskell)
