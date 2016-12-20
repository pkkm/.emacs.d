;;; Haskell. -*- lexical-binding: t -*-
;; Things to consider adding in the future:
;;   * https://github.com/chrisdone/hindent
;;   * https://github.com/chrisdone/structured-haskell-mode
;;   * https://github.com/alanz/HaRe

;; Main package.
(use-package haskell-mode
  :ensure t
  :config

  (add-hook 'haskell-mode-hook #'turn-on-haskell-indentation) ; Most advanced indentation mode (hi2 was merged into it in March 2015: <https://github.com/haskell/haskell-mode/wiki/Month-in-haskell-mode-March-2015#important-developments>).
  (add-hook 'haskell-mode-hook #'interactive-haskell-mode) ; Use the new interactive mode.
  (add-hook 'haskell-mode-hook #'eldoc-mode) ; Show type signatures in minibuffer (eldoc config is in conf/view/eldoc.el). Uses interactive process when available, otherwise hardcoded list.

  ;; Customizations suggested on <https://github.com/haskell/haskell-mode/wiki/Haskell-Interactive-Mode-Setup>.
  (setq haskell-process-suggest-remove-import-lines t)
  (setq haskell-process-auto-import-loaded-modules t)
  (setq haskell-process-log t)

  ;; Use Cabal for the REPL so that projects stay sandboxed instead of polluting the global database.
  (setq haskell-process-type 'cabal-repl))

;; Auto-configure Flycheck from Cabal.
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
