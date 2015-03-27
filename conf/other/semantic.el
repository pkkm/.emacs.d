;;; Semantic (part of CEDET) -- syntax-aware features for various languages.

(use-package semantic ; Bundled with Emacs.
  :defer t
  :init
  (semantic-mode 1)
  :config

  ;; Disable Semantic's completion on TAB press (makes Emacs hang for multiple seconds, as of 2015-01).
  (setq completion-at-point-functions
        (delq #'semantic-completion-at-point-function completion-at-point-functions))

  (bind-key "C-c ." #'semantic-ia-fast-jump semantic-mode-map) ; Go to definition (to be shadowed by some modes).
  (bind-key "C-c ^" #'senator-go-to-up-reference semantic-mode-map) ; Go up one "reference level".

  (global-semantic-idle-summary-mode 1)) ; Similar to Eldoc.

;; Other useful functions:
;;   * semantic-ia-show-doc -- show documentation.
;;   * semantic-ia-show-summary -- show type and some other information.
;;   * semantic-analyze-proto-impl-toggle -- toggle between declaration and implementation.

;; Refactoring on C-c RET (Emacs 24.4+).
(when (version<= "24.4" emacs-version)
  (use-package srefactor
    :ensure srefactor
    :defer t
    :init
    (with-eval-after-load 'semantic
      (bind-key "C-c RET" #'srefactor-refactor-at-point semantic-mode-map))
    :config
    (with-eval-after-load 'evil
      (evil-set-initial-state 'srefactor-ui-menu-mode 'emacs))))

(provide 'conf/other/semantic)
