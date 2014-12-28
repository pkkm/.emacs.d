;;; Semantic (part of CEDET) -- syntax-aware features for various languages.

(use-package semantic ; Bundled with Emacs.
  :defer t
  :init
  (semantic-mode 1)
  :config

  (bind-key "C-c ." #'semantic-ia-fast-jump semantic-mode-map) ; Go to definition (to be shadowed by some modes).
  (bind-key "C-c ^" #'senator-go-to-up-reference semantic-mode-map) ; Go up one "reference level".

  (global-semantic-idle-summary-mode 1)) ; Similar to Eldoc.

;; Other useful functions:
;;   * semantic-ia-show-doc -- show documentation.
;;   * semantic-ia-show-summary -- show type and some other information.
;;   * semantic-analyze-proto-impl-toggle -- toggle between declaration and implementation.

(provide 'conf/other/semantic)
