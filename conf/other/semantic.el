;;; Semantic (part of CEDET) -- syntax-aware features for various languages. -*- lexical-binding: t -*-
;; Keys are bound globally instead of in semantic-mode-map because otherwise they would shadow major-mode's bindings.

(use-package semantic ; Bundled with Emacs.
  :init
  ;; I'm undecided whether this should be enabled by default. Let's leave it in for now.
  (semantic-mode 1)
  :config

  ;; Disable Semantic's completion on TAB press (makes Emacs hang for multiple seconds, as of 2015-01).
  (setq completion-at-point-functions
        (delq #'semantic-completion-at-point-function completion-at-point-functions))

  (bind-key "C-c M-." #'semantic-ia-fast-jump) ; Go to definition.

  (global-semantic-idle-summary-mode 1)) ; Similar to Eldoc.

;; Other useful functions:
;;   * senator-go-to-up-reference -- go up one "reference level".
;;   * semantic-ia-show-doc -- show documentation.
;;   * semantic-ia-show-summary -- show type and some other information.
;;   * semantic-analyze-proto-impl-toggle -- toggle between declaration and implementation.

(provide 'conf/other/semantic)
