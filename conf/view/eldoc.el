;;; Eldoc minor mode -- show function arguments in the minibuffer. -*- lexical-binding: t -*-

(use-package eldoc ; Included with Emacs.
  :diminish eldoc-mode
  :config
  (setq eldoc-idle-delay 0.1)
  (setq eldoc-documentation-strategy 'eldoc-documentation-compose-eagerly) ; Allow documentation strings from multiple backends.

  (bind-key "C-h ." #'eldoc-doc-buffer) ; Show a buffer with results from all backends.
  (setq eldoc-echo-area-prefer-doc-buffer t)) ; Prefer the buffer for display if it's visible.

(provide 'conf/view/eldoc)
