;;; Undo customizations. -*- lexical-binding: t -*-

(use-package undo-fu
  :ensure t
  :config
  (setq undo-fu-ignore-keyboard-quit t))

(use-package vundo
  :ensure t
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (setq vundo-window-max-height 10))

;; Increase memory limits for undo history.
(dolist (var '(undo-limit undo-strong-limit undo-outer-limit))
  (set var (* 5 (symbol-value var))))

;; In visual state, use "u" for undoing instead of lowercasing ("g u").
(with-eval-after-load 'evil
  (evil-define-key 'visual 'global (kbd "u") #'evil-undo))

(provide 'conf/editing/undo)
