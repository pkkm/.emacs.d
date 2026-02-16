;;; Highlighting and wrapping of long lines. -*- lexical-binding: t -*-

;; Wrap on word boundaries.
(setq-default word-wrap t)

;; A mode for highlighting text that extends beyond a certain column.
(use-package column-enforce-mode
  :ensure t
  :config
  (setq column-enforce-column 80))

(provide 'conf/view/long-lines)
