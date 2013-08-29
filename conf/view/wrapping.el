;;; Wrapping of long lines.

;; Wrap on word boundaries.
(setq-default word-wrap t)

;; Indent wrapped parts the same as original lines.
(require 'conf/packages)
(package-ensure-installed 'adaptive-wrap)
(define-globalized-minor-mode global-adaptive-wrap-prefix-mode
  adaptive-wrap-prefix-mode adaptive-wrap-prefix-mode)
(global-adaptive-wrap-prefix-mode 1)

(provide 'conf/view/wrapping)
