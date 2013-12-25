;;; Wrapping of long lines.

;; Wrap on word boundaries.
(setq-default word-wrap t)

;; Indent wrapped parts the same as original lines.
;; The code is in vendor/adaptive-wrap.el -- this is the version from <https://gist.github.com/tsavola/6222431>, which handles tabs properly, in contrast to the version on ELPA (as of Dec 2013).
(autoload 'adaptive-wrap-prefix-mode "adaptive-wrap")
(define-globalized-minor-mode global-adaptive-wrap-prefix-mode
  adaptive-wrap-prefix-mode adaptive-wrap-prefix-mode)
(global-adaptive-wrap-prefix-mode 1)

(provide 'conf/view/wrapping)
