;;; Highlighting and wrapping of long lines.

;; When whitespace-mode is enabled, highlight long lines (and weird indentation).
;; Threshold column: `whitespace-line-column' (default: 80).
(setq whitespace-style '(face lines-tail space-before-tab))

;; Wrap on word boundaries.
(setq-default word-wrap t)

;; Indent wrapped parts the same as original lines.
(use-package adaptive-wrap ; Installed in vendor/adaptive-wrap.el -- this is the version from <https://gist.github.com/tsavola/6222431>, which handles tabs properly, in contrast to the version on ELPA (as of Dec 2013).
  :config
  (with-no-warnings ; To disable the byte-compilation warning saying that this should be called at toplevel.
    (define-globalized-minor-mode global-adaptive-wrap-prefix-mode
      adaptive-wrap-prefix-mode adaptive-wrap-prefix-mode))
  (global-adaptive-wrap-prefix-mode))

(provide 'conf/view/long-lines)
