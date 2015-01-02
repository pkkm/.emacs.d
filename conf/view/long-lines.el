;;; When whitespace-mode is enabled, highlight long lines (and weird indentation).
;; Highlighted column: `whitespace-line-column' (default: 80).

(setq whitespace-style '(face lines-tail space-before-tab))

(provide 'conf/view/long-lines)
