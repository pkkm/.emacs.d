;;; Terminal modes.

;; Don't highlight the current line.
(add-hook 'term-mode-hook
          (lambda ()
            (hl-line-mode -1)))

(provide 'conf/mode-specific/term)
