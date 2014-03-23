;;; Terminal modes.

(add-hook 'term-mode-hook #'my-term-mode-hook)

(defun my-term-mode-hook ()
  (hl-line-mode -1) ; Don't highlight the current line.
  (setq scroll-margin 0)) ; Some programs display prompts at the bottom of the screen.)

(provide 'conf/mode-specific/term)
