;;; Terminal modes.

(use-package term ; Bundled with Emacs.
  :config

  (defun my-term-mode-hook ()
    (hl-line-mode -1) ; Don't highlight the current line.
    (setq scroll-margin 0)) ; Some programs display prompts at the bottom of the screen.
  (add-hook 'term-mode-hook #'my-term-mode-hook)

  ;; Start in Emacs state.
  (with-eval-after-load 'evil
    (evil-set-initial-state 'term-mode 'emacs)))

(provide 'conf/mode-specific/term)
