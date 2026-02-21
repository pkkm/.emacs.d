;;; Highlighting and wrapping of long lines. -*- lexical-binding: t -*-

;; Wrap on word boundaries.
(setq-default word-wrap t)

(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(defun my-update-fill-column-indicator-column (&rest _)
  (set-face-foreground 'fill-column-indicator (face-background 'highlight nil t)))
(add-hook 'enable-theme-functions #'my-update-fill-column-indicator-column)
(my-update-fill-column-indicator-column)

(provide 'conf/view/long-lines)
