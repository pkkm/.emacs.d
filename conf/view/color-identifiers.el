;;; Color identifiers (variables, etc.) based on their name.

(use-package color-identifiers-mode
  :ensure color-identifiers-mode
  :diminish color-identifiers-mode
  :defer t
  :if (>= (display-color-cells) 256)
  :idle
  (global-color-identifiers-mode)
  :config
  (add-hook 'after-load-theme-hook #'color-identifiers:regenerate-colors))

(provide 'conf/view/color-identifiers)
