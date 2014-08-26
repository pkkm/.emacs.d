;;; Color identifiers (variables, etc.) based on their name.

(use-package color-identifiers-mode
  :ensure color-identifiers-mode
  :if (>= (display-color-cells) 256)
  :config
  (global-color-identifiers-mode))

(provide 'conf/view/color-identifiers)
