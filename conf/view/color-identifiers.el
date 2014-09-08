;;; Color identifiers (variables, etc.) based on their name.

(use-package color-identifiers-mode
  :ensure color-identifiers-mode
  :diminish color-identifiers-mode
  :defer t
  :if (>= (display-color-cells) 256)
  :idle
  (global-color-identifiers-mode))

(provide 'conf/view/color-identifiers)
