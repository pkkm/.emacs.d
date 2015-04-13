;;; Color identifiers (variables, etc.) based on their name.

(use-package color-identifiers-mode
  :ensure t
  :if (>= (display-color-cells) 256)
  :defer 3 ; Load after 3 s of idle.
  :diminish color-identifiers-mode
  :config
  (global-color-identifiers-mode)
  (add-hook 'after-load-theme-hook #'color-identifiers:regenerate-colors))

(provide 'conf/view/color-identifiers)
