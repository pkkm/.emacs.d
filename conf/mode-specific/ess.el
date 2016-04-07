;;; ESS (Emacs Speaks Statistics).

(use-package ess-site
  :config
  (ess-toggle-S-assign nil)) ; Don't replace "_" with " <- ".

(provide 'conf/mode-specific/ess)
