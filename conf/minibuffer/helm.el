;;; Helm -- an alternative to ido-mode with more features, but no flx matching.

(use-package helm
  :ensure helm
  :defer t
  :init
  (bind-key "C-x g" #'helm-semantic-or-imenu) ; Go to some function/variable definition (works with a lot of modes).
  :config
  (require 'helm-plugin)) ; Fixes error in `helm-semantic-or-imenu': "helm-interpret-value: Symbol must be a function or a variable".

(provide 'conf/minibuffer/helm)
