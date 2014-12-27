;;; Helm -- an alternative to ido-mode with more features, but no flx matching.

(use-package helm
  :ensure helm
  :defer t
  :init
  (bind-key "C-x g" #'helm-semantic-or-imenu)) ; Go to some function/variable definition (works with a lot of modes).

(provide 'conf/minibuffer/helm)
