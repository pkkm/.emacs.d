;;; Helm -- an alternative to ido-mode with more features, but no flx matching. -*- lexical-binding: t -*-

(use-package helm
  :ensure t
  :init

  ;; Go to some function/variable definition (works with a lot of modes).
  (bind-key "C-x g" #'helm-semantic-or-imenu)

  ;; Insert LaTeX math symbol.
  (bind-key "C-c i" #'helm-insert-latex-math)
  (defun my-require-auctex-for-helm-insert-latex-math (&rest _args)
    (require 'latex)) ; Require the needed part of AUCTeX (otherwise the function will error out).
  (advice-add #'helm-insert-latex-math :before #'my-require-auctex-for-helm-insert-latex-math))

(provide 'conf/minibuffer/helm)
