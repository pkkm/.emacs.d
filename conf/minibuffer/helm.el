;;; Helm -- an alternative to ido-mode with more features, but no flx matching.

(use-package helm
  :ensure t
  :init

  ;; Go to some function/variable definition (works with a lot of modes).
  (bind-key "C-x g" #'helm-semantic-or-imenu)

  ;; Insert LaTeX math symbol.
  (bind-key "C-c i" #'helm-insert-latex-math)
  (defadvice helm-insert-latex-math (before require-auctex activate)
    (require 'latex))) ; Require the needed part of AUCTeX (otherwise the function will error out).

(provide 'conf/minibuffer/helm)
