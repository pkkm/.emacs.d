;;; Expand-region: increase selection by semantic units.

(require 'conf/evil)

(use-package expand-region
  :ensure expand-region
  :commands (er/expand-region er/contract-region)
  :init

  ;; Bindings: C-SPC -- expand, C-M-SPC -- contract.
  (define-key evil-motion-state-map (kbd "C-SPC") #'er/expand-region)
  (define-key evil-motion-state-map (kbd "C-M-SPC") #'er/contract-region)

  ;; Bindings for terminals C-@ -- expand, C-M-@ -- contract.
  (define-key evil-motion-state-map (kbd "C-@") #'er/expand-region)
  (define-key evil-motion-state-map (kbd "C-M-@") #'er/contract-region))

(provide 'conf/visual/expand-region)
