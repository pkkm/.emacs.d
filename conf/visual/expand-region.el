;;; Expand-region: increase selection by semantic units.

(use-package expand-region
  :ensure expand-region
  :commands (er/expand-region er/contract-region)
  :init

  (with-eval-after-load 'evil
    ;; Bindings: C-SPC -- expand, C-M-SPC -- contract.
    (define-key evil-motion-state-map (kbd "C-SPC") #'er/expand-region)
    (define-key evil-motion-state-map (kbd "C-M-SPC") #'er/contract-region)

    ;; The same for terminals, where C-SPC sends C-@.
    (define-key evil-motion-state-map (kbd "C-@") #'er/expand-region)
    (define-key evil-motion-state-map (kbd "C-M-@") #'er/contract-region)))

(provide 'conf/visual/expand-region)
