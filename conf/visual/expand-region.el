;;; Expand-region: increase selection by semantic units.

(use-package expand-region
  :ensure expand-region
  :commands (er/expand-region er/contract-region)
  :init

  (with-eval-after-load 'evil
    ;; Bindings: C-SPC -- expand, C-M-SPC -- contract.
    (bind-key "C-SPC" #'er/expand-region evil-motion-state-map)
    (bind-key "C-M-SPC" #'er/contract-region evil-motion-state-map)

    ;; The same for terminals, where C-SPC sends C-@.
    (bind-key "C-@" #'er/expand-region evil-motion-state-map)
    (bind-key "C-M-@" #'er/contract-region evil-motion-state-map)))

(provide 'conf/visual/expand-region)
