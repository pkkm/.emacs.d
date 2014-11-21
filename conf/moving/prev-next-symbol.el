;;; Smart Scan -- go to next/previous occurrence of current symbol.

(use-package smartscan
  :ensure smartscan
  :defer t
  :init
  (global-smartscan-mode 1)
  :config
  ;; Unbind M-' (replace symbol in buffer) -- I have my own function for this.
  (bind-key "M-'" nil smartscan-map))

(provide 'conf/moving/prev-next-symbol)
