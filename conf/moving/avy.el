;;; Avy -- quickly jump to word/char (like ace-jump, but with more features and works with Evil out of the box).

(use-package avy
  :ensure t
  :defer t
  :init
  (bind-key "g SPC" #'avy-goto-word-1 evil-motion-state-map)
  (bind-key "g RET" #'avy-goto-char-2 evil-motion-state-map))

(provide 'conf/moving/avy)
