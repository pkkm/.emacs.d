;;; Count lines, words and characters in buffer. -*- lexical-binding: t -*-

;; g C-g: count in buffer or region, if active.
(with-eval-after-load 'evil
  (bind-key "g C-g" #'count-words evil-motion-state-map))

(provide 'conf/evil-specific/count-lines-words-chars)
