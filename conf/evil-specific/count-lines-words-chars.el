;;; Count lines, words and characters in buffer. -*- lexical-binding: t -*-

;; g C-g: count in buffer or region, if active.
(with-eval-after-load 'evil
  (evil-define-key 'motion 'global (kbd "g C-g") #'count-words))

(provide 'conf/evil-specific/count-lines-words-chars)
