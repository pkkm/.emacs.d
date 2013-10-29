;;; Count lines, words and characters in buffer.

;; g C-g: count in buffer or region, if active.
(define-key evil-motion-state-map (kbd "g C-g") #'count-words)

(provide 'conf/other/count-lines-words-chars)
