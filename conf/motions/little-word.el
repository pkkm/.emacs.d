;;; "Little word" -- like `w', but for CamelCase and under_scores.

(require 'conf/evil)

(define-key evil-motion-state-map (kbd "C-w") 'evil-little-word)

(evil-define-motion evil-little-word (count)
  "Move by COUNT little words (words inside CamelCase or under_scores)."
  :type exclusive
  (let* ((case-fold-search nil)
         (count (if count count 1)))
    (dotimes (unused-counter count nil)
      (forward-char)
      (search-forward-regexp "[_A-Z]\\|\\W" nil t)
      (backward-char))))

(provide 'conf/motions/little-word)
