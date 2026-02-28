;;; "Little word" -- like `w', but for CamelCase and under_scores. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (autoload 'subword-forward "subword")

  (evil-define-motion evil-little-word (count)
    "Move by COUNT little words (CamelCase, snake_case, or numbers)."
    :type exclusive
    (let ((count (or count 1)))
      (subword-forward count)))

  (evil-define-key 'motion 'global (kbd "C-w") #'evil-little-word))

(provide 'conf/evil-specific/little-word)
