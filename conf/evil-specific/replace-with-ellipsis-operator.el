;;; An operator for replacing text with "(...)". -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (evil-define-operator evil-replace-with-ellipsis (beg end type register yank-handler)
    "Replace text with an ellipsis."
    ;; Args and interactive spec are copied from the definition `evil-change' in `evil-operators.el'.
    (interactive "<R><x><y>")
    (evil-delete beg end type register yank-handler)
    (insert "(...)" (if (looking-at-p "[\\.,;[:space:]]") "" " ")))

  (evil-define-key '(normal visual) 'global (kbd "g .") #'evil-replace-with-ellipsis))

(provide 'conf/evil-specific/replace-with-ellipsis-operator)
