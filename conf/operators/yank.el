;;; Customizations for yanking.

(with-eval-after-load 'evil
  ;; Make "Y" yank to the end of line instead of yanking the whole line.
  (evil-define-operator evil-yank-to-eol (beg end type register yank-handler)
    "Yank to end of line."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-yank beg end type register yank-handler))
  (bind-key "Y" nil evil-normal-state-map)
  (bind-key "Y" #'evil-yank-to-eol evil-motion-state-map)

  ;; Make "y" also work in motion state.
  (bind-key "y" #'evil-yank evil-motion-state-map))

(provide 'conf/operators/yank)
