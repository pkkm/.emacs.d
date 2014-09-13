;;; Customizations for yanking.

(with-eval-after-load 'evil
  ;; Make "Y" yank to the end of line instead of yanking the whole line.
  (evil-define-operator evil-yank-to-eol (beg end type register yank-handler)
    "Yank to end of line."
    :motion evil-end-of-line
    (interactive "<R><x><y>")
    (evil-yank beg end type register yank-handler))
  (define-key evil-normal-state-map (kbd "Y") nil)
  (define-key evil-motion-state-map (kbd "Y") #'evil-yank-to-eol)

  ;; Make "y" also work in motion state.
  (define-key evil-motion-state-map (kbd "y") #'evil-yank))

(provide 'conf/operators/yank)
