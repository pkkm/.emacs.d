;;; Make "Y" yank to the end of line instead of yanking the whole line.

(require 'conf/evil)

(evil-define-operator evil-yank-to-eol (beg end type register yank-handler)
  "Yank to end of line."
  :motion evil-end-of-line
  (interactive "<R><x><y>")
  (evil-yank beg end type register yank-handler))

(define-key evil-motion-state-map (kbd "Y") #'evil-yank-to-eol)

(provide 'conf/operators/yank-to-eol)
