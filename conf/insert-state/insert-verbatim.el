;;; Insert a character verbatim.

(require 'conf/evil)

(define-key evil-insert-state-map (kbd "C-v") #'quoted-insert)
(define-key evil-replace-state-map (kbd "C-v") #'quoted-insert)

(provide 'conf/insert-state/insert-verbatim)
