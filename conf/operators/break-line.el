;;; Break line at point, continuing comment if within one.

(require 'conf/evil)

(define-key evil-normal-state-map (kbd "<C-return>") #'indent-new-comment-line)
(define-key evil-insert-state-map (kbd "<C-return>") #'indent-new-comment-line)
;; Works both in GUI and xterm.

(provide 'conf/operators/break-line)
