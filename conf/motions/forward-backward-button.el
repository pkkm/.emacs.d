;;; Make "g TAB" jump to the next button (link), like TAB does in read-only buffers by default (without Evil).

(require 'conf/evil)

(define-key evil-motion-state-map (kbd "g TAB") 'forward-button)
(define-key evil-motion-state-map (kbd "g <backtab>") 'backward-button) ; <backtab> is S-TAB.

(provide 'conf/motions/forward-backward-button)
