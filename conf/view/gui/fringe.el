;;; Fringe.

;; None on the left, 8 pixels wide on the right.
(set-fringe-mode '(0 . 0))

;; Invisible (same color as background).
;;(face-spec-reset-face 'fringe)
;;(set-face-attribute 'fringe nil :inherit 'default)

(provide 'conf/view/gui/fringe)
