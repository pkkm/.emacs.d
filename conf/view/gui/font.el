;;; Font.

;; Default font.
(set-frame-font "DejaVu Sans Mono-9")

;; Variable-pitch font.
;; This will cause a segfault when executed in `emacs -nw'.
(set-face-font 'variable-pitch "DejaVu Sans-9.6")

;; Changing font size.
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(provide 'conf/view/gui/font)
