;;; Font.

(use-package dash :ensure dash)

(defun first-available-font (&rest fonts)
  "Return the first available font among the font names in FONTS."
  (--first (find-font (font-spec :name it)) fonts))

;; Default font.
(let ((available-font (first-available-font
                       "DejaVu Sans Mono-9" "Consolas-10" "Courier New-9.5")))
  (when available-font
    (set-face-font 'default available-font)))

;; Variable-pitch font.
;; This will cause a segfault when executed in `emacs -nw'.
(let ((available-font (first-available-font
                       "DejaVu Sans-9.6" "Verdana" "Helvetica" "Arial")))
  (when available-font
    (set-face-font 'variable-pitch available-font)))

;; Changing font size.
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(provide 'conf/view/gui/font)
