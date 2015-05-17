;;; Font.


(defun first-available-font (fonts)
  "Return the first available font among the font names in FONTS."
  (--first (find-font (font-spec :name it)) fonts))

(defun set-first-available-font (face fonts)
  "Set FACE's font to the first available of FONTS. If none is available, do nothing."
  (-if-let (font (first-available-font fonts))
      (set-face-font face font)))

(set-first-available-font 'default '("DejaVu Sans Mono-9" "Consolas-10" "Courier New-9.5"))
(set-first-available-font 'variable-pitch '("DejaVu Sans-9.6" "Verdana" "Helvetica" "Arial")) ; Causes segfault in `emacs -nw'.

;; Changing font size.
(bind-key "C-+" #'text-scale-increase)
(bind-key "C--" #'text-scale-decrease)

(provide 'conf/view/gui/font)
