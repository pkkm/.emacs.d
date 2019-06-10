;;; Minor mode that decreases the height of blank lines. -*- lexical-binding: t -*-
;; TODO: feature request for <https://github.com/cpitclaudel/compact-docstrings> to integrate this functionality.

(defface shrink-blank-lines-face
  '((t :height 0.6))
  "Face applied to blank lines.")

(defconst shrink-blank-lines--keywords
  '(("^[ \t]*\n" 0 'shrink-blank-lines-face prepend)))

(define-minor-mode shrink-blank-lines-mode
  "Shrink blank lines."
  :lighter " ShrinkB"
  (if shrink-blank-lines-mode
      (font-lock-add-keywords nil shrink-blank-lines--keywords 'append)
    (font-lock-remove-keywords nil shrink-blank-lines--keywords))
  (font-lock-flush))

(provide 'conf/view/shrink-blank-lines)
