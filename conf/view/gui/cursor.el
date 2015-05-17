;;; Cursor (point).

(when (display-graphic-p)
  (blink-cursor-mode 0)
  (setq x-stretch-cursor t)) ; Stretch the cursor to cover wide characters (e.g. tabs).

;; Cursor color is set in view/color-theme.el.

(provide 'conf/view/gui/cursor)
