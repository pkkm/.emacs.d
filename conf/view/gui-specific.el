;;; Configuration specific to graphical Emacs. -*- lexical-binding: t -*-

(when (display-graphic-p)

  ;; Cursor (point).
  ;; (Cursor color is set in view/color-theme.el.)
  (blink-cursor-mode 0)
  (setq x-stretch-cursor t) ; Stretch the cursor to cover wide characters (e.g. tabs).

  ;; Resize frames by pixels instead of characters.
  (setq frame-resize-pixelwise t)

  ;; Focus follows mouse.
  (setq mouse-autoselect-window t)

  ;; Frame title.
  (setq frame-title-format
        '("Emacs: %b " (:eval (when (buffer-modified-p) "+ "))
          "[" default-directory "]"))
  (setq icon-title-format frame-title-format)) ; Don't change the title when minimized.


  ;;; Fringe.
  (set-fringe-mode '(0 . 0)) ; Disable both left and right fringes.

  ;; Same color as the background.
  ;;(face-spec-reset-face 'fringe)
  ;;(set-face-attribute 'fringe nil :inherit 'default)


  ;;; Font.

  (defun first-available-font (fonts)
    "Return the first available font among the font names in FONTS."
    (--first (find-font (font-spec :name it)) fonts))

  (defun set-first-available-font (face fonts)
    "Set FACE's font to the first available of FONTS. If none is available, do nothing."
    (-if-let (font (first-available-font fonts))
        (set-face-font face font)))

  (set-first-available-font 'default '("DejaVu Sans Mono-10" "Consolas-10.5" "Courier New-10"))
  (set-first-available-font 'variable-pitch '("DejaVu Sans-10" "Verdana-10" "Helvetica-10" "Arial-10")) ; Causes segfault in `emacs -nw'.


(provide 'conf/view/gui-specific)
