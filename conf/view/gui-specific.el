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
  (setq icon-title-format frame-title-format) ; Don't change the title when minimized.


  ;;; Fringe.

  (set-fringe-mode '(8 . 0)) ; Disable right fringe.

  ;; Same color as the mode line.
  (defun my-update-fringe-color (&rest _)
    (set-face-background 'fringe (face-background 'mode-line-active nil t)))
  (add-hook 'enable-theme-functions #'my-update-fringe-color)
  (my-update-fringe-color)


  ;;; Font.

  (defun my-find-available-font (fonts)
    "Return the first available font among the font names in FONTS."
    (seq-find (lambda (font) (find-font (font-spec :name font))) fonts))

  (defun my-set-available-font (face fonts)
    "Set FACE's font to the first available of FONTS. If none is available, do nothing."
    (when-let ((font (my-find-available-font fonts)))
      (set-face-font face font)))

  ;; Try also: Roboto Mono (the winning font when I tried <https://www.codingfont.com/> in 2025-01).

  (my-set-available-font 'default '("DejaVu Sans Mono-10" "Consolas-10.5" "Courier New-10"))
  (my-set-available-font 'variable-pitch '("DejaVu Sans-10" "Verdana-10" "Helvetica-10" "Arial-10")))


(provide 'conf/view/gui-specific)
