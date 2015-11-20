;;; Configuration specific to graphical Emacs.

(when (display-graphic-p)


  ;;; Cursor (point).
  ;; (Cursor color is set in view/color-theme.el.)
  (blink-cursor-mode 0)
  (setq x-stretch-cursor t) ; Stretch the cursor to cover wide characters (e.g. tabs).


  ;;; Fringe.
  (set-fringe-mode '(0 . 0)) ; Disable both left and right fringes.

  ;; Invisible (same color as background).
  ;;(face-spec-reset-face 'fringe)
  ;;(set-face-attribute 'fringe nil :inherit 'default)


  ;;; Focus follows mouse.
  (setq mouse-autoselect-window t)


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


  ;;; Resize frames by pixels instead of characters.
  (setq frame-resize-pixelwise t)


  ;;; Frame title.

  (setq frame-title-format
        '("Emacs: %b " (:eval (when (buffer-modified-p) "+ "))
          "[" default-directory "]"))
  ;;(setq icon-title-format frame-title-format)


  ;;; Start Emacs maximized.

  (defun maximize-frame-if-visible ()
    "Maximize the current frame if it's visible (not minimized)."
    (unless (frame-parameter nil 'fullscreen)
      (toggle-frame-maximized)))

  (add-hook 'emacs-startup-hook #'maximize-frame-if-visible))


(provide 'conf/view/gui-specific)
