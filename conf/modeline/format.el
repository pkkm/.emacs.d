;;; Modeline with left, center and right aligned parts (using powerline).

(use-package powerline
  :ensure powerline
  :demand t
  :config

  ;; Separators. Nice ones: alternate, slant, rounded, contour.
  (defalias 'ml-separator-left 'powerline-nil-right)
  (defalias 'ml-separator-right 'powerline-nil-left)
  (add-hook 'after-load-theme-hook #'powerline-reset t) ; Reset separator colors after a theme has been loaded.

  (package-ensure-installed 'dash) (require 'dash) ; Used: -interpose.
  (defun interpose-with-spaces (&rest parts)
    "Delete empty elements from PARTS, and separate the rest with \" \"."
    (let ((non-empty-parts (delq nil (delq "" parts))))
      (when non-empty-parts
        (-interpose " " non-empty-parts))))

  ;; "Helper" face for less important parts of the modeline.
  (defface ml-shadow `((t ())) ; Foreground of `mode-line-inactive'.
    "Face for de-emphasized parts of the modeline."
    :group 'modeline)
  (defun set-mode-line-helper-faces ()
    "Calculate the modeline \"helper\" faces that depend on colors in other faces."
    (set-face-foreground 'ml-shadow (face-foreground 'mode-line-inactive)))
  (add-hook 'after-load-theme-hook #'set-mode-line-helper-faces)
  (set-mode-line-helper-faces)

  (require 'conf/utils/paths) ; Used: shorten-path.
  (defun ml-format ()
    (let* ((shortened-dir
            (when buffer-file-name ; If the buffer is visiting a file...
              (propertize (shorten-path default-directory 20) ; TODO replace the hardcoded 20 with 1/3 of the modeline width or something similar.
                          'face 'ml-shadow)))

           (ml-modes (format-mode-line '(""
                                         mode-name
                                         mode-line-process
                                         minor-mode-alist)))

           (ml-coding
            ;; Hide the encoding if it is or will be turned into utf-8-unix.
            (unless (memq buffer-file-coding-system
                          '(utf-8-unix prefer-utf-8-unix undecided-unix))
              (symbol-name buffer-file-coding-system)))

           (left (append
                  (list " ") ; Spacing.
                  (interpose-with-spaces
                   (concat (or shortened-dir "") "%b") ; Directory and buffer name.
                   (when buffer-read-only (propertize "RO" 'face '(:weight bold))) ; Read only?
                   (when (buffer-modified-p) (propertize "+" 'face 'warning)) ; Modified?
                   (when (buffer-narrowed-p) (propertize "Narrow" 'face '(:underline t)))))) ; Narrowed?

           (center (interpose-with-spaces
                    (propertize "%[" 'face 'ml-shadow) ; Recursive edit braces.
                    ml-modes ; Major and minor modes.
                    (format-mode-line global-mode-string) ; Used for example by `display-time'.
                    (propertize "%]" 'face 'ml-shadow))) ; Recursive edit braces.

           (right (append
                   (interpose-with-spaces
                    ml-coding ; Coding system (empty if utf-8-unix).
                    "%p" ; Position (e.g. "56%" or "All").
                    "%l:%c") ; Line and column.
                   (list " ")))) ; Spacing.

      ;; Render the modeline.
      (concat
       (powerline-render left)
       (powerline-fill-center nil (/ (powerline-width center) 2.0))
       (powerline-render center)
       (powerline-fill nil (powerline-width right))
       (powerline-render right))))

  (require 'conf/utils/ignore-messages) ; Used: ignore-specific-messages.
  (setq-default mode-line-format
                '((:eval (ignore-specific-messages '("pl/ generating new separator")
                                                   (ml-format))))))

(provide 'conf/modeline/format)
