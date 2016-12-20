;;; Modeline with left, center and right aligned parts (using powerline). -*- lexical-binding: t -*-

(use-package powerline
  :ensure t
  :demand t
  :config

  ;; Separators. Nice ones: alternate, slant, rounded, contour.
  (defalias 'ml-separator-left 'powerline-nil-right)
  (defalias 'ml-separator-right 'powerline-nil-left)
  (add-hook 'after-load-theme-hook #'powerline-reset t) ; Reset separator colors after a theme has been loaded.

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
  (require 'conf/utils/lists) ; Used: interpose-nonempty.
  (defun ml-format ()
    (let* ((shortened-dir
            (when buffer-file-name ; If the buffer is visiting a file...
              (propertize (shorten-path (abbreviate-file-name (file-name-directory buffer-file-name))
                                        (/ (window-total-width) 3))
                          'face 'ml-shadow)))

           (ml-coding
            ;; Hide the encoding if it is or will be turned into utf-8-unix.
            (unless (memq buffer-file-coding-system
                          '(utf-8-unix prefer-utf-8-unix undecided-unix))
              (symbol-name buffer-file-coding-system)))

           (left (append
                  (list " ") ; Spacing.
                  (interpose-nonempty " "
                    (concat (or shortened-dir "") "%b") ; Directory and buffer name.
                    (when buffer-read-only (propertize "RO" 'face '(:weight bold))) ; Read only?
                    (when (buffer-modified-p) (propertize "+" 'face 'warning)) ; Modified?
                    (when (buffer-narrowed-p) (propertize "Narrow" 'face '(:underline t)))))) ; Narrowed?

           (center (interpose-nonempty " "
                     (propertize "%[" 'face 'ml-shadow) ; Recursive edit braces.
                     (format-mode-line '(""
                                         mode-name
                                         mode-line-process
                                         minor-mode-alist))
                     (format-mode-line global-mode-string) ; Used for example by `display-time'.
                     (propertize "%]" 'face 'ml-shadow))) ; Recursive edit braces.

           (right (append
                   (interpose-nonempty " "
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

(provide 'conf/view/modeline/format)
