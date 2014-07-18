;;; Modeline with left, center and right aligned parts (using powerline).

(require 'conf/evil) ; To display the current state.
(require 'conf/packages)
(require 'conf/utils/ignore-messages) ; Used: ignore-specific-messages.
(require 'cl-lib)

(package-ensure-installed 'powerline)

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

(defun ml-format () ; TODO figure out a way to determine if we're rendering the modeline for the focused window, or an unfocused one.
  (let* (

         ;; Modeline sections.

         (shortened-dir
          (when buffer-file-name ; If the buffer is visiting a file...
            (propertize (shorten-directory default-directory 20)
                        'face 'ml-shadow)))

         (ml-dir-and-name
          (concat (or shortened-dir "") "%b"))

         (ml-read-only
          (when buffer-read-only
            (propertize "RO" 'face '(:weight bold))))

         (ml-modified
          (when (buffer-modified-p)
            (propertize "+" 'face 'warning)))

         (ml-is-narrowed
          (when (buffer-narrowed-p)
            (propertize "Narrow" 'face '(:underline t))))

         (ml-recursive-edit-open-braces
          (propertize "%[" 'face 'ml-shadow))

         (ml-modes (format-mode-line '(""
                                       mode-name
                                       mode-line-process
                                       minor-mode-alist)))

         (ml-recursive-edit-close-braces
          (propertize "%]" 'face 'ml-shadow))

         (ml-global-mode-string
          (format-mode-line global-mode-string))

         (ml-coding
          ;; Hide the encoding if it is or will be turned into utf-8-unix.
          (unless (memq buffer-file-coding-system
                        '(utf-8-unix prefer-utf-8-unix undecided-unix))
            (symbol-name buffer-file-coding-system)))

         (ml-position "%p")

         (ml-line-column "%l:%c")

         ;; Left, center and right parts of the modeline.

         (left (append
                (list " ") ; Spacing from the window edge.
                (interpose-with-spaces ml-dir-and-name
                                       ml-read-only
                                       ml-modified
                                       ml-is-narrowed)))

         (center (interpose-with-spaces ml-recursive-edit-open-braces
                                        ml-modes
                                        ml-global-mode-string
                                        ml-recursive-edit-close-braces))

         (right (append
                 (interpose-with-spaces ml-coding
                                        ml-position
                                        ml-line-column)
                 (list " ")))) ; Spacing from the window edge.

    ;; Rendering the modeline.
    (concat
     (powerline-render left)
     (powerline-fill-center nil (/ (powerline-width center) 2.0))
     (powerline-render center)
     (powerline-fill nil (powerline-width right))
     (powerline-render right))))

(setq-default mode-line-format
              '((:eval (ignore-specific-messages
                        '("pl/ generating new separator")
                        (ml-format)))))

;; TODO improve this and move to conf/utils.
(defun shorten-directory (dir max-length)
  "Shorten directory name DIR to up to MAX-LENGTH characters."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(provide 'conf/modeline/format)
