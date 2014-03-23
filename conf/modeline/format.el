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

(defun append-face-in-text (text new-face)
  "Like (propertize text 'face new-face), but instead of overriding the previous face, appends to the list of faces."
  (let ((new-face-as-list (if (listp new-face)
                              new-face
                            (list new-face))))
    (alter-text-property 0 (length text) 'face
                         (lambda (face)
                           (cond
                            ((listp face) (append face new-face-as-list))
                            (face (append (list face) new-face-as-list))
                            (t new-face-as-list)))
                         text))
  text)

(package-ensure-installed 'dash) (require 'dash) ; Used: -interpose.
(defun interpose-with-spaces (&rest parts)
  "" ; TODO
  (let ((non-empty-parts (delq nil (delq "" parts))))
    (when non-empty-parts
      (-interpose " " non-empty-parts))))

(defun ml-format () ; TODO figure out a way to determine if we're rendering the modeline for the focused window, or an unfocused one.
  (let* (

         ;; Modeline sections.

         (ml-dir-and-name
          (concat
           (if buffer-file-name ; If the buffer is visiting a file...
               (propertize (shorten-directory default-directory 20)
                           'face 'ml-directory)
             "")
           (propertize "%b" 'face 'ml-filename)))

         (ml-modified-ro
          (cond (buffer-read-only
                 (propertize "RO" 'face 'ml-read-only))
                ((buffer-modified-p)
                 (propertize "+" 'face 'ml-modified))
                (t
                 nil)))

         (ml-is-narrowed
          (if (not (string= (format-mode-line "%n") ""))
              (propertize "Narrow" 'face 'ml-narrowed)
            nil))

         (ml-recursive-edit-open-braces
          (propertize "%[" 'face 'ml-recursive-edit-braces))

         (ml-recursive-edit-close-braces
          (propertize "%]" 'face 'ml-recursive-edit-braces))

         (ml-major-mode
          (propertize (format-mode-line mode-name)
                      'face 'ml-major-mode))

         (ml-minor-modes
          (if (not (string= (format-mode-line minor-mode-alist) ""))
              (propertize (substring (format-mode-line minor-mode-alist) 1)
                          'face 'ml-minor-modes)
            ""))

         (ml-process
          (propertize (format-mode-line mode-line-process)
                      'face 'ml-process))

         (ml-global-mode-string
          (propertize (format-mode-line global-mode-string)
                      'face 'ml-global-mode-string))

         (ml-coding
          (propertize (symbol-name buffer-file-coding-system)
                      'face 'ml-coding))

         (ml-position
          (propertize "%p" 'face 'ml-position))

         (ml-line-column
          (concat
           (propertize "%l" 'face 'ml-line)
           (propertize ":%c" 'face 'ml-column)))

         ;; Left, center and right parts of the modeline.

         (left (append
                (list " ") ; Spacing from the window edge.
                (interpose-with-spaces ml-dir-and-name
                                       ml-modified-ro
                                       ml-is-narrowed)))

         (center (interpose-with-spaces ml-recursive-edit-open-braces
                                        ml-major-mode
                                        ml-process
                                        ml-global-mode-string
                                        ml-minor-modes
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

(require 'conf/modeline/faces)
(setq-default mode-line-format
              '((:eval
                 (ignore-specific-messages '("pl/ generating new separator")
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
