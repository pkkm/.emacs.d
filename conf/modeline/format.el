;;; Modeline with left, center and right aligned parts (using powerline).

(require 'conf/evil) ; To display the current state.
(require 'conf/packages)
(require 'conf/utils/ignore-messages) ; Used: ignore-specific-messages.

(package-ensure-installed 'powerline)

;; Separators. Nice ones: alternate, slant, rounded, contour.
(defalias 'ml-separator-left 'powerline-nil-right)
(defalias 'ml-separator-right 'powerline-nil-left)
(add-hook 'after-load-theme-hook #'powerline-reset t) ; Reset separator colors after a theme has been loaded.

(defun append-to-face-property (text new-face)
  (alter-text-property 0 (length text) 'face
                       (lambda (face)
                         (cond
                          ((listp face) (append face (list new-face)))
                          (face (list face new-face))
                          (t new-face)))
                       text))

(require 'cl-lib)
(package-ensure-installed 'dash) (require 'dash) ; Used: -interpose.
;; TODO document this, write comments (and possibly `let's).
(defun ml-make-segment (face sep-padding-face separator padding &rest parts)
  (let ((non-empty-parts (delq nil (delq "" parts))))
    (if non-empty-parts
        (append (when (member padding '(left both))
                 (list (propertize " " 'face sep-padding-face)))
               (when (member separator '(left both))
                 (list (ml-separator-left sep-padding-face face)
                       (propertize " " 'face face)))
               (mapcar (lambda (part)
                         (append-to-face-property part face)
                         part)
                       (-interpose (propertize " " 'face face)
                                   non-empty-parts))
               (when (member separator '(right both))
                 (list (propertize " " 'face face)
                       (ml-separator-right face sep-padding-face)))
               (when (member padding '(right both))
                 (list (propertize " " 'face sep-padding-face))))
      nil)))

(defun ml-format ()
  (let* ((window-active (powerline-selected-window-active))

         ;; "Base" faces.

         (ml-face-1
          (if window-active
              'ml-active-1
            'ml-inactive-1))

         (ml-face-2
          (if window-active
              'ml-active-2
            'ml-inactive-2))

         ;; Modeline sections.

         (ml-dir-and-name
          (concat
           (if buffer-file-name ; If the buffer is visiting a file...
               (propertize (shorten-directory default-directory 20)
                           'face (if window-active
                                     'ml-directory-active
                                   'ml-directory-inactive))
             "")
           (propertize "%b" 'face 'ml-filename)))

         (ml-modified-ro
          (cond (buffer-read-only
                 (propertize "RO" 'face 'ml-read-only))
                ((buffer-modified-p)
                 (propertize "+" 'face (if window-active
                                           'ml-modified-active
                                         'ml-modified-inactive)))
                (t
                 nil)))

         (ml-is-narrowed
          (if (not (string= (format-mode-line "%n") ""))
              (propertize "Narrow" 'face 'ml-narrowed)
            nil))

         (ml-recursive-edit-open-braces
          (propertize "%[" 'face (if window-active
                                     'ml-recursive-edit-braces-active
                                   'ml-recursive-edit-braces-inactive)))

         (ml-recursive-edit-close-braces
          (propertize "%]" 'face (if window-active
                                     'ml-recursive-edit-braces-active
                                   'ml-recursive-edit-braces-inactive)))

         (ml-evil-state
          (propertize (capitalize (symbol-name evil-state))
                      'face 'ml-evil-state))

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
          (propertize "%p" 'face (if window-active
                                     'ml-position-active
                                   'ml-position-inactive)))

         (ml-line-column
          (concat
           (propertize "%l" 'face (if window-active
                                      'ml-line-active
                                    'ml-line-inactive))
           (propertize ":%c" 'face (if window-active
                                       'ml-column-active
                                     'ml-column-inactive))))

         ;; Left, center and right parts of the modeline.

         (left (append
                (list (propertize " " 'face ml-face-1)) ; Spacing from the window edge.
                (ml-make-segment ml-face-1 ml-face-2 'right nil
                                 ml-dir-and-name)
                (ml-make-segment ml-face-2 ml-face-2 nil 'left
                                 ml-modified-ro
                                 ml-is-narrowed)))

         (center (append
                  (ml-make-segment ml-face-2 ml-face-2 nil 'right
                                   ml-recursive-edit-open-braces
                                   ml-major-mode
                                   ml-process
                                   ml-global-mode-string)
                  (ml-make-segment ml-face-1 ml-face-2 'both nil
                                   ml-evil-state)
                  (ml-make-segment ml-face-2 ml-face-2 nil 'left
                                   ml-minor-modes
                                   ml-recursive-edit-close-braces)))

         (right (append
                 (ml-make-segment ml-face-2 ml-face-2 nil 'right
                                  ml-coding)
                 (ml-make-segment ml-face-1 ml-face-2 'left nil
                                  ml-position
                                  ml-line-column)
                 (list (propertize " " 'face ml-face-1))))) ; Spacing from the window edge.

    ;; Rendering the modeline.
    (concat
     (powerline-render left)
     (powerline-fill-center ml-face-2 (/ (powerline-width center) 2.0))
     (powerline-render center)
     (powerline-fill ml-face-2 (powerline-width right))
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
