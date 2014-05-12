;;; Faces for various parts of the modeline.

(require 'conf/utils/colors) ; Used: color-mix.

;;; "Base" faces.

(defface ml-active-1
  '((t (:inherit mode-line))) ; Foreground and background: from the 'fringe face (set below).
  "Powerline face 1."
  :group 'powerline)

(defface ml-active-2
  '((t (:inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface ml-inactive-1
  '((t (:inverse-video t :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface ml-inactive-2
  '((t (:inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)

(defun set-mode-line-base-faces ()
  "Calculate the modeline \"base\" faces that depend on colors in other faces."
  (set-face-foreground 'ml-active-1
                       (face-foreground 'fringe nil t))
  (set-face-background 'ml-active-1
                       (face-background 'fringe nil t)))
(add-hook 'after-load-theme-hook #'set-mode-line-base-faces)
(set-mode-line-base-faces)

;;; "Helper" faces to inherit from.

(defface ml-shadow `((t ())) ; Foreground: mix the foreground and background of 'ml-active-1 (below).
  "Face for de-emphasized parts of the modeline."
  :group 'modeline)

(defun set-mode-line-helper-faces ()
  "Calculate the modeline \"helper\" faces that depend on colors in other faces."
  (set-face-foreground 'ml-shadow
                       (if (display-graphic-p)
                           (color-mix (or (face-foreground 'ml-active-1 nil t) (face-foreground 'default)) 0.6
                                      (or (face-background 'ml-active-1 nil t) (face-foreground 'default)) 0.4)
                         (face-foreground 'ml-active-1 nil t))))
(add-hook 'after-load-theme-hook #'set-mode-line-helper-faces t) ; Add to end (because `set-mode-line-base-faces' should run first).
(set-mode-line-helper-faces)

;;; Left.

(defface ml-directory-active '((t (:inherit ml-shadow)))
  "Face for the folder name in the modeline."
  :group 'modeline)
(defface ml-directory-inactive '((t ()))
  "Face for the folder name in the modeline."
  :group 'modeline)

(defface ml-filename '((t ()))
  "Face for the file name in the modeline."
  :group 'modeline)

(defface ml-modified-active '((t (:inherit warning)))
  "Face for the indicator of file being modified in an active modeline."
  :group 'modeline)
(defface ml-modified-inactive '((t (:weight bold)))
  "Face for the indicator of file being modified in an inactive modeline."
  :group 'modeline)

(defface ml-read-only '((t (:weight bold)))
  "Face for the indicator of file being read only in the modeline."
  :group 'modeline)

(defface ml-narrowed '((t ())) ; (:box t)
  "Face for the indicator of buffer being narrowed in the modeline."
  :group 'modeline)

;;; Center.

(defface ml-recursive-edit-braces-active '((t (:inherit ml-shadow)))
  "Face for the recursive edit braces in an active modeline."
  :group 'modeline)
(defface ml-recursive-edit-braces-inactive '((t ()))
  "Face for the recursive edit braces in an inactive modeline."
  :group 'modeline)

(defface ml-major-mode '((t ()))
  "Face for the major mode in the modeline."
  :group 'modeline)

(defface ml-process '((t ()))
  "Face for the process status in the modeline."
  :group 'modeline)

(defface ml-global-mode-string '((t ()))
  "Face for the global mode string (see help for global-mode-string) in the modeline."
  :group 'modeline)

(defface ml-evil-state '((t (:weight bold)))
  "Face for the evil state in the modeline."
  :group 'modeline)

(defface ml-minor-modes '((t ()))
  "Face for minor modes in the modeline."
  :group 'modeline)

;;; Right.

(defface ml-coding '((t ()))
  "Face for the coding system name in the modeline."
  :group 'modeline)

(defface ml-position-active '((t (:inherit ml-shadow)))
  "Face for the buffer position in an active modeline."
  :group 'modeline)
(defface ml-position-inactive '((t ()))
  "Face for the buffer position in an inactive modeline."
  :group 'modeline)

(defface ml-line-active '((t ()))
  "Face for the current line in an active modeline."
  :group 'modeline)
(defface ml-line-inactive '((t (:weight bold)))
  "Face for the current line in an inactive modeline."
  :group 'modeline)

(defface ml-column-active '((t (:inherit ml-shadow)))
  "Face for the current column in an active modeline."
  :group 'modeline)
(defface ml-column-inactive '((t ()))
  "Face for the current column in an inactive modeline."
  :group 'modeline)

(provide 'conf/modeline/faces)
