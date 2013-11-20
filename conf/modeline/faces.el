;;; Faces for various parts of the modeline.

(require 'conf/utils/colors) ; Used: color-mix.
(require 'conf/utils/hooks) ; Used: add-hooks.
(require 'conf/evil)

;;; "Base" faces.

(defface ml-active
  '((t (:inherit mode-line)))
  "Base face for the active modeline."
  :group 'modeline)

(defface ml-inactive
  '((t (:inherit mode-line-inactive)))
  "Base face for the inactive modeline."
  :group 'modeline)

;; Active modeline in various Evil states.
(defface ml-active-evil-normal-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Normal." :group 'modeline)
(defface ml-active-evil-motion-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Motion." :group 'modeline)
(defface ml-active-evil-insert-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Insert." :group 'modeline)
(defface ml-active-evil-replace-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Replace." :group 'modeline)
(defface ml-active-evil-visual-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Visual." :group 'modeline)
(defface ml-active-evil-operator-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Operator." :group 'modeline)
(defface ml-active-evil-emacs-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is Emacs." :group 'modeline)
(defface ml-active-evil-nil-state '((t (:inherit 'ml-active)))
  "Face for the active modeline when the Evil state is nil." :group 'modeline)

(require 'cl-lib)
(defun set-mode-line-backgrounds ()
  "Calculate the modeline backgrounds for various Evil states."
  (set-face-background 'ml-active-evil-normal-state nil)
  (set-face-background 'ml-active-evil-motion-state nil)
  (set-face-background 'ml-active-evil-insert-state (color-mix "blue" 0.1 (face-background 'mode-line nil t) 0.9))
  (set-face-background 'ml-active-evil-replace-state (color-mix "blue" 0.1 (face-background 'mode-line nil t) 0.9))
  (set-face-background 'ml-active-evil-visual-state (color-mix "green" 0.1 (face-background 'mode-line nil t) 0.9))
  (set-face-background 'ml-active-evil-operator-state nil)
  (set-face-background 'ml-active-evil-emacs-state (color-mix "black" 0.4 (face-background 'mode-line nil t) 0.6))
  (set-face-background 'ml-active-evil-nil-state (color-mix (face-background 'fringe nil t) 0.7 (face-background 'mode-line nil t) 0.3)))
(add-hook 'after-load-theme-hook #'set-mode-line-backgrounds)
(set-mode-line-backgrounds)

;;; "Helper" faces to inherit from.

(defface ml-shadow `((t ())) ; Foreground: mix the foreground and background of 'ml-active (below).
  "Face for de-emphasized parts of the modeline."
  :group 'modeline)

(defun set-mode-line-helper-faces ()
  "Calculate the modeline \"helper\" faces that depend on colors in other faces."
  (set-face-foreground 'ml-shadow
                       (if window-system
                           (color-mix (or (face-foreground 'mode-line nil t) (face-foreground 'default)) 0.6
                                      (or (face-background 'mode-line nil t) (face-foreground 'default)) 0.4)
                         (face-foreground 'mode-line nil t))))
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

(defface ml-narrowed '((t (:underline t)))
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
