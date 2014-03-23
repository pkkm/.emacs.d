;;; Faces for various parts of the modeline.

(require 'conf/utils/colors) ; Used: color-mix.
(require 'conf/evil)

;;; "Base" faces.

;; Modeline backgrounds in various Evil states.
(defvar ml-evil-normal-state-background nil "Background for the active modeline when the Evil state is Normal.")
(defvar ml-evil-motion-state-background nil "Background for the active modeline when the Evil state is Motion.")
(defvar ml-evil-insert-state-background nil "Background for the active modeline when the Evil state is Insert.")
(defvar ml-evil-replace-state-background nil "Background for the active modeline when the Evil state is Replace.")
(defvar ml-evil-visual-state-background nil "Background for the active modeline when the Evil state is Visual.")
(defvar ml-evil-operator-state-background nil "Background for the active modeline when the Evil state is Operator.")
(defvar ml-evil-emacs-state-background nil "Background for the active modeline when the Evil state is Emacs.")
(defvar ml-evil-nil-state-background nil "Background for the active modeline when the Evil state is nil.")

(defun calculate-mode-line-backgrounds ()
  "Calculate the modeline backgrounds for various Evil states."
  (let ((original-background (face-background 'mode-line nil t)))
    (setq ml-evil-normal-state-background original-background)
    (setq ml-evil-motion-state-background original-background)
    (setq ml-evil-insert-state-background (color-mix "blue" 0.1 original-background 0.9))
    (setq ml-evil-replace-state-background (color-mix "blue" 0.1 original-background 0.9))
    (setq ml-evil-visual-state-background (color-mix "green" 0.1 original-background 0.9))
    (setq ml-evil-operator-state-background original-background)
    (setq ml-evil-emacs-state-background (color-mix "black" 0.4 original-background 0.6))
    (setq ml-evil-nil-state-background (color-mix "black" 0.4 original-background 0.6))))
(add-hook 'after-load-theme-hook #'calculate-mode-line-backgrounds)
(calculate-mode-line-backgrounds)

;; Set the modeline background when Evil state changes.
(require 'conf/utils/hooks) ; Used: add-hooks.
(require 'cl-lib)
(defun set-mode-line-background ()
  "Set the background of the `mode-line' face, according to the current Evil state."
  (set-face-background 'mode-line
                       (cl-case evil-state
                         ('normal ml-evil-normal-state-background)
                         ('motion ml-evil-motion-state-background)
                         ('insert ml-evil-insert-state-background)
                         ('replace ml-evil-replace-state-background)
                         ('visual ml-evil-visual-state-background)
                         ('operator ml-evil-operator-state-background)
                         ('emacs ml-evil-emacs-state-background)
                         (nil ml-evil-nil-state-background))))
(add-hook 'post-command-hook #'set-mode-line-background) ; Not ideal, but usually works.
(add-hook 'after-load-theme-hook #'set-mode-line-background t) ; t -- append, so that we run after `calculate-mode-line-backgrounds'.

;;; "Helper" faces to inherit from.

(defface ml-shadow `((t ())) ; Foreground: mix the foreground and background of 'ml-active (below).
  "Face for de-emphasized parts of the modeline."
  :group 'modeline)

(defun set-mode-line-helper-faces ()
  "Calculate the modeline \"helper\" faces that depend on colors in other faces."
  (set-face-foreground 'ml-shadow
                       (if window-system
                           (color-mix (face-foreground 'mode-line nil t) 0.6 (face-background 'mode-line nil t) 0.4)
                         (face-foreground 'mode-line nil t))))
(add-hook 'after-load-theme-hook #'set-mode-line-helper-faces)
(set-mode-line-helper-faces)

;;; Left.

(defface ml-directory '((t (:inherit mode-line-inactive)))
  "Face for the folder name in the modeline."
  :group 'modeline)

(defface ml-filename '((t ()))
  "Face for the file name in the modeline."
  :group 'modeline)

(defface ml-modified '((t (:inherit warning)))
  "Face for the indicator of file being modified in an active modeline."
  :group 'modeline)

(defface ml-read-only '((t (:weight bold)))
  "Face for the indicator of file being read only in the modeline."
  :group 'modeline)

(defface ml-narrowed '((t (:underline t)))
  "Face for the indicator of buffer being narrowed in the modeline."
  :group 'modeline)

;;; Center.

(defface ml-recursive-edit-braces '((t (:inherit ml-shadow)))
  "Face for the recursive edit braces in an active modeline."
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

(defface ml-position '((t ()))
  "Face for the buffer position in an active modeline."
  :group 'modeline)

(defface ml-line '((t ()))
  "Face for the current line in an active modeline."
  :group 'modeline)

(defface ml-column '((t ()))
  "Face for the current column in an active modeline."
  :group 'modeline)

(provide 'conf/modeline/faces)
