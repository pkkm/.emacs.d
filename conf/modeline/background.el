;;; Change the background of the modeline according to Evil state.

(with-eval-after-load 'evil
  (require 'conf/utils/colors) ; Used: color-mix.

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
  (require 'cl-lib) ; Used: cl-case.
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
  (add-hook 'after-load-theme-hook #'set-mode-line-background t)) ; t -- append, so that we run after `calculate-mode-line-backgrounds'.

(provide 'conf/modeline/background)
