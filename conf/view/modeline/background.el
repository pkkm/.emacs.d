;;; Change the background of the modeline according to Evil state.

(use-package ht :ensure t :defer t)

(with-eval-after-load 'evil
  (require 'ht)
  (require 'conf/utils/colors) ; Used: color-mix.

  (defvar my-modeline-evil-state-to-background (ht)
    "A hash-table maping Evil state to modeline background color, e.g. (ht ('normal \"#373b41\") ('insert \"#2e325d\"))")
  (defvar my-modeline-other-evil-state-background (face-background 'mode-line nil t)
    "Modeline background color for Evil states that aren't in `my-modeline-evil-state-to-background'.")

  ;; Calculate backgrounds for various states depending on color theme.
  (defun calculate-mode-line-backgrounds ()
    "Calculate the modeline backgrounds for various Evil states."
    (let ((original-background (face-background 'mode-line nil t)))
      (setq my-modeline-evil-state-to-background
            (ht ('normal original-background)
                ('motion original-background)
                ('operator original-background)
                ('insert (color-mix "blue" 0.2 original-background 0.8))
                ('replace (color-mix "blue" 0.2 original-background 0.8))
                ('visual (color-mix "green" 0.2 original-background 0.8))))
      (setq my-modeline-other-evil-state-background
            (color-mix "black" 0.45 original-background 0.55))))
  (add-hook 'after-load-theme-hook #'calculate-mode-line-backgrounds)
  (calculate-mode-line-backgrounds)

  ;; Set the background when Evil state changes.
  (defun set-mode-line-background ()
    "Set the modeline background according to the current Evil state."
    (set-face-background 'mode-line
                         (ht-get my-modeline-evil-state-to-background
                                 evil-state
                                 my-modeline-other-evil-state-background)))
  (add-hook 'post-command-hook #'set-mode-line-background) ; Not ideal, but usually works.
  (add-hook 'after-load-theme-hook #'set-mode-line-background t)) ; Append so that we run after `calculate-mode-line-backgrounds'.

(provide 'conf/view/modeline/background)
