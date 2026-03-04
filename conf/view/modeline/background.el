;;; Change the background of the modeline according to Evil state. -*- lexical-binding: t -*-

(with-eval-after-load 'evil
  (require 'conf/utils) ; Used: my-color-mix.

  (defvar my-evil-state-modeline-colors '()
    "An alist maping Evil state to modeline background color, e.g. '((normal \"#373b41\") (insert \"#2e325d\"))")
  (defvar my-evil-state-modeline-default-color (face-background 'mode-line-active nil t)
    "Modeline background color for Evil states that aren't in `my-evil-state-modeline-colors'.")

  ;; Calculate backgrounds for various states depending on color theme.
  (defun my-update-evil-state-modeline-colors (&rest _)
    "Calculate the modeline backgrounds for various Evil states."
    (let ((original-background (face-background 'mode-line-active nil t)))
      (setq my-evil-state-modeline-colors
            `((normal . ,original-background)
              (motion . ,original-background)
              (operator . ,original-background)
              (insert . ,(my-color-mix "blue" 0.2 original-background 0.8))
              (replace . ,(my-color-mix "blue" 0.2 original-background 0.8))
              (visual . ,(my-color-mix "green" 0.2 original-background 0.8))))
      (setq my-evil-state-modeline-default-color
            (my-color-mix "black" 0.55 original-background 0.45))))
  (add-hook 'enable-theme-functions #'my-update-evil-state-modeline-colors)
  (my-update-evil-state-modeline-colors)

  ;; Set the background when Evil state changes.
  (defun my-set-evil-state-modeline-color (&rest _)
    "Set the modeline background according to the current Evil state."
    (set-face-background 'mode-line-active
                         (alist-get evil-state
                                    my-evil-state-modeline-colors
                                    my-evil-state-modeline-default-color)))
  (add-hook 'post-command-hook #'my-set-evil-state-modeline-color)
  (add-hook 'enable-theme-functions #'my-set-evil-state-modeline-color t)) ; Append so that we run after `my-update-evil-state-modeline-colors'.

(provide 'conf/view/modeline/background)
