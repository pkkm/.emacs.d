;;; Highlight the line with the point.

(use-package hl-line ; Bundled with Emacs.
  :if (>= (display-color-cells) 16)
  :defer t
  :init
  (global-hl-line-mode 1)
  :config

  ;; Disable in some modes and when region is active.
  (defvar hl-line-disable-in-modes '(term-mode)
    "Modes in which the current line should not be highlighted.")
  (defadvice global-hl-line-highlight (around disable-in-modes-or-region activate)
    (unless (or (region-active-p)
                (memq major-mode hl-line-disable-in-modes))
      ad-do-it))

  ;; Don't allow themes to change other properties than the background.
  (defface my-hl-line-face `((t)) "My face for hl-line." :group 'hl-line)
  (defun my-hl-line-update-background ()
    (set-face-background 'my-hl-line-face (face-background 'hl-line nil t)))
  (my-hl-line-update-background)
  (add-hook 'after-load-theme-hook #'my-hl-line-update-background)
  (setq hl-line-face 'my-hl-line-face)

  ;; If we're running on a graphical display, make the highlight more pronounced in insert state, and less in other states.
  (require 'conf/utils/colors) ; Used: color-mix.
  (when (display-graphic-p)
    (with-eval-after-load 'evil
      (remove-hook 'after-load-theme-hook #'hl-line-update-background) ; We'll replace it with a more advanced version.

      (defvar hl-line-insert-background nil)
      (defvar hl-line-normal-background nil)

      (defun hl-line-update-backgrounds ()
        "Update the background colors for the line with point inside and outside Insert state."
        (setq hl-line-insert-background (face-background 'hl-line nil t))
        (setq hl-line-normal-background (color-mix (face-background 'hl-line nil t) 0.6
                                                   (face-background 'default nil t) 0.4)))
      (hl-line-update-backgrounds)
      (add-hook 'after-load-theme-hook #'hl-line-update-backgrounds)

      (defun set-hl-line-insert-background ()
        (set-face-background hl-line-face hl-line-insert-background))
      (defun set-hl-line-normal-background ()
        (set-face-background hl-line-face hl-line-normal-background))
      (add-hook 'evil-insert-state-entry-hook #'set-hl-line-insert-background)
      (add-hook 'evil-replace-state-entry-hook #'set-hl-line-insert-background)
      (add-hook 'evil-insert-state-exit-hook #'set-hl-line-normal-background)
      (add-hook 'evil-replace-state-exit-hook #'set-hl-line-normal-background)

      (defun set-hl-line-background ()
        (if (or (evil-insert-state-p)
                (evil-replace-state-p))
            (set-hl-line-insert-background)
          (set-hl-line-normal-background)))
      (set-hl-line-background)
      (add-hook 'after-load-theme-hook #'set-hl-line-background t))))

(provide 'conf/view/hl-current-line)
