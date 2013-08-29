;;; Highlight the line with the point.

(require 'conf/evil)
(require 'conf/utils/colors) ; Used: color-mix.

(global-hl-line-mode 1)

(setq hl-line-sticky-flag nil) ; Highlight only in the active window.

;; Disable highlighting in visual state (to see the selection better).
(add-hook 'evil-visual-state-entry-hook (lambda () (global-hl-line-mode 0)))
(add-hook 'evil-visual-state-exit-hook (lambda () (global-hl-line-mode 1)))

;; Remove all attributes except background color from the highlight face.
;; (By creating a face with the background of `hl-line-face' and other attributes unset.)

;; Use my face for the line with point. Set its background dynamically, based on the 'hl-line face.
(defface my-hl-line-face `((t)) "My face for hl-line.")
(setq hl-line-face 'my-hl-line-face)

(if window-system
    ;; Make the highlight more pronounced in insert state, and less in other states.
    (progn
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
      (add-hook 'after-load-theme-hook #'set-hl-line-background t))
  ;; When in terminal: just set the background (color mixing won't work).
  (defun hl-line-update-background ()
    (set-face-background hl-line-face
                         (face-background 'hl-line nil t)))
  (hl-line-update-background)
  (add-hook 'after-load-theme-hook #'hl-line-update-background))

(provide 'conf/view/hl-current-line)
