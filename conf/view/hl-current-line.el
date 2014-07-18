;;; Highlight the line with the point.

(require 'conf/evil)
(require 'conf/utils/colors) ; Used: color-mix.
(require 'hl-line) ; So that its faces can be customized.

;; Highlight only in the active window.
(setq hl-line-sticky-flag nil)

;; Don't highlight on displays with a small amount of colors.
(defvar my-hl-line-enabled (>= (display-color-cells) 16)
  "Non-nil if the current line should be highlighted.")

;; Enable hl-line-mode in all buffers, with the exception of some modes.
(defvar hl-line-disable-in-modes
  '(minibuffer-inactive-mode term-mode)
  "A list of modes in which the current line should not be highlighted.")
(add-hook 'after-change-major-mode-hook #'my-hl-line-maybe-enable)
(defun my-hl-line-maybe-enable ()
  "Enable `hl-line-mode' unless the current mode is in `hl-line-disable-in-modes' or `my-hl-line-enabled' is nil."
  (when (and my-hl-line-enabled
             (not (member major-mode hl-line-disable-in-modes)))
    (hl-line-mode 1)))

;; Disable highlighting in visual state (to see the selection better).
(defvar hl-line-before-entering-visual-state nil
  "t if hl-line-mode was enabled before entering visual state, nil otherwise.")
(make-variable-buffer-local 'hl-line-before-entering-visual-state)
(add-hook 'evil-visual-state-entry-hook #'my-hl-line-deactivate)
(defun my-hl-line-deactivate ()
  (setq hl-line-before-entering-visual-state hl-line-mode)
  (hl-line-mode -1))
(add-hook 'evil-visual-state-exit-hook #'my-hl-line-activate-if-was-active)
(defun my-hl-line-activate-if-was-active ()
  (when hl-line-before-entering-visual-state (hl-line-mode 1)))

;; Use my face for the line with point. Set its background dynamically, based on the 'hl-line face.
(defface my-hl-line-face `((t)) "My face for hl-line." :group 'hl-line)
(setq hl-line-face 'my-hl-line-face)

(if (display-graphic-p)
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
