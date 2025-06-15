;;; Highlight the line with the point. -*- lexical-binding: t -*-

(use-package hl-line ; Bundled with Emacs.
  :if (>= (display-color-cells) 16)
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

  ;; Don't allow themes to change properties other than the background. Apply theme-specific color tweaks.
  (require 'conf/utils/colors) ; Used: color-mix.
  (defface my-hl-line-face
    '((t :extend t))
    "My face for hl-line."
    :group 'hl-line)
  (defun my-hl-line-update-background ()
    (set-face-background
     'my-hl-line-face
     (cond
      ((memq 'sanityinc-tomorrow-bright custom-enabled-themes)
       (color-mix (face-background 'hl-line nil t) 0.5 "black" 0.5))
      ((memq 'sanityinc-tomorrow-night custom-enabled-themes)
       (color-mix (face-background 'hl-line nil t) 0.85 "black" 0.15))
      (t (face-background 'hl-line nil t)))))
  (my-hl-line-update-background)
  (add-hook 'after-load-theme-hook #'my-hl-line-update-background)
  (setq hl-line-face 'my-hl-line-face))

(provide 'conf/view/hl-current-line)
