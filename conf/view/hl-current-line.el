;;; Highlight the line with the point. -*- lexical-binding: t -*-

(use-package hl-line ; Bundled with Emacs.
  :init
  (global-hl-line-mode 1)
  :config

  ;; Disable in terminal mode.
  (setq global-hl-line-modes '(not term-mode))

  ;; Disable when the region is active.
  (setq hl-line-range-function
        (lambda ()
          (unless (region-active-p)
            (cons (line-beginning-position)
                  (line-beginning-position 2)))))

  ;; Don't allow themes to change properties other than the background. Apply theme-specific color tweaks.
  (require 'conf/utils) ; Used: my-color-mix.
  (defface my-hl-line-face
    '((t :extend t))
    "My face for hl-line."
    :group 'hl-line)
  (defun my-hl-line-update-background (&rest _)
    (let ((base-bg (face-background 'hl-line nil t)))
      (set-face-background
       'my-hl-line-face
       (cond
        ((memq 'sanityinc-tomorrow-bright custom-enabled-themes)
         (color-darken-name base-bg 50))
        ((memq 'sanityinc-tomorrow-night custom-enabled-themes)
         (color-darken-name base-bg 15))
        (t base-bg)))))
  (my-hl-line-update-background)
  (add-hook 'enable-theme-functions #'my-hl-line-update-background)
  (setq hl-line-face 'my-hl-line-face))

(provide 'conf/view/hl-current-line)
