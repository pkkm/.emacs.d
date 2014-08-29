;;; Color theme.

;; Create `after-load-theme-hook'.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(when (>= (display-color-cells) 16)
  ;; Ample theme -- dark, mainly blues and greens.
  ;;(use-package ample-theme
  ;;  :ensure ample-theme
  ;;  :config
  ;;  (load-theme 'ample t))

  ;; Deeper Blue theme -- dark, with light blues.
  ;; Installed in `my-vendor-dir'.
  ;;(load-theme 'deeper-blue)

  ;; Soft Morning theme -- light, beige.
  ;;(use-package soft-morning-theme
  ;;  :ensure soft-morning-theme
  ;;  :config
  ;;  (load-theme 'soft-morning t))

  ;; Wombat theme -- dark, with many greens and browns.
  (load-theme 'wombat)
  (set-cursor-color "navajo white")

  ;; Different colors for Org headline levels with the Wombat theme.
  ;; (Wombat has the same colors for Org headlines with levels 1 and 2, and also 6 and 7.)
  (with-eval-after-load 'org
    (setq org-level-faces
          '(org-level-1 org-level-3 org-level-4 org-level-5 org-level-6 org-level-8))
    (setq org-n-level-faces 6)))

;; Don't let Evil set the cursor color.
(with-eval-after-load 'evil
  (setq evil-default-cursor nil))

(provide 'conf/view/color-theme)
