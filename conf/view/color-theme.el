;;; Color theme.

;; Create `after-load-theme-hook'.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(when (>= (display-color-cells) 16)
  ;; Ample theme -- dark, mainly blues and greens.
  ;;(require 'conf/packages)
  ;;(package-ensure-installed 'ample-theme)
  ;;(add-to-list 'custom-safe-themes "ed81411169b1b3e3d4cfc39b09d68ea13e0ff7708dc5b9d0bedb319e071968ad")
  ;;(load-theme 'ample)

  ;; Deeper Blue theme -- dark, with light blues.
  ;; Installed in `my-vendor-dir'.
  ;;(load-theme 'deeper-blue)

  ;; Soft Morning theme -- light, beige.
  ;;(require 'conf/packages)
  ;;(package-ensure-installed 'soft-morning-theme)
  ;;(add-to-list 'custom-safe-themes "a30d5f217d1a697f6d355817ac344d906bb0aae3e888d7abaa7595d5a4b7e2e3")
  ;;(load-theme 'soft-morning t)

  ;; Wombat theme -- dark, with many greens and browns.
  (load-theme 'wombat)
  (set-cursor-color "navajo white")

  ;; Different colors for Org headline levels with the Wombat theme.
  ;; (Wombat has the same colors for Org headlines with levels 1 and 2, and also 6 and 7.)
  (require 'conf/utils/hooks) ; Used: add-one-shot-hook.
  (add-one-shot-hook 'org-mode-hook #'my-org-fix-wombat-headlines)
  (defun my-org-fix-wombat-headlines ()
    (setq org-level-faces
          '(org-level-1 org-level-3 org-level-4 org-level-5 org-level-6 org-level-8))
    (setq org-n-level-faces 6)))

;; Don't let Evil set the cursor color.
(require 'conf/evil)
(setq evil-default-cursor nil)

(provide 'conf/view/color-theme)
