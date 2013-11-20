;;; Color theme.

;; Create `after-load-theme-hook'.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;; Dark, with many greens and browns.
(load-theme 'wombat)
(set-cursor-color "navajo white")

;; Different colors for Org headline levels with the Wombat theme.
;; (Wombat has the same colors for Org headlines with levels 1 and 2, and also 6 and 7.)
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.
(add-one-shot-hook 'org-mode-hook #'my-org-fix-wombat-headlines)
(defun my-org-fix-wombat-headlines ()
  (setq org-level-faces
        '(org-level-1 org-level-3 org-level-4 org-level-5 org-level-6 org-level-8))
  (setq org-n-level-faces 6))

;; Light, beige.
;;(require 'conf/packages)
;;(package-ensure-installed 'soft-morning-theme)
;;(load-theme 'soft-morning t)

;; Cursor color.
(require 'conf/evil)
(setq evil-default-cursor nil)

(provide 'conf/view/color-theme)
