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

;; Light, beige.
;;(require 'conf/packages)
;;(package-ensure-installed 'soft-morning-theme)
;;(load-theme 'soft-morning t)

;; Cursor color.
(require 'conf/evil)
(setq evil-default-cursor nil)

(provide 'conf/view/color-theme)
