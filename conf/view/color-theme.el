;;; Color theme.

;; Create `after-load-theme-hook'.
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;(require 'conf/packages)
;;(package-ensure-installed 'solarized-theme)
;;(load-theme 'solarized-dark t)

(load-theme 'wombat)

;; Cursor color.
(set-cursor-color "navajo white")
(require 'conf/evil)
(setq evil-default-cursor nil)

(provide 'conf/view/color-theme)
