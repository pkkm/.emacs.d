;;; Make the bell less annoying that either the audible or the visible one.

(use-package echo-bell
  :ensure t
  :init

  ;; Variables have to be set before the package is loaded (as of 2015-10).
  (setq echo-bell-string "")
  (setq echo-bell-background (face-foreground 'success nil t)) ; TODO update when color theme changes.
  (setq echo-bell-delay 0.05)
  (echo-bell-mode))

(provide 'conf/view/bell)
