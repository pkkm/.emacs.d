;;; Misc. tweaks that are too small for their own file.

;; Show character names in `C-x =`.
(when (version<= "27.1" emacs-version)
  (setq what-cursor-show-names t))

(provide 'conf/misc)
