;;; Indentation guides.

(require 'conf/packages)
(package-ensure-installed 'indent-guide)

;; We need to load now instead of autoloading, because otherwise `diminish' won't work.
(require 'indent-guide)
;;(autoload 'indent-guide-global-mode "indent-guide")
;;(autoload 'indent-guide-mode "indent-guide")

;; Don't display this minor mode in the modeline.
(require 'conf/modeline/cleaner-minor-modes)
(diminish 'indent-guide-mode)

;; Use a color close to the background.
(require 'conf/utils/colors)
(face-spec-set 'indent-guide-face `((t)) 'face-defface-spec)
(defun update-indent-guide-face ()
  (set-face-foreground 'indent-guide-face
                       (color-mix (face-foreground 'shadow nil t) 0.4
                                  (face-background 'default nil t) 0.6)))
(add-hook 'after-load-theme-hook #'update-indent-guide-face)
(update-indent-guide-face)

(setq indent-guide-char "│") ; Useful characters: ·│┆┊╎

(provide 'conf/view/indent-guides)
