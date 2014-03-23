;;; Indentation guides.

(require 'conf/packages)
(package-ensure-installed 'indent-guide)

;; We need to load now instead of autoloading, because otherwise `diminish' won't work.
(require 'indent-guide)
;;(autoload 'indent-guide-global-mode "indent-guide")
;;(autoload 'indent-guide-mode "indent-guide")

(require 'conf/modeline/cleaner-minor-modes)
(diminish 'indent-guide-mode "IG")

(face-spec-set 'indent-guide-face `((t)) 'face-defface-spec)
(defun update-indent-guide-face ()
  (set-face-foreground 'indent-guide-face
                       (face-foreground 'shadow nil t)))
(add-hook 'after-load-theme-hook #'update-indent-guide-face)
(update-indent-guide-face)

(setq indent-guide-char "·")

(provide 'conf/view/indent-guides)
