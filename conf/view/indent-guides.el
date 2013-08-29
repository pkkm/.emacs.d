;;; Indentation guides.

(require 'conf/packages)
(package-ensure-installed 'indent-guide)

(face-spec-set 'indent-guide-face `((t)) 'face-defface-spec)
(defun update-indent-guide-face ()
  (set-face-foreground 'indent-guide-face
                       (face-foreground 'shadow nil t)))
(add-hook 'after-load-theme-hook #'update-indent-guide-face)
(update-indent-guide-face)

(setq indent-guide-char "·")

(autoload 'indent-guide-global-mode "indent-guide")
(indent-guide-global-mode)

(provide 'conf/view/indent-guides)
