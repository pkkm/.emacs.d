;;; Indentation guides.

(require 'conf/packages)
(require 'conf/utils/colors) ; Used: color-mix.

(use-package indent-guide
  :ensure indent-guide
  :diminish indent-guide-mode
  :commands indent-guide-mode
  :config

  ;; Use a color close to the background.
  (face-spec-set 'indent-guide-face `((t)) 'face-defface-spec)
  (defun update-indent-guide-face ()
    (set-face-foreground 'indent-guide-face
                         (color-mix (face-foreground 'shadow nil t) 0.4
                                    (face-background 'default nil t) 0.6)))
  (add-hook 'after-load-theme-hook #'update-indent-guide-face)
  (update-indent-guide-face)

  (setq indent-guide-char "│")) ; Useful characters: ·│┆┊╎

(provide 'conf/view/indent-guides)
