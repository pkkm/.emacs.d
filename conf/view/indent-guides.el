;;; Indentation guides. -*- lexical-binding: t -*-

(require 'conf/utils) ; Used: my-color-mix.

(use-package indent-guide
  :ensure t
  :diminish indent-guide-mode
  :config

  ;; Use a color close to the background.
  (face-spec-set 'indent-guide-face `((t)) 'face-defface-spec)
  (defun my-update-indent-guide-color (&rest _)
    (set-face-foreground 'indent-guide-face
                         (my-color-mix (face-foreground 'shadow nil t) 0.4
                                       (face-background 'default nil t) 0.6)))
  (add-hook 'enable-theme-functions #'my-update-indent-guide-color)
  (my-update-indent-guide-color)

  (setq indent-guide-char "│")) ; Useful characters: ·│┆┊╎

(provide 'conf/view/indent-guides)
