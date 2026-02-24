;;; Flycheck -- on-the-fly syntax checker. -*- lexical-binding: t -*-

;; Enable by default.
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;; Display error messages in a child frame when point is on them.
(use-package flycheck-posframe
  :ensure t
  :init
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode)
  :config

  (setq flycheck-posframe-border-width 3)

  (defun my-flycheck-posframe-set-color (&rest args)
    (set-face-background 'flycheck-posframe-background-face
                         (if (eq (frame-parameter nil 'background-mode) 'dark)
                             "black"
                           "white")))
  (my-flycheck-posframe-set-color)
  (add-hook 'enable-theme-functions #'my-flycheck-posframe-set-color))

(provide 'conf/driving-processes/flycheck)
