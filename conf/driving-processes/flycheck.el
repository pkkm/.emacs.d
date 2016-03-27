;;; Flycheck -- on-the-fly syntax checker.

;; Enable by default.
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode 1))

;; Display error messages in a tooltip at point.
(use-package flycheck-pos-tip
  :ensure t
  :init
  (with-eval-after-load 'flycheck
    (flycheck-pos-tip-mode))
  :config
  (setq flycheck-pos-tip-timeout most-positive-fixnum))

(provide 'conf/driving-processes/flycheck)
