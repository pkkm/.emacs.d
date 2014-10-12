;;; Flycheck -- on-the-fly syntax checker.

;; Enable by default.
(use-package flycheck
  :ensure flycheck
  :defer t
  :init
  (global-flycheck-mode 1))

;; Display error messages in a popup at point instead of the modeline.
(use-package flycheck-pos-tip
  :ensure flycheck-pos-tip
  :defer t
  :init
  (with-eval-after-load 'flycheck
    (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))

(provide 'conf/driving-processes/flycheck)
