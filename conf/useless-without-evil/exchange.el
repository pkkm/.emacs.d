;;; Exchange a text object with another.
;; Example: to swap two words, go to first one, type gxiw, go to second one and type gxiw again (or .).
;; To clear pending exchange, type gX.

(use-package evil-exchange
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'evil
    (evil-exchange-install))) ; Will use default bindings: gx and gX.

(provide 'conf/useless-without-evil/exchange)
