;;; Exchange a text object with another. -*- lexical-binding: t -*-
;; Example: to swap two words, go to first one, type gxiw, go to second one and type gxiw again (or .).
;; To clear pending exchange, type gX.

(use-package evil-exchange
  :ensure t
  :init
  (with-eval-after-load 'evil
    (evil-exchange-install))) ; Will use default bindings: gx and gX.

(provide 'conf/evil-specific/exchange)
