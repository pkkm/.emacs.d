;;; Show unbound keys. -*- lexical-binding: t -*-

(use-package free-keys
  :ensure t
  :config
  (with-eval-after-load 'evil
    (evil-set-initial-state 'free-keys-mode 'emacs)))

(provide 'conf/other/unbound-keys)
