;;; Search for selected text with # and * in visual state. -*- lexical-binding: t -*-

(use-package evil-visualstar
  :ensure t
  :init
  (with-eval-after-load 'evil
    (global-evil-visualstar-mode)))

(provide 'conf/evil-specific/search-selected)
