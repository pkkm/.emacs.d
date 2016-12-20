;;; nXML -- advanced XML editing mode with validation. -*- lexical-binding: t -*-

(use-package nxml-mode ; Bundled with Emacs.
  :mode "\\.xsd\\'" ; Use for XML Schema files.
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-insinuate 'nxml)
    (add-hook 'nxml-mode-hook #'enable-indent-tabs-mode))

  ;; Expand abbreviations like "p>ul>li*5" with C-j.
  (require 'conf/editing/emmet)
  (add-hook 'nxml-mode-hook #'emmet-mode))

(provide 'conf/mode-specific/xml)
