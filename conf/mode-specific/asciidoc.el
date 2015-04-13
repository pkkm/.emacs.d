;;; AsciiDoc.

(use-package adoc-mode
  :disabled t ; I don't use it for now, and it causes a compile error when being installed with package.el.
  :ensure t
  :defer t
  :mode ("\\.asciidoc\\'" . adoc-mode)
  :config
  ;; Use a variable-width face.
  (add-hook 'adoc-mode-hook #'variable-pitch-mode))

(provide 'conf/mode-specific/asciidoc)
