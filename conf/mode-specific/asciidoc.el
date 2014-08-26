;;; AsciiDoc.

(use-package adoc-mode
  :ensure adoc-mode
  :defer t
  :mode ("\\.asciidoc\\'" . adoc-mode)
  :config
  ;; Use a variable-pitch face.
  (defun turn-on-variable-pitch-mode ()
    (variable-pitch-mode 1))
  (add-hook 'adoc-mode-hook #'turn-on-variable-pitch-mode))

(provide 'conf/mode-specific/asciidoc)
