;;; Support for the EditorConfig format for defining coding styles (see editorconfig.org). -*- lexical-binding: t -*-

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :init
  (editorconfig-mode 1))

(provide 'conf/opening-saving/editorconfig)
