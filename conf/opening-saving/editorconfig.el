;;; Support for the EditorConfig format for defining coding styles (see editorconfig.org). -*- lexical-binding: t -*-

(use-package editorconfig
  :ensure t) ; The package is initialized automatically. (The `edconf-find-file-hook' function is added to `find-file-hook'.)

(provide 'conf/opening-saving/editorconfig)
