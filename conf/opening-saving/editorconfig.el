;;; Support for the EditorConfig format for defining coding styles (see editorconfig.org).

(use-package editorconfig
  :ensure editorconfig
  :defer t) ; The package will be initialized automatically. (The `edconf-find-file-hook' function will be added to `find-file-hook'.)

(provide 'conf/opening-saving/editorconfig)
