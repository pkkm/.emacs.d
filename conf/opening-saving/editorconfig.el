;;; Support for the EditorConfig format for defining coding styles (see editorconfig.org).

(package-ensure-installed 'editorconfig)

;; The package should be initialized automatically.
;; (The `edconf-find-file-hook' function will be added to `find-file-hook'.)

(provide 'conf/opening-saving/editorconfig)
