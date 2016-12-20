;;; Save point position between sessions. -*- lexical-binding: t -*-

(use-package saveplace ; Bundled with Emacs.
  :demand t
  :config
  (setq-default save-place t))

(provide 'conf/opening-saving/save-point-position)
