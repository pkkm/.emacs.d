;;; Save point position between sessions.

(use-package saveplace ; Bundled with Emacs.
  :demand t
  :config
  (setq-default save-place t)
  (setq save-place-file
        (expand-file-name "point-positions" my-savefile-dir)))

(provide 'conf/opening-saving/save-point-position)
