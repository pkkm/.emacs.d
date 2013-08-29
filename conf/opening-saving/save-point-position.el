;;; Save point position between sessions.

(require 'saveplace) ; Included with Emacs.

(setq-default save-place t)
(setq save-place-file (expand-file-name "point-positions" my-savefile-dir))

(provide 'conf/opening-saving/save-point-position)
