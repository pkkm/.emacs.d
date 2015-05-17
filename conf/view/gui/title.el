;;; Frame title.

(when (display-graphic-p)
  (setq frame-title-format
        '("Emacs: " "%b " (:eval (when (buffer-modified-p) "+ "))
          "[" default-directory "]")))

;;(setq icon-title-format frame-title-format)

(provide 'conf/view/gui/title)
