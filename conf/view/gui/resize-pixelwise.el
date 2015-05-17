;;; Resize frames by pixels instead of characters.

(when (display-graphic-p)
  (setq frame-resize-pixelwise t))

(provide 'conf/view/gui/resize-pixelwise)
