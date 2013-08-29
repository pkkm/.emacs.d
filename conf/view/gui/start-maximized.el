;;; Start Emacs maximized.

(defun maximize-this-frame ()
  (unless (or (eq (frame-parameter nil 'fullscreen) 'maximized)
              (eq (frame-parameter nil 'maximized) 'maximized))
    (toggle-frame-maximized)))

(add-hook 'emacs-startup-hook #'maximize-this-frame)

(provide 'conf/view/gui/start-maximized)
