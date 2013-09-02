;;; Start Emacs maximized.

(defun maximize-frame-if-visible ()
  "Maximize the current frame if it's visible (not minimized)."
  (when (and (eq (frame-visible-p (selected-frame)) t)
             (not (or (eq (frame-parameter nil 'fullscreen) 'maximized)
                      (eq (frame-parameter nil 'maximized) 'maximized))))
    (cond
     ((fboundp 'toggle-frame-maximized) (toggle-frame-maximized))
     ((eq window-system 'w32) (w32-send-sys-command 61488)))))

(add-hook 'emacs-startup-hook #'maximize-frame-if-visible)

(provide 'conf/view/gui/start-maximized)
