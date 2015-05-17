;;; Start Emacs maximized.

(when (display-graphic-p)
  (defun maximize-frame-if-visible ()
    "Maximize the current frame if it's visible (not minimized)."
    (unless (frame-parameter nil 'fullscreen)
      (cond
       ((fboundp 'toggle-frame-maximized) (toggle-frame-maximized))
       ((eq window-system 'w32) (w32-send-sys-command 61488))))) ; Compatibility with Emacs 24.3 (24.4 has `toggle-frame-maximized').

  (add-hook 'emacs-startup-hook #'maximize-frame-if-visible))

(provide 'conf/view/gui/start-maximized)
