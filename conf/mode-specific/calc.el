;;; Customization of Emacs calculator mode.

(defun my-calc-workspace ()
  "Display only Calc and Trail buffers."
  (interactive)
  (calc)
  (delete-other-windows)
  (call-interactively #'calc-trail-display))

;; Use the `--calc' argument to start Emacs Calc-only.
(defun my-calc-workspace-after-startup (&rest ignored-args)
  "Call `my-calc-workspace' after Emacs startup."
  (add-hook 'emacs-startup-hook #'my-calc-workspace t))
(add-to-list 'command-switch-alist (cons "--calc" #'my-calc-workspace-after-startup))

(provide 'conf/mode-specific/calc)
