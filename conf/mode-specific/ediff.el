;;; Ediff -- a mode for merging files.

(use-package ediff ; Bundled with Emacs.
  :defer t
  :config

  ;; Display everything in one frame.
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; Display the second file to the side instead of below the first one.
  (setq ediff-split-window-function #'split-window-horizontally))

(provide 'conf/mode-specific/ediff)
