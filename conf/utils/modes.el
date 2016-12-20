;;; Functions for modes. -*- lexical-binding: t -*-

(require 'cl-lib) ; Used: cl-loop.
(defun derived-mode-hierarchy (mode)
  "Returns MODE, its parent mode, the parent mode of its parent mode, etc."
  (cl-loop collect mode
           do (setq mode (get mode 'derived-mode-parent))
           while mode))

(provide 'conf/utils/modes)
