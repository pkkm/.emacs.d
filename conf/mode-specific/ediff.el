;;; Ediff -- a mode for merging files. -*- lexical-binding: t -*-
;; When using Ediff, press h for help.

(use-package ediff ; Bundled with Emacs.
  :config

  ;; Display everything in one frame.
  (setq ediff-window-setup-function #'ediff-setup-windows-plain)

  ;; Display the second file to the side instead of below the first one.
  (setq ediff-split-window-function #'split-window-horizontally))

;; ZTree -- diff directory trees.
;; Usage: M-x ztree-diff, then:
;;   * SPC, RET -- open/close directory.
;;   * x -- open/close directory recursively.
;;   * Backspace -- jump to parent directory.
;;   * RET -- start Ediff on file.
;;   * SPC -- show diff of current file.
;;   * TAB -- switch between panels.
;;   * h -- toggle showing identical files.
;;   * C -- copy to other panel.
;;   * D -- delete.
;;   * v -- view.
;;   * r -- rescan file/directory.
;;   * F5 -- rescan everything.
(use-package ztree
  :ensure t)

(provide 'conf/mode-specific/ediff)
