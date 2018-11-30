;;; ESS (Emacs Speaks Statistics). -*- lexical-binding: t -*-

;; Useful keybindings:
;;   C-c C-z -- switch to REPL / back to text file.
;;   C-c C-s -- change REPL process associated with buffer.
;;   C-c C-j -- evaluate line.
;;   C-c C-c -- evaluate region, function or paragraph.
;;   C-c C-b -- evaluate buffer.
;;   C-c C-v -- open help.
;;   , (in REPL) -- open a menu with some handy commands, e.g. installing a package.

(use-package ess
  :ensure t
  :config
  (setq ess-smart-S-assign-key nil)) ; Don't replace "_" with " <- ".

(provide 'conf/mode-specific/ess)
