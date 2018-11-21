;;; ESS (Emacs Speaks Statistics). -*- lexical-binding: t -*-

(use-package ess
  :ensure t
  :config
  (setq ess-smart-S-assign-key nil)) ; Don't replace "_" with " <- ".

(provide 'conf/mode-specific/ess)
