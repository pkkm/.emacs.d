;;; ESS (Emacs Speaks Statistics). -*- lexical-binding: t -*-

(use-package ess
  :ensure t
  :config
  (with-eval-after-load 'ess-s-l
    (ess-toggle-S-assign nil))) ; Don't replace "_" with " <- ".

(provide 'conf/mode-specific/ess)
