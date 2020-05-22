;;; Vim configuration files. -*- lexical-binding: t -*-

(use-package vimrc-mode
  :ensure t
  :mode ((".vim\\'" . vimrc-mode)
         ("vimrc\\'" . vimrc-mode)))

(provide 'conf/mode-specific/vimrc)
