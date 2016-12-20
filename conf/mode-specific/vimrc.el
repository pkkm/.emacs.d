;;; Vim configuration files (also useful for pentadactylrc, etc.). -*- lexical-binding: t -*-

(use-package vimrc-mode
  :ensure t
  :mode ((".vim\\'" . vimrc-mode)
         ("vimrc\\'" . vimrc-mode)
         (".pentadactylrc\\'" . vimrc-mode)))

(provide 'conf/mode-specific/vimrc)
