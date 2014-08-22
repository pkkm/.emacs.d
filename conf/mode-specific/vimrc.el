;;; Vim configuration files (also useful for pentadactylrc, etc.).

(require 'conf/packages)

(use-package vimrc-mode
  :ensure vimrc-mode
  :defer t
  :mode ((".vim\\'" . vimrc-mode)
         ("vimrc\\'" . vimrc-mode)
         (".pentadactylrc\\'" . vimrc-mode)))

(provide 'conf/mode-specific/vimrc)
