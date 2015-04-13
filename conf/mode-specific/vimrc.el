;;; Vim configuration files (also useful for pentadactylrc, etc.).

(use-package vimrc-mode
  :ensure t
  :defer t
  :mode ((".vim\\'" . vimrc-mode)
         ("vimrc\\'" . vimrc-mode)
         (".pentadactylrc\\'" . vimrc-mode)))

(provide 'conf/mode-specific/vimrc)
