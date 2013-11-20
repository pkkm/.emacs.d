;;; Vim configuration files (also useful for pentadactylrc, etc.).

(require 'conf/packages)
(package-ensure-installed 'vimrc-mode)

;; Automaticaly activate for *.vim, vimrc, .pentadactylrc.
(add-to-list 'auto-mode-alist '(".vim\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist '("vimrc\\'" . vimrc-mode))
(add-to-list 'auto-mode-alist '(".pentadactylrc\\'" . vimrc-mode))

(provide 'conf/mode-specific/vimrc)
