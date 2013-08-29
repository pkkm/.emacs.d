;;; Read Vim modelines (like local variables in Emacs).
;;; Example: /* vim: shiftwidth=4 expandtab: */.

(autoload 'vim-modeline/do "vim-modeline") ; In `my-vendor-dir'.
(add-hook 'find-file-hook #'vim-modeline/do)

(provide 'conf/opening-saving/vim-modelines)
