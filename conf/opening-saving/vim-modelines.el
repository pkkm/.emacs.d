;;; Read Vim modelines (like file-local variables in Emacs).
;; Example: /* vim: shiftwidth=4 expandtab: */.

(use-package vim-modeline ; Installed in `my-vendor-dir'.
    :commands vim-modeline/do
    :init
    (add-hook 'find-file-hook #'vim-modeline/do))

(provide 'conf/opening-saving/vim-modelines)
