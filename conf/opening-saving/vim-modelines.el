;;; Read Vim modelines (like file-local variables in Emacs). -*- lexical-binding: t -*-
;; Example: /* vim: shiftwidth=4 expandtab: */.
;; (Warning: the above example may mess up the indentation in this file.)

(use-package vim-modeline ; Installed in `my-vendor-dir' because it's not available in the repos (as of 2026-02).
  :commands vim-modeline/do
  :init
  (add-hook 'find-file-hook #'vim-modeline/do))

(provide 'conf/opening-saving/vim-modelines)
