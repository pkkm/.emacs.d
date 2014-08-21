;;; Distinguish buffers that visit files with the same names using parts of directory name, e.g. Makefile|dir1.

(require 'conf/packages)

(use-package uniquify ; Included with Emacs.
  :config
  (setq uniquify-buffer-name-style 'post-forward
        uniquify-strip-common-suffix t ; Strip common directory suffixes (f|x/a, f|y/a -> f|x, f|y).
        uniquify-separator "|"))

(provide 'conf/view/unique-buffer-names)
