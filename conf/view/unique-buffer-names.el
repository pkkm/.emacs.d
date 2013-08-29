;;; How to distinguish buffers that visit files with the same names.

(require 'uniquify) ; Included with Emacs.

;; Uniquify the names using parts of directory name, ex.: Makefile|dir1.
(setq uniquify-buffer-name-style 'post-forward
      uniquify-strip-common-suffix t ; Strip common directory suffixes (f|x/a, f|y/a -> f|x, f|y).
      uniquify-separator "|")

(provide 'conf/view/unique-buffer-names)
