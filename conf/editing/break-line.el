;;; Break line at point, continuing comment if within one. -*- lexical-binding: t -*-

(bind-key "<C-return>" #'indent-new-comment-line) ; Works both in GUI and xterm.

(provide 'conf/editing/break-line)
