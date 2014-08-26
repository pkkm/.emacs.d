;;; Ido settings for file completion.

(require 'conf/minibuffer/ido)

;; Sort Ido's file list by modification time.
(package-ensure-installed 'ido-sort-mtime)
(ido-sort-mtime-mode 1)
(setq ido-sort-mtime-tramp-files-at-end t)

;; If there's a filename at point, use it as the starting point.
(setq ido-use-filename-at-point 'guess)

(provide 'conf/opening-saving/ido)
