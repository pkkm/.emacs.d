;;; Ido settings for file completion.

;; Sort Ido's file list by modification time.
(use-package ido-sort-mtime
  :ensure t
  :init
  (with-eval-after-load 'ido
    (ido-sort-mtime-mode 1))
  :config
  (setq ido-sort-mtime-tramp-files-at-end t))

;; If there's a filename at point, use it as the starting point.
(with-eval-after-load 'ido
  (setq ido-use-filename-at-point 'guess))

(provide 'conf/opening-saving/ido)
