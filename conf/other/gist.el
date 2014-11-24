;;; Support for GitHub Gist (a pastebin with many features).

(use-package gist
  :ensure gist
  :bind (("C-c G p" . gist-region-or-buffer)
         ("C-c G r" . gist-region-or-buffer-private)
         ("C-c G l" . gist-list))
  :config

  ;; Open gist in browser after it's created.
  (setq gist-view-gist t))

(provide 'conf/other/gist)