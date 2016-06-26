;;; CSV, TSV, etc.

(use-package 'csv-mode
  :ensure t
  :config

  ;; Make the TAB key insert a literal TAB.
  (bind-key "TAB" #'self-insert-command csv-mode-map))

(provide 'conf/mode-specific/csv)
