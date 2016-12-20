;;; CSV, TSV, etc. -*- lexical-binding: t -*-

(use-package csv-mode
  :ensure t
  :init

  ;; Use for *.tsv files.
  ;; TODO: set `csv-separators' buffer-locally instead of globally.
  (defun my-tsv-in-csv-mode ()
    (csv-mode)
    (custom-set-variables '(csv-separators '("\t")))) ; We can't use `setq' because the `:set' hook wouldn't be run.
  (add-to-list 'auto-mode-alist '("\\.tsv\\'" . my-tsv-in-csv-mode))

  :config

  ;; Make the TAB key insert a literal TAB.
  (bind-key "TAB" #'self-insert-command csv-mode-map))

(provide 'conf/mode-specific/csv)
