;;; Ag -- the silver searcher (alternative to ack, which is an alternative to grep).
;; Usage: `ag', `ag-regexp', `ag-project', `ag-project-regexp'.

(use-package ag
  :ensure ag
  :bind (("C-c a" . ag)
         ("C-c A" . ag-project))
  :config
  (setq ag-highlight-search t))

(provide 'conf/opening-saving/ag)
