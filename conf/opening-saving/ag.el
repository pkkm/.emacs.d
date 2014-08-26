;;; Ag -- the silver searcher (alternative to ack, which is an alternative to grep).
;; Usage: `ag', `ag-regexp', `ag-project', `ag-project-regexp'.

(package-ensure-installed 'ag)

(setq ag-highlight-search t)

(provide 'conf/opening-saving/ag)
