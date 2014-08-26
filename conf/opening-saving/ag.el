;;; Ag -- the silver searcher (alternative to ack, which is an alternative to grep).
;; Usage: `ag', `ag-regexp', `ag-project', `ag-project-regexp'.

(package-ensure-installed 'ag)

(setq ag-highlight-search t)

(global-set-key (kbd "C-c a") #'ag)
(global-set-key (kbd "C-c A") #'ag-project)

(provide 'conf/opening-saving/ag)
