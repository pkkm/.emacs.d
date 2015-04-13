;;; Ag -- the silver searcher (alternative to ack, which is an alternative to grep).
;; Usage: `ag', `ag-regexp', `ag-project', `ag-project-regexp'.

(use-package ag
  :ensure t
  :bind (("C-c a" . ag)
         ("C-c A" . ag-project))
  :config
  (setq ag-highlight-search t))

;; WGrep integration.
;; Use `wgrep-change-to-wgrep-mode' in an Ag buffer to make the buffer editable.
;; To confirm changes, use C-x C-s (this will make changes to buffers but won't save them).
(use-package wgrep-ag
  :ensure t
  :defer t)

(provide 'conf/driving-processes/ag)
