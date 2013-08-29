;;; Indentation.

(setq-default indent-tabs-mode t) ; In indentation, replace `tab-width' spaces with a TAB.
(setq-default tab-width 4) ; Width of a TAB.

;; Make a tabstop every `tab-width' characters.
;; (Tabstops -- positions to align to in the Fundamental mode.)
;; (Doesn't affect programming modes.)
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((tab-width (or width tab-width))
		 (max-column (or max 200))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))
(setq tab-stop-list (my-generate-tab-stops))

(provide 'conf/editing/indentation)
