;;; Indentation.

;; By default, use spaces and mode's default configuration.
(setq-default indent-tabs-mode nil)

;; For specific languages, use Smart Tabs -- indent with tabs, align with spaces.
;; To enable for a language:
;;   (smart-tabs-insinuate 'language)
;;   (add-hook 'language-mode-hook (lambda () (setq indent-tabs-mode t) (smart-tabs-mode 1)))
;; For modes without configuration in smart-tabs-mode, also use:
;;   (smart-tabs-add-language-support c++ c++-mode-hook
;;     ((c-indent-line . c-basic-offset)
;;      (c-indent-region . c-basic-offset)))
(require 'conf/packages)
(package-ensure-installed 'smart-tabs-mode)

(setq-default tab-width 4)

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
