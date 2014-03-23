;;; Indentation.

;; By default, use spaces and mode's default configuration.
(setq-default indent-tabs-mode nil)

;; For specific languages, use Smart Tabs -- indent with tabs, align with spaces.
;; To enable for a language:
;;   (smart-tabs-insinuate 'language)
;;   (add-hook 'language-mode-hook #'enable-indent-tabs-mode)
;; For modes without configuration in smart-tabs-mode, also use:
;;   (smart-tabs-add-language-support c++ c++-mode-hook
;;     ((c-indent-line . c-basic-offset)
;;      (c-indent-region . c-basic-offset)))
(require 'conf/packages)
(package-ensure-installed 'smart-tabs-mode)
(require 'smart-tabs-mode) ; TODO needed?

;; Utility functions for easy adding to hooks.
(defun enable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode t))
(defun disable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode nil))

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

;; Don't convert tabs to spaces on backspace.
(setq backward-delete-char-untabify-method nil)

(provide 'conf/editing/indentation)
