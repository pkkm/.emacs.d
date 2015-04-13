;;; Indentation.

;; For specific languages, use Smart Tabs -- indent with tabs, align with spaces.
;; To enable for a language:
;;   (smart-tabs-insinuate 'language)
;;   (add-hook 'language-mode-hook #'enable-indent-tabs-mode)
;; For modes without configuration in smart-tabs-mode, also use:
;;   (smart-tabs-add-language-support c++ c++-mode-hook
;;     ((c-indent-line . c-basic-offset)
;;      (c-indent-region . c-basic-offset)))
(use-package smart-tabs-mode
  :ensure t
  :demand t) ; Load immediately, so that we can use (with-eval-after-load 'smart-tabs-mode) in mode-specific indentation settings.

;; By default, indent with 4 spaces.
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; Don't convert tabs to spaces on backspace.
(setq backward-delete-char-untabify-method nil)

;; Utility functions for easy adding to hooks.
(defun enable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode t))
(defun disable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode nil))

;; Make a tabstop every `tab-width' characters.
;; (Tabstops -- positions to align to in the Fundamental mode. Doesn't affect programming modes.)
(defun my-generate-tab-stops (&optional width max)
  "Return a sequence suitable for `tab-stop-list'."
  (let* ((tab-width (or width tab-width))
         (max-column (or max 200))
         (count (/ max-column tab-width)))
    (number-sequence tab-width (* tab-width count) tab-width)))
(setq tab-stop-list (my-generate-tab-stops))

(provide 'conf/editing/indentation)
