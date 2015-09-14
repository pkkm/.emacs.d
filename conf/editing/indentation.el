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

;; Utility functions for easy adding to hooks.
(defun enable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode t))
(defun disable-indent-tabs-mode ()
  "Set `indent-tabs-mode' to t in the current buffer."
  (setq indent-tabs-mode nil))

;; Don't convert tabs to spaces on backspace.
(setq backward-delete-char-untabify-method nil)

(provide 'conf/editing/indentation)
