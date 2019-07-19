;;; Support for the Language Server Protocol. -*- lexical-binding: t -*-
;; To enable, invoke M-x lsp (the server will be selected automatically).

(use-package lsp-mode
  :ensure t
  :config
  (setq lsp-prefer-flymake nil)) ; Prefer Flycheck to Flymake.

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode))

(use-package company-lsp
  :ensure t
  :init
  (with-eval-after-load 'company
    (with-eval-after-load 'lsp-mode
      (add-to-list 'company-backends #'company-lsp))))

;; As of 2018-11, the Emacs package for cquery is more polished than ccls. (I tried both and ccls failed to work without any indication of what's wrong.)
(use-package cquery
  :ensure t
  :init

  ;; Needed for M-x lsp to work (as of 2019-07).
  (with-eval-after-load 'lsp-mode
    (require 'cquery))

  :config

  ;; Store cache in one place instead of polluting project directories.
  (setq cquery-cache-dir-function #'cquery-cache-dir-consolidated))

(provide 'conf/driving-processes/lsp)
