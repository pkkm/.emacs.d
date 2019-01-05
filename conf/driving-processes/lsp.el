;;; Support for the Language Server Protocol. -*- lexical-binding: t -*-

(use-package lsp-mode
  :ensure t
  :config

  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)

  (add-hook 'lsp-after-open-hook #'company-mode))

(use-package lsp-ui
  :ensure t
  :init
  (with-eval-after-load 'lsp-mode
    (add-hook 'lsp-mode-hook #'lsp-ui-mode)))

(use-package company-lsp
  :ensure t
  :init
  (with-eval-after-load 'company
    (with-eval-after-load 'lsp-mode
      (add-to-list 'company-backends #'company-lsp))))

;; To enable, invoke M-x lsp-cquery-enable. Don't use M-x lsp-mode -- lsp-cquery-enable will turn it on for you.
;; As of 2018-11, the Emacs package for cquery is more polished than ccls. (I tried both and ccls often failed to work without any indication of what's wrong.)
(use-package cquery
  :ensure t

  :init
  (autoload 'lsp-cquery-enable "cquery" nil t)

  :config
  ;; Store cache in one place rather than polluting project directories.
  (setq cquery-cache-dir-function #'cquery-cache-dir-consolidated))

(provide 'conf/driving-processes/lsp)
