;;; Support for the Language Server Protocol. -*- lexical-binding: t -*-
;; To enable, invoke M-x lsp (the server will be selected automatically).
;; For working with Dockerized language servers, see <https://github.com/emacs-lsp/lsp-docker/>.
;; Docs are shown automatically when hovering a symbol with the mouse. To show them with the keyboard, press M-l h g.

(use-package lsp-mode
  :ensure t)

;; lsp-mode activates lsp-ui by default unless `lsp-auto-configure' is set to nil.
(use-package lsp-ui
  :ensure t)

(use-package ccls
  :ensure t
  :init

  ;; Needed for M-x lsp to work (as of 2019-07).
  (with-eval-after-load 'lsp-mode
    (require 'ccls)))

(provide 'conf/driving-processes/lsp)
