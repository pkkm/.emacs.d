;;; Support for the Language Server Protocol. -*- lexical-binding: t -*-
;; To enable, invoke M-x lsp (the server will be selected automatically).
;; For working with Dockerized language servers, see <https://github.com/emacs-lsp/lsp-docker/>.
;; Docs are shown automatically when hovering a symbol with the mouse. To show them with the keyboard, press M-l h g.

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "M-l")

  :config
  (setq lsp-headerline-breadcrumb-enable nil) ; I didn't find the header line useful.
  (lsp-enable-which-key-integration t) ; Enable integration for all major modes.
  (setq lsp-auto-execute-action nil)) ; Don't skip the code action prompt when there's a single action.

;; lsp-mode activates lsp-ui by default unless `lsp-auto-configure' is set to nil.
(use-package lsp-ui
  :ensure t)

(provide 'conf/driving-processes/lsp)
