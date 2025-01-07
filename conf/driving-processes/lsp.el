;;; Support for the Language Server Protocol. -*- lexical-binding: t -*-
;; To enable, invoke M-x lsp (the server will be selected automatically).

(use-package lsp-mode
  :ensure t)

(use-package lsp-ui
  :ensure t
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  :config

  ;; Don't display the symbol name a second time on the right side.
  (setq lsp-ui-sideline-show-symbol nil)

  ;; Make the sideline colors dimmer.
  (dolist (face '(lsp-ui-sideline-code-action
                  lsp-ui-sideline-current-symbol
                  lsp-ui-sideline-symbol
                  lsp-ui-sideline-symbol-info))
    (let* ((new-color "#7F7F7F")
           (box (face-attribute face :box nil t)))
      (set-face-foreground face new-color)
      (when (and box (plist-get box :color))
        (set-face-attribute
         face nil :box (plist-put box :color new-color))))))

(use-package ccls
  :ensure t
  :init

  ;; Needed for M-x lsp to work (as of 2019-07).
  (with-eval-after-load 'lsp-mode
    (require 'ccls)))

(provide 'conf/driving-processes/lsp)
