;;; LaTeX.

(require 'conf/editing/completion)
(require 'conf/editing/indentation)
(require 'conf/utils/hooks) ; Used: add-one-shot-hook.
(require 'conf/evil)

(use-package tex-site
  :ensure auctex
  :defer t
  :config

  ;; Auto-completion.
  (setq ac-modes (append TeX-modes ac-modes)) ; Enable `auto-complete-mode' in AUCTeX modes.

  ;; Indentation: smart tabs.
  (smart-tabs-add-language-support LaTeX LaTeX-mode-hook
    ((LaTeX-indent-line . LaTeX-indent-level)))
  (smart-tabs-insinuate 'LaTeX)
  (add-hook 'LaTeX-mode-hook #'enable-indent-tabs-mode)

  ;; Indentation: other.
  (defvaralias 'LaTeX-left-right-indent-level 'LaTeX-indent-level) ; Indent \left and \right normally.
  (setq LaTeX-item-indent 0) ; Don't indent \item additionally (the `itemize' environment will already have its own indentation).
  (setq LaTeX-document-regexp nil) ; Indent the `document' environment too.
  (setq LaTeX-syntactic-comments nil) ; Don't touch the inside of comments when indenting.

  ;; Use the XeTeX engine by default.
  (setq-default TeX-engine 'xetex)

  ;; Output to PDF by default.
  (setq-default TeX-PDF-mode t)

  ;; Auto-save before compiling.
  (setq TeX-save-query nil)

  ;; Don't ask for confirmation when deleting temporary files.
  (setq TeX-clean-confirm nil)

  ;; Use zathura for viewing PDF files.
  (defun my-LaTeX-use-zathura-for-pdf ()
    (add-to-list 'TeX-view-program-list
                 '("zathura"
                   ("zathura" (mode-io-correlate "--page %(outpage)") " %o")))
    (add-to-list 'TeX-view-program-selection '(output-pdf "zathura")))
  (add-one-shot-hook 'LaTeX-mode-hook #'my-LaTeX-use-zathura-for-pdf)

  ;; Make RET also indent.
  (setq TeX-newline-function #'evil-ret-and-indent))

(provide 'conf/mode-specific/latex)
