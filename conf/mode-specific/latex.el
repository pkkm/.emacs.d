;;; LaTeX.
;; Emacs tip: to type TeX symbols and have them converted to Unicode, press C-\ TeX RET (toggle with C-\).

(use-package tex
  :ensure auctex
  :defer t
  :config

  ;; Indentation: smart tabs.
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-add-language-support LaTeX LaTeX-mode-hook
      ((LaTeX-indent-line . LaTeX-indent-level)))
    (smart-tabs-insinuate 'LaTeX)
    (add-hook 'LaTeX-mode-hook #'enable-indent-tabs-mode))

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
  (add-to-list 'TeX-view-program-list
               '("zathura"
                 ("zathura" (mode-io-correlate "--page %(outpage)") " %o")))
  (add-to-list 'TeX-view-program-selection '(output-pdf "zathura"))

  ;; Make RET also indent.
  (with-eval-after-load 'evil
    (setq TeX-newline-function #'evil-ret-and-indent))

  ;; Use ";" as prefix for quickly entering math (toggle with C-c ~).
  ;; (The mode also displays a "Math" menu with many symbols -- can be used efficiently with Lacarte.)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (setq LaTeX-math-abbrev-prefix ";"))

(provide 'conf/mode-specific/latex)
