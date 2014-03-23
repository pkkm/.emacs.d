;;; LaTeX.

(require 'conf/packages)
(package-ensure-installed 'auctex)

;; Indentation: use smart tabs.
(require 'conf/editing/indentation)
(smart-tabs-add-language-support LaTeX LaTeX-mode-hook
  ((LaTeX-indent-line . LaTex-indent-level)))
(smart-tabs-insinuate 'LaTeX)
(add-hook 'LaTeX-mode-hook #'enable-indent-tabs-mode)

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
               '("zathura" ("zathura"
                            (mode-io-correlate "--page %(outpage)")
                            " %o")))

  (add-to-list 'TeX-view-program-selection '(output-pdf "zathura")))
(require 'conf/utils/hooks)
(add-one-shot-hook 'LaTeX-mode-hook #'my-LaTeX-use-zathura-for-pdf)

;; Make RET also indent.
(setq TeX-newline-function #'evil-ret-and-indent)

(provide 'conf/mode-specific/latex)
