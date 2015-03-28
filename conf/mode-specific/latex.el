;;; LaTeX.
;; Emacs tip: to type TeX symbols and have them converted to Unicode, press C-\ TeX RET (toggle with C-\).
;; For AUCTeX tips, see <http://tex.stackexchange.com/questions/20843/useful-shortcuts-or-key-bindings-or-predefined-commands-for-emacsauctex>.

(use-package tex
  :ensure auctex
  :defer t
  :init
  (autoload #'LaTeX-math-mode "latex" nil t) ; For use in other modes.
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
  (setq LaTeX-math-abbrev-prefix ";")

  ;; Electric braces after sub- and superscripts.
  (setq TeX-electric-sub-and-superscript)

  ;; Quickly enter \frac.
  (with-eval-after-load 'latex ; LaTeX mode is loaded after TeX mode.
    (bind-key "C-c /" (lambda () (interactive) (TeX-insert-macro "frac")) LaTeX-mode-map))

  ;; Make LaTeX previews bigger.
  ;; (Activate previews in buffer with C-c C-p C-b, clear with C-c C-p C-c C-b).
  (defvar my-latex-preview-scale-factor 1.17
    "Factor by which the default LaTeX preview size (calculated from font size) should be multiplied.")
  (defun my-latex-preview-scale ()
    (* (funcall (preview-scale-from-face))
       my-latex-preview-scale-factor))
  (setq preview-scale-function #'my-latex-preview-scale))

(provide 'conf/mode-specific/latex)
