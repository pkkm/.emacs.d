;;; LaTeX. -*- lexical-binding: t -*-
;; Emacs tip: to type TeX symbols and have them converted to Unicode, press C-\ TeX RET (toggle with C-\).
;; To lint a LaTeX file, use C-c C-c ChkTeX RET.
;; For AUCTeX tips, see <http://tex.stackexchange.com/questions/20843/useful-shortcuts-or-key-bindings-or-predefined-commands-for-emacsauctex>.

(use-package tex
  :ensure auctex
  :init
  (autoload #'LaTeX-math-mode "latex" nil t) ; For use in other modes.
  :config

  ;; Indentation.
  (setq LaTeX-item-indent 0) ; Don't indent \item additionally (the `itemize' environment will already have its own indentation).
  (setq LaTeX-syntactic-comments nil) ; Don't touch the inside of comments when indenting.

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
  ;; To change the bindings, modify `LaTeX-math-list' or `LaTeX-math-default'. The macros are inserted by `LaTeX-math-insert'.
  ;; (The mode also displays a "Math" menu with many symbols -- can be used efficiently with Lacarte.)
  (add-hook 'LaTeX-mode-hook #'LaTeX-math-mode)
  (setq LaTeX-math-abbrev-prefix ";")
  (setq LaTeX-math-list
        '((?o "omega") (?O "Omega") ; Already on "w" but bound because "o" is more mnemonic.
          (?/ "frac") (?! "not") ; \not is on "/" by default; \frac is unbound.
          (?\C-i "int")
          (?Q "sqrt")))

  ;; Make LaTeX previews bigger.
  ;; (Activate previews in buffer with C-c C-p C-b, clear with C-c C-p C-c C-b).
  (defvar my-latex-preview-scale-factor 1.17
    "Factor by which the default LaTeX preview size (calculated from font size) should be multiplied.")
  (defun my-latex-preview-scale ()
    (* (funcall (preview-scale-from-face))
       my-latex-preview-scale-factor))
  (setq preview-scale-function #'my-latex-preview-scale)

  ;; Make the " key use `csquotes'. (If this doesn't work, try pressing C-c C-n.)
  (setq LaTeX-csquotes-open-quote "\\enquote{")
  (setq LaTeX-csquotes-close-quote "}")

  ;; Flyspell: don't spell-check TeX comments.
  ;; Instead of using a hook, we could set the `flyspell-mode-predicate' property on tex-mode, latex-mode, context-mode, etc. (We'd have to do it for each mode rather than a single parent mode because Flyspell doesn't check the parent mode's property.)
  (defvar my-flyspell-check-tex-comments nil
    "Should TeX comments be spell-checked?")
  (defun my-tex-flyspell-word-predicate ()
    (and (or my-flyspell-check-tex-comments (not (nth 4 (syntax-ppss))))
         (tex-mode-flyspell-verify)))
  (defun my-tex-set-flyspell-word-predicate ()
    (setq flyspell-generic-check-word-predicate #'my-tex-flyspell-word-predicate))
  (add-hook 'TeX-mode-hook #'my-tex-set-flyspell-word-predicate)

  ;; Parse files automatically after opening.
  (setq TeX-parse-self t)

  ;; Cheatsheet.
  (defun my-cheatsheet-AUCTeX ()
    (interactive)
    (open-cheatsheet "https://ftp.gnu.org/gnu/auctex/11.88-extra/tex-ref.pdf")) ; From conf/other/cheatsheets.
  (bind-key "C-c C" #'my-cheatsheet-AUCTeX TeX-mode-map))

;; Latexmk support.
(use-package auctex-latexmk
  :ensure t
  :init
  (with-eval-after-load 'tex
    (auctex-latexmk-setup))
  :config

  ;; Use Latexmk as the default command.
  ;; (We have to use a hook instead of `setq-default' because AUCTeX sets this variable on mode activation.)
  (defun my-tex-set-latexmk-as-default ()
    (setq TeX-command-default "LatexMk"))
  (add-hook 'TeX-mode-hook #'my-tex-set-latexmk-as-default)

  ;; Compile to PDF when `TeX-PDF-mode' is active.
  (setq auctex-latexmk-inherit-TeX-PDF-mode t))

;; Parse labels and bibliographies. Keybinding to insert citations: C-c [.
(use-package reftex
  :init
  (with-eval-after-load 'tex
    (add-hook 'TeX-mode-hook #'turn-on-reftex))
  :config
  (setq reftex-plug-into-AUCTeX t))

;; Completion for citation keys and labels.
(use-package company-reftex
  :ensure t
  :init
  (with-eval-after-load 'tex
    (with-eval-after-load 'reftex
      (add-to-list 'company-backends #'company-reftex-citations)
      (add-to-list 'company-backends #'company-reftex-labels))))

(provide 'conf/mode-specific/latex)
