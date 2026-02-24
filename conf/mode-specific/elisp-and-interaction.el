;;; Emacs Lisp and Lisp Interaction modes. -*- lexical-binding: t -*-
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

;; Highlight defined symbols.
(use-package highlight-defined
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode))

(use-package lisp-mode ; Bundled with Emacs; contains lisp-mode, emacs-lisp-mode and lisp-interaction-mode.
  :config

  ;; Eldoc -- show function arguments in the minibuffer.
  ;; (Configuration is in conf/view/eldoc.el.)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

  ;; Bindings.

  (defun my-evil-eval-region (region-start region-end)
    "Evaluate region and exit Evil's visual state."
    (interactive "r") ; Needs a region.
    (eval-region region-start region-end)
    (evil-exit-visual-state))

  (dolist (keymap (list lisp-interaction-mode-map emacs-lisp-mode-map))
    (bind-key "C-c C-e" #'pp-eval-last-sexp keymap)
    (bind-key "C-c C-i" #'eval-print-last-sexp keymap) ; Insert value at point.

    (with-eval-after-load 'evil
      (bind-key "C-c C-r" #'my-evil-eval-region keymap))
    (bind-key "C-c C-b" #'eval-buffer keymap)
    (bind-key "C-c C-d" #'eval-defun keymap) ; Eval the top-level form containing point (or after point)))).

    (bind-key "C-c C-p" #'pp-eval-expression keymap) ; Prompt for an expression to eval.

    (bind-key "C-c C-t" #'helpful-at-point keymap))

  ;; Delete elc file when saving an el file.
  (defun my-remove-elc-if-exists ()
    "If this buffer is editing an .el file and there's an .elc file with the same name without extension, delete the .elc file."
    (interactive)
    (when (and (stringp buffer-file-name)
               (string= (file-name-extension buffer-file-name) "el"))
      (let ((elc-file-name (concat buffer-file-name "c")))
        (when (file-exists-p elc-file-name)
          (delete-file elc-file-name)
          (message "Deleted compiled file: %s" elc-file-name)))))
  (defun my-remove-elc-on-save ()
    "When saving an .el file, remove the associated .elc file."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook #'my-remove-elc-if-exists))
  (add-hook 'emacs-lisp-mode-hook #'my-remove-elc-on-save)

  ;; Disable Flycheck (on typical Emacs configs, produces far more false positives than useful warnings).
  (with-eval-after-load 'flycheck
    (->> (default-value 'flycheck-disabled-checkers)
         (append '(emacs-lisp emacs-lisp-checkdoc))
         (-uniq)
         (setq-default flycheck-disabled-checkers))))

;; This refactoring package is only worthwhile for Emacs Lisp. For other languages, it has very few refactorings; use LSP instead.
(use-package emr
  :ensure t
  :init
  (bind-key "C-c \\" #'emr-show-refactor-menu emacs-lisp-mode-map)
  (bind-key "C-c \\" #'emr-show-refactor-menu lisp-interaction-mode-map)
  (add-hook 'emacs-lisp-mode-hook #'emr-initialize)
  (add-hook 'lisp-interaction-mode-hook #'emr-initialize))

(provide 'conf/mode-specific/elisp-and-interaction)
