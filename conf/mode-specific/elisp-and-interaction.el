;;; Emacs Lisp and Lisp Interaction modes. -*- lexical-binding: t -*-
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

;; Highlight defined symbols.
(use-package highlight-defined
  :ensure t
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode))

;; SLIME-like navigation in Elisp.
(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  :config

  ;; `C-c .', `C-c ,' -- go to definition of symbol at point, go back.
  ;; `C-c t' -- describe thing at point.
  (require 'conf/utils/keys) ; Used: clear-keymap.
  (clear-keymap elisp-slime-nav-mode-map)
  (bind-key "C-c ." #'elisp-slime-nav-find-elisp-thing-at-point elisp-slime-nav-mode-map)
  (bind-key "C-c ," #'pop-tag-mark elisp-slime-nav-mode-map)
  (bind-key "C-c C-t" #'elisp-slime-nav-describe-elisp-thing-at-point elisp-slime-nav-mode-map))

(use-package lisp-mode ; Bundled with Emacs; contains lisp-mode, emacs-lisp-mode and lisp-interaction-mode.
  :config

  ;; Eldoc -- show function arguments in the minibuffer.
  ;; (Configuration is in conf/view/eldoc.el.)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

  ;; Fix completion (the default `company-capf' backend is broken in lisp-interaction-mode as of 2019-03).
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-elisp))

  ;; Evaluation bindings.
  (defun evil-eval-region (region-start region-end)
    "Evaluate region and exit Evil's visual state."
    (interactive "r") ; Needs a region.
    (eval-region region-start region-end)
    (evil-exit-visual-state))
  (dolist (keymap (list lisp-interaction-mode-map emacs-lisp-mode-map))
    (let ((map keymap))
      (bind-key "C-c C-e" #'pp-eval-last-sexp map)
      (bind-key "C-c C-i" #'eval-print-last-sexp map) ; Insert value at point.

      (with-eval-after-load 'evil
        (bind-key "C-c C-r" #'evil-eval-region map)) ; Depends on `lexical-binding' (for `map' to be available).
      (bind-key "C-c C-b" #'eval-buffer map)
      (bind-key "C-c C-d" #'eval-defun map) ; Eval the top-level form containing point (or after point)))).

      (bind-key "C-c C-p" #'pp-eval-expression map))) ; Prompt for an expression to eval.

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

(provide 'conf/mode-specific/elisp-and-interaction)
