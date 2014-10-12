;;; Emacs Lisp and Lisp Interaction modes.
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

;; Eldoc mode -- show function arguments in minibuffer.
(use-package eldoc ; Included with Emacs.
  :diminish eldoc-mode
  :commands eldoc-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  :config
  (setq eldoc-idle-delay 0.1)
  (setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit))

;; Highlight defined symbols.
(use-package highlight-defined
  :ensure highlight-defined
  :commands highlight-defined-mode
  :init
  (add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode))

;; SLIME-like navigation in Elisp.
(use-package elisp-slime-nav
  :ensure elisp-slime-nav
  :diminish elisp-slime-nav-mode
  :commands turn-on-elisp-slime-nav-mode
  :pre-load

  ;; `C-c .', `C-c ,' -- go to definition of symbol at point, go back.
  ;; `C-c t' -- describe thing at point.
  ;; For some reason, this needs to happen before `elisp-slime-nav' is loaded.
  (setq elisp-slime-nav-mode-map (make-sparse-keymap))
  (bind-key "C-c ." #'elisp-slime-nav-find-elisp-thing-at-point elisp-slime-nav-mode-map)
  (bind-key "C-c ," #'pop-tag-mark elisp-slime-nav-mode-map)
  (bind-key "C-c C-t" #'elisp-slime-nav-describe-elisp-thing-at-point elisp-slime-nav-mode-map)

  :init
  (add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode))

(use-package lisp-mode ; Bundled with Emacs; contains lisp-mode, emacs-lisp-mode and lisp-interaction-mode.
  :defer t
  :config

  ;; Completion sources.
  (require 'conf/editing/completion) ; Used: my-major-mode-ac-sources.
  (with-eval-after-load 'auto-complete
    (let ((my-elisp-ac-sources '(ac-source-functions ac-source-variables ac-source-symbols ac-source-features)))
      (add-to-list 'my-major-mode-ac-sources `(emacs-lisp-mode . ,my-elisp-ac-sources))
      (add-to-list 'my-major-mode-ac-sources `(lisp-interaction-mode . ,my-elisp-ac-sources))))

  ;; Bindings for evaluating elisp.
  (require 'cl) ; Used: lexical-let.
  (defun evil-eval-region (region-start region-end)
    "Evaluate region and exit Evil's visual state."
    (interactive "r") ; Needs a region.
    (eval-region region-start region-end)
    (evil-exit-visual-state))
  (dolist (keymap (list lisp-interaction-mode-map emacs-lisp-mode-map))
    (lexical-let ((map keymap)) ; This is needed so that the binding is available when the code in `with-eval-after-load' is executed.
      (bind-key "C-c C-e" #'pp-eval-last-sexp map)
      (bind-key "C-c C-i" #'eval-print-last-sexp map) ; Insert value at point.

      (with-eval-after-load 'evil
        (bind-key "C-c C-r" #'evil-eval-region map))
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

  ;; Flycheck: don't check documentation, just code.
  (with-eval-after-load 'flycheck
    (setq-default flycheck-disabled-checkers
                  (cons 'emacs-lisp-checkdoc flycheck-disabled-checkers))))

(provide 'conf/mode-specific/elisp-and-interaction)
