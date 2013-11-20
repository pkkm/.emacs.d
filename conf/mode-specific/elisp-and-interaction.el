;;; Emacs Lisp and Lisp Interaction modes.
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

(require 'conf/evil)
(require 'conf/utils/hooks) ; Used: add-one-shot-hooks.

;;; Elisp evaluation bindings.

(defun my-elisp-bindings ()
  "Add some C-c bindings useful in Emacs Lisp and Lisp Interaction modes."
  (local-set-key (kbd "C-c C-e") #'pp-eval-last-sexp)
  (local-set-key (kbd "C-c C-i") #'eval-print-last-sexp) ; Insert value at point.

  (local-set-key (kbd "C-c C-r") #'evil-eval-region)
  (local-set-key (kbd "C-c C-b") #'eval-buffer)
  (local-set-key (kbd "C-c C-d") #'eval-defun) ; Eval the top-level form containing point (or after point).

  (local-set-key (kbd "C-c C-p") #'pp-eval-expression)) ; Prompt for an expression to eval.

(add-hook 'emacs-lisp-mode-hook #'my-elisp-bindings)
;; `add-hook' is used instead of `add-one-shot-hooks' because `emacs-lisp-mode-hook' is ran in Lisp Interaction mode, but `emacs-lisp-mode-map' is not inherited by this mode.

(defun evil-eval-region (region-start region-end)
  "Evaluate region and exit Evil's visual state."
  (interactive "r") ; Needs a region.
  (eval-region region-start region-end)
  (evil-exit-visual-state))

;;; Eldoc mode -- show function arguments in minibuffer.
(setq eldoc-idle-delay 0.1)
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)

(provide 'conf/mode-specific/elisp-and-interaction)
