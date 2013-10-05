;;; Elisp and Lisp Interaction mode.

(require 'conf/evil)
(require 'conf/utils/hooks) ; Used: add-one-shot-hooks.

;;; Elisp evaluation bindings.

(add-one-shot-hooks '(lisp-interaction-mode-hook emacs-lisp-mode-hook)
                    #'add-elisp-keys)

(defun add-elisp-keys ()
  (local-set-key (kbd "C-c C-e") #'pp-eval-last-sexp)
  (local-set-key (kbd "C-c C-i") #'eval-print-last-sexp) ; Insert value at point.

  (local-set-key (kbd "C-c C-r") #'evil-eval-region)
  (local-set-key (kbd "C-c C-b") #'eval-buffer)
  (local-set-key (kbd "C-c C-d") #'eval-defun) ; Eval the top-level form containing point (or after point).

  (local-set-key (kbd "C-c C-p") #'pp-eval-expression)) ; Prompt for an expression to eval.

(defun evil-eval-region (sel-start sel-end)
  "Evaluate region and exit Evil's visual state."
  (interactive "r") ; Needs a region.
  (eval-region sel-start sel-end)
  (evil-exit-visual-state))

;;; Eldoc mode -- show function arguments in minibuffer.
(setq eldoc-idle-delay 0.1)
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(add-hooks '(lisp-interaction-mode-hook
             emacs-lisp-mode-hook)
           #'turn-on-eldoc-mode)

(provide 'conf/lang/elisp-and-interaction)
