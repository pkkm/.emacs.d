;;; Elisp and Lisp Interaction mode.

(require 'conf/evil)
(require 'conf/utils/hooks) ; Used: add-one-shot-hooks.

;;; Elisp evaluation bindings.

(add-one-shot-hooks '(lisp-interaction-mode-hook
                      emacs-lisp-mode-hook)
                    #'add-elisp-keys)

(defun add-elisp-keys ()
  ;; "SPC e": Outside visual state, prefix key for Elisp evaluation. Inside, eval region.
  (evil-define-key 'motion (current-local-map) (kbd "SPC e") #'my-elisp-eval-map)
  (evil-define-key 'visual (current-local-map) (kbd "SPC e") #'evil-eval-region))

(defun evil-eval-region (sel-start sel-end)
  "Evaluate region and exit Evil's visual state."
  (interactive "r") ; Needs a region.
  (eval-region sel-start sel-end)
  (evil-exit-visual-state))

(defvar my-elisp-eval-map (make-sparse-keymap)
  "Keymap for Elisp evaluation.")
(define-prefix-command 'my-elisp-eval-map)

(define-key my-elisp-eval-map (kbd "l") #'pp-eval-last-sexp)
(define-key my-elisp-eval-map (kbd "i") #'eval-print-last-sexp) ; Insert value at point.
(define-key my-elisp-eval-map (kbd "b") #'eval-buffer)
(define-key my-elisp-eval-map (kbd "e") #'pp-eval-expression) ; Prompt for an expression to eval.
(define-key my-elisp-eval-map (kbd "t") #'eval-defun) ; Eval the top-level form containing point (or after point).

;;; Eldoc mode -- show function arguments in minibuffer.
(setq eldoc-idle-delay 0.1)
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(add-hooks '(lisp-interaction-mode-hook
             emacs-lisp-mode-hook)
           #'turn-on-eldoc-mode)

(provide 'conf/lang/elisp-and-interaction)
