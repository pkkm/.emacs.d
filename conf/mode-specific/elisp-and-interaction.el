;;; Emacs Lisp and Lisp Interaction modes.
;; Lisp Interaction mode inherits hooks, etc. from Emacs Lisp mode, but doesn't inherit its keymaps.

(require 'conf/packages)
(require 'conf/evil)


;;; Indentation.

(defun my-elisp-indentation ()
  (setq tab-width 8) ; For reading ancient Lisp code.
  (setq indent-tabs-mode nil))
(add-hook 'emacs-lisp-mode-hook #'my-elisp-indentation)


;;; Completion

;; Add elisp-specific completion sources.
(require 'conf/editing/completion)
(let ((my-elisp-ac-sources '(ac-source-functions ac-source-variables ac-source-symbols ac-source-features)))
  (add-to-list 'my-major-mode-ac-sources `(emacs-lisp-mode . ,my-elisp-ac-sources))
  (add-to-list 'my-major-mode-ac-sources `(lisp-interaction-mode . ,my-elisp-ac-sources)))


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
;; `add-hook' is used instead of `add-one-shot-hook' so that the bindings are available both in Emacs Lisp mode and Lisp Interaction mode. Explanation: `emacs-lisp-mode-hook' is ran in Lisp Interaction mode, but `emacs-lisp-mode-map' is not inherited by this mode. This means that using `add-one-shot-hook' would the cause the bindings to be added only to the mode that runs first.

(defun evil-eval-region (region-start region-end)
  "Evaluate region and exit Evil's visual state."
  (interactive "r") ; Needs a region.
  (eval-region region-start region-end)
  (evil-exit-visual-state))


;;; Eldoc mode -- show function arguments in minibuffer.
;; Included with Emacs.
(setq eldoc-idle-delay 0.1)
(setq eldoc-echo-area-use-multiline-p 'truncate-sym-name-if-fit)
(add-hook 'emacs-lisp-mode-hook #'turn-on-eldoc-mode)


;;; Highlight known symbols.
(package-ensure-installed 'highlight-defined)
(add-hook 'emacs-lisp-mode-hook #'highlight-defined-mode)


;;; SLIME-like navigation in Elisp.
;; `C-c .', `C-c ,' -- go to definition of symbol at point, go back. `C-c t' -- describe thing at point.
(package-ensure-installed 'elisp-slime-nav)
(add-hook 'emacs-lisp-mode-hook #'turn-on-elisp-slime-nav-mode)
(setq elisp-slime-nav-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-c .") #'elisp-slime-nav-find-elisp-thing-at-point)
        (define-key map (kbd "C-c ,") #'pop-tag-mark)
        (define-key map (kbd "C-c C-t") #'elisp-slime-nav-describe-elisp-thing-at-point)
        map))
;; Don't show the mode in the modeline.
(require 'conf/modeline/cleaner-minor-modes)
(add-hook 'elisp-slime-nav-mode-hook #'diminish-elisp-slime-nav-mode)
(defun diminish-elisp-slime-nav-mode ()
  (diminish 'elisp-slime-nav-mode ""))


;;; Delete elc file when saving an el file.
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


(provide 'conf/mode-specific/elisp-and-interaction)
