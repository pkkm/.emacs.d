;;; Driving compilers using Emacs.

;; Use `smart-compile' to select the compilation command.
;; To define a command for a mode or extension, add an appropriate entry to `smart-compile-alist' (see the docstring).
(require 'conf/packages)
(package-ensure-installed 'smart-compile)
(require 'smart-compile) ; So that `smart-compile-alist' is defined.

(defvar my-compile-run-command nil
  "Like `compile-command', but used in `my-smart-compile-run'.")
(make-variable-buffer-local 'my-compile-run-command)
(defvar my-smart-compile-run-alist nil
  "Like `smart-compile-alist', but will be used instead of it when calling `my-smart-compile-run'.")
(defun my-smart-compile-run (&optional arg)
  "Like `smart-compile', but uses `my-smart-compile-run-alist' instead of `smart-compile-alist', and `my-compile-run-command' instead of `compile-command'.
ARG is passed to `smart-compile'."
  (interactive "p")
  (let ((smart-compile-alist my-smart-compile-run-alist)
        (was-compile-command-local (local-variable-p 'compile-command))
        (compile-command-backup compile-command)
        (compile-command my-compile-run-command))
    (smart-compile arg)

    ;; Save the new `compile-command', and restore the old value.
    (setq my-compile-run-command compile-command)
    (when (local-variable-p 'compile-command)
      (if was-compile-command-local
          (setq compile-command compile-command-backup)
        (kill-local-variable 'compile-command)))))

;; TODO figure out a way to `let' a variable to another so that assignments propagate to the binding.
;; symbol-macrolet?
(setq xxx "FIRST")
(cl-letf (((symbol-value 'compile-command) xxx))
  (setq compile-command "SECOND"))
(message (concat xxx "|" compile-command))

;; Keybindings.
(global-set-key (kbd "<f5>") #'my-smart-compile-run) ; Run the program.
(global-set-key (kbd "<f6>") #'previous-error)
(global-set-key (kbd "<f7>") #'next-error)
(global-set-key (kbd "<f8>") #'smart-compile) ; Compile, selecting the command automatically.

;; Disable scroll margin in compilation buffers (because `next-error' and `previous-error' show which error we're currently at by scrolling to it).
(defun disable-scroll-margin-in-buffer ()
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))
(add-hook 'compilation-mode-hook #'disable-scroll-margin-in-buffer)

(provide 'conf/other/compiling)
