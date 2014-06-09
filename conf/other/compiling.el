;;; Driving compilers using Emacs.

;; Variable for additional compiler arguments.
(defvar my-additional-compile-args ""
  "Additional arguments to pass to the compiler.")
(make-variable-buffer-local 'my-additional-compile-args)

;; Define a `run' command to run the current program using `compile'.
;; Use `run-command' instead of `compile-command' and `run-read-command' instead of `compile-read-command'.
(defvar run-command nil
  "Like `compile-command', but used in `my-run'.")
(defvar run-read-command t
  "Like `compile-read-command', but used in `run'.")
(defun run (command)
  "Use `compile' to run COMMAND. The output buffer will be in Comint mode.
Interactively, prompts for the command if the variable `run-read-command' is non-nil; otherwise uses `run-command'. With prefix arg, always prompts."
  (interactive (list (let ((command (eval run-command)))
                       (if (or run-read-command current-prefix-arg)
                           (read-shell-command "Run command: " run-command)
                         command))))
  (unless (equal command (eval run-command))
    (setq run-command command))
  (setq-default compilation-directory default-directory)
  (compilation-start command t))
(defun rerun (&optional edit-command)
  "Re-run the program ran using `run'.
If this is run in a Compilation mode buffer, re-use the arguments from the original use. Otherwise, use `run-command'. The buffer will be in Comint mode by default."
  (interactive "P")
  (let ((default-directory (or compilation-directory default-directory)))
    (apply #'compilation-start (or compilation-arguments `(,(eval run-command) t)))))

;; Define a `clean' command, which reads a command and executes it (`clean-command' by default).
(defvar clean-command nil
  "Command used in `clean'.")
(defvar clean-read-command t
  "Like `compile-read-command', but used in `run'.")
(defun clean (command)
  "Execute `clean-command' using `shell-command'.
Interactively, prompts for the command if the variable `clean-read-command' is non-nil; otherwise uses `clean-command'. With prefix arg, always prompts."
  (interactive (list (let ((command (eval clean-command)))
                       (if (or clean-read-command current-prefix-arg)
                           (read-shell-command "Clean command: " clean-command)
                         command))))
  (unless (equal command (eval clean-command))
    (setq clean-command command))
  (shell-command command))

;; Keybindings.
(global-set-key (kbd "<f5>") #'run) ; Run the program.
(global-set-key (kbd "<f6>") #'previous-error)
(global-set-key (kbd "<f7>") #'next-error)
(global-set-key (kbd "<f8>") #'compile)
(global-set-key (kbd "<f9>") #'clean)

;; Double pressing <f5>, <f8> or <f9> should accept the default command.
(dolist (key (list (kbd "<f5>") (kbd "<f8>") (kbd "<f9>")))
  (define-key minibuffer-local-shell-command-map key (kbd "RET")))

;; Disable scroll margin in compilation buffers (because `next-error' and `previous-error' show which error we're currently at by scrolling to it).
(defun disable-scroll-margin-in-buffer ()
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))
(add-hook 'compilation-mode-hook #'disable-scroll-margin-in-buffer)

;; Old approach to the `run' command: use a `defalias'-like macro to replace `compile-command' with `run-command'.
;;(defmacro pseudo-alias-let (replaced-variable replacement-variable &rest body)
;;  "This macro is intended to have some functionality of `defalias' with `let' semantics.
;;
;;It makes a backup of REPLACED-VARIABLE and binds REPLACED-VARIABLE to the value of REPLACEMENT-VARIABLE, then evals BODY. If BODY changed REPLACED-VARIABLE, the new value is saved to REPLACEMENT-VARIABLE and REPLACED-VARIABLE is restored from the backup."
;;  (declare (indent 2))
;;  (when (not (and (symbolp replaced-variable) (symbolp replacement-variable)))
;;    (error "Both the variable name and binding must be symbols"))
;;  (let ((was-replaced-variable-local-symbol (make-symbol (concat "was-" (symbol-name replaced-variable) "-local")))
;;        (replaced-variable-backup-symbol (make-symbol (concat (symbol-name replaced-variable) "-backup"))))
;;    `(let ((,was-replaced-variable-local-symbol (local-variable-p ',replaced-variable))
;;           (,replaced-variable-backup-symbol ,replaced-variable)
;;           (,replaced-variable ,replacement-variable))
;;       (unwind-protect
;;           ,@body
;;         (setq ,replacement-variable ,replaced-variable)
;;         (if (and (local-variable-p ',replaced-variable)
;;                  (not ,was-replaced-variable-local-symbol))
;;             (kill-local-variable ',replaced-variable)
;;           (setq ,replaced-variable ,replaced-variable-backup-symbol))))))
;;
;;;; Define the command `my-run', that runs the current program using `compile'.
;;;; The variable for the command to use is `run-command' instead of `compile-command'.
;;(defvar run-command nil
;;  "Like `compile-command', but used in `my-run'.")
;;(make-variable-buffer-local 'run-command)
;;(defun my-run ()
;;  "Like `compile' but uses `run-command' instead of `compile-command'."
;;  (interactive)
;;  (pseudo-alias-let compile-command run-command
;;    (let ((current-prefix-arg '(4)))
;;      (call-interactively #'compile))))

(provide 'conf/other/compiling)
