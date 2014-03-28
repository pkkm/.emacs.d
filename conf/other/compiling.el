;;; Driving compilers using Emacs.

(defmacro pseudo-alias-let (replaced-variable replacement-variable &rest body)
  "This macro is intended to Make a backup of REPLACED-VARIABLE and bind REPLACED-VARIABLE to the value of REPLACEMENT-VARIABLE, then eval BODY. After this, restore the old value of REPLACED-VARIABLE."
  (declare (indent 2))
  (when (not (and (symbolp replaced-variable) (symbolp replacement-variable)))
    (error "Both the variable name and binding must be symbols"))
  (let ((was-replaced-variable-local-symbol (make-symbol (concat "was-" (symbol-name replaced-variable) "-local")))
        (replaced-variable-backup-symbol (make-symbol (concat (symbol-name replaced-variable) "-backup"))))
    `(let ((,was-replaced-variable-local-symbol (local-variable-p ',replaced-variable))
           (,replaced-variable-backup-symbol ,replaced-variable)
           (,replaced-variable ,replacement-variable))
       (unwind-protect
           ,@body
         (setq ,replacement-variable ,replaced-variable)
         (if (and (local-variable-p ',replaced-variable)
                  (not ,was-replaced-variable-local-symbol))
             (kill-local-variable ',replaced-variable)
           (setq ,replaced-variable ,replaced-variable-backup-symbol))))))

;; Define the command `my-run', that runs the current program using `compile'.
;; The variable for the command to use is `run-command' instead of `compile-command'.
(defvar run-command nil
  "Like `compile-command', but used in `my-run'.")
(defvar run-read-command t
  "Like `compile-read-command', but used in `my-run'.")
(make-variable-buffer-local 'run-command)
(defun my-run ()
  "Like `compile' but uses `run-command' instead of `compile-command'."
  (interactive)
  (pseudo-alias-let compile-command run-command
                    (let ((current-prefix-arg '(4)))
                      (call-interactively #'compile))))

(defun run (command)
  "Use `compile' to run COMMAND. The output buffer will be in Comint mode.
Interactively, prompts for the command if the variable `run-read-command' is non-nil; otherwise uses `run-command'. With prefix arg, always prompts. "
  (interactive (list (let ((command (eval run-command)))
                       (if (or run-read-command current-prefix-arg)
                           (compilation-read-command command)
                         command))
                     (consp current-prefix-arg)))
  (unless (equal command (eval run-command))
    (setq run-command command))
  (setq-default compilation-directory default-directory)
  (compilation-start command t))
(defun rerun (&optional edit-command)
  "Re-run the program ran using `run'.
If this is run in a Compilation mode buffer, re-use the arguments from the original use. Otherwise, use `run-command'."
  (interactive "P")
  (let ((default-directory (or compilation-directory default-directory)))
    (apply #'compilation-start (or compilation-arguments
                                   `(,(eval compile-command))))))

;; Keybindings.
(global-set-key (kbd "<f5>") #'my-run) ; Run the program.
(global-set-key (kbd "<f6>") #'previous-error)
(global-set-key (kbd "<f7>") #'next-error)
(global-set-key (kbd "<f8>") #'compile)

;; Disable scroll margin in compilation buffers (because `next-error' and `previous-error' show which error we're currently at by scrolling to it).
(defun disable-scroll-margin-in-buffer ()
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))
(add-hook 'compilation-mode-hook #'disable-scroll-margin-in-buffer)

(provide 'conf/other/compiling)
