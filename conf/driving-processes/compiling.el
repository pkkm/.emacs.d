;;; Driving compilers using Emacs. -*- lexical-binding: t -*-


;;; Setting default compilation, execution and cleaning commands for major modes.

(defvar compile-run-clean-command-setter-alist '()
  "Alist of functions setting the compilation, running and cleaning commands in various major modes.
Each element has the form (MODE . FUNCTION). When file-local variables are put into effect in a buffer with major mode MODE, FUNCTION will be executed.
Mode hooks can't be used for this purpose, since they run before file-local variables are set (by `normal-mode').")

;; When determining the compilation command to use, we usually check a file-local variable, but file-local variables are put into effect after the major mode is set (by `normal-mode'). Therefore, we can't just set the commands in the mode's hook. Instead, we'll set them after file-local variables are put into effect (by `hack-local-variables').
(defun set-compile-run-clean-commands-for-mode ()
  "Set compiling, running and cleaning commands for the current major mode by executing the appropriate function from `compile-run-clean-command-setter-alist'."
  (interactive)
  (let ((function-to-run
         (cdr (assoc major-mode compile-run-clean-command-setter-alist))))
    (when function-to-run
      (funcall function-to-run))))
(add-hook 'hack-local-variables-hook #'set-compile-run-clean-commands-for-mode)

;; Variable for additional compiler arguments (for use in compile command setters of major modes).
(defvar my-additional-compile-args ""
  "Additional arguments to pass to the compiler.")
(make-variable-buffer-local 'my-additional-compile-args)


;; Define a `run' command to run the current program using `compile'.
;; Use `run-command' instead of `compile-command' and `run-read-command' instead of `compile-read-command'.
(defvar run-command ""
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
(defvar clean-command ""
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
(bind-key "<f5>" #'run) ; Run the program.
(bind-key "<f6>" #'previous-error)
(bind-key "<f7>" #'next-error)
(bind-key "<f8>" #'compile)
(bind-key "<f9>" #'clean)

;; Double pressing <f5>, <f8> or <f9> should accept the default command.
(dolist (key (list (kbd "<f5>") (kbd "<f8>") (kbd "<f9>")))
  (bind-key key (kbd "RET") minibuffer-local-shell-command-map))

;; Keybindings for repeating compilation/running.
;; TODO unify with the above bindings using something like "Compile Do What I Mean" from <www.emacswiki.org/emacs/CompileCommand>.
(with-eval-after-load 'evil
  (bind-key "g c" #'recompile evil-motion-state-map)
  (bind-key "g r" #'rerun evil-motion-state-map))

;; Disable scroll margin in compilation buffers (because `next-error' and `previous-error' show which error we're currently at by scrolling to it).
(defun disable-scroll-margin-in-buffer ()
  (interactive)
  (set (make-local-variable 'scroll-margin) 0))
(add-hook 'compilation-mode-hook #'disable-scroll-margin-in-buffer)

(provide 'conf/driving-processes/compiling)
