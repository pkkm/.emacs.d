;;; C and C++.

;; Indentation (Smart Tabs).
(require 'conf/editing/indentation)
(smart-tabs-insinuate 'c 'c++)
(add-hook 'c-mode-common-hook #'enable-indent-tabs-mode)

;; Don't redefine TAB (so that it can be used for completion).
(defun my-kill-local-tab-binding ()
  (local-set-key (kbd "TAB") nil))
(add-hook 'c-mode-common-hook #'my-kill-local-tab-binding)

;; Make the default indentation style Linux instead of GNU.
;; This variable is defined after c-mode is loaded, but it needs to be set before c-mode's hooks run. Because of this, it'd be hard to modify it (instead of setting it) in the init file.
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

;; Default compilation, execution and cleaning commands.
(require 'conf/other/compiling)
(defun set-c-compile-run-clean-commands ()
  (interactive)
  (let* ((input-file-name (file-name-nondirectory (buffer-file-name)))
         (output-file-name (concat (file-name-sans-extension input-file-name)
                                   (if (eq window-system 'w32) ".exe" "")))
         (input-file-name-quoted (shell-quote-argument input-file-name))
         (output-file-name-quoted (shell-quote-argument output-file-name))
         (compiler "gcc"))
    (set (make-local-variable 'compile-command)
         (concat compiler " " input-file-name-quoted " -o " output-file-name-quoted
                 (if (string= my-additional-compile-args "") "" " ")
                 my-additional-compile-args ; Defined in 'conf/other/compiling. To be used as a file-local variable.
                 " --std=c99 -O2" ; Compile using the C99 standard and optimize.
                 " -Wall -Wextra" ; Essential warnings.
                 " -Werror=implicit-function-declaration" ; Calling an undefined function should be an error.
                 " -Wstrict-overflow=5" ; Warn on possible signed overflow.
                 " -ftrapv" ; Add runtime checks for undefined behavior (hurts performance).
                 (if (string= compiler "clang") " -fsanitize=undefined-trap -fsanitize-undefined-trap-on-error" ""))) ; Clang-specific runtime undefined behavior checks.
    (set (make-local-variable 'run-command) (concat "./" output-file-name-quoted))
    (set (make-local-variable 'clean-command) (concat "rm " output-file-name-quoted))))
(add-hook 'c-mode-hook #'set-c-compile-run-clean-commands)

(provide 'conf/mode-specific/c-and-c++)