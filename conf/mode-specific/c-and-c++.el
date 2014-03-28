;;; C and C++.

;; Indentation (Smart Tabs).
(require 'conf/editing/indentation)
(smart-tabs-insinuate 'c 'c++)
(add-hook 'c-mode-common-hook #'enable-indent-tabs-mode)

;; Make the default indentation style Linux instead of GNU.
;; This variable is defined after c-mode is loaded, but it needs to be set before c-mode's hooks run. Because of this, it'd be hard to modify it (instead of setting it) in the init file.
(setq c-default-style
      '((java-mode . "java")
        (awk-mode . "awk")
        (other . "linux")))

;; Default compilation and execution commands.
(require 'conf/other/compiling)
(defun set-c-compile-run-commands ()
  (interactive)
  (let* ((input-file-name (file-name-nondirectory (buffer-file-name)))
         (output-file-name (concat (file-name-sans-extension input-file-name)
                                   (if (eq window-system 'w32) ".exe" ""))))
    (setq compile-command (concat "gcc -O2 -Wall --std=c99"
                                  " " (shell-quote-argument input-file-name)
                                  " -o " (shell-quote-argument output-file-name)))
    (setq run-command (concat "./" (shell-quote-argument output-file-name)))))
(add-hook 'c-mode-hook #'set-c-compile-run-commands)

(provide 'conf/mode-specific/c-and-c++)
