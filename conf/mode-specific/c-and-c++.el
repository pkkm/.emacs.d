;;; C and C++.

(use-package cc-mode ; Bundled with Emacs.
  :defer t
  :config

  ;; Indentation (Smart Tabs).
  (with-eval-after-load 'smart-tabs-mode
    (smart-tabs-insinuate 'c 'c++)
    (add-hook 'c-mode-common-hook #'enable-indent-tabs-mode))

  ;; Don't redefine TAB (so that it can be used for completion).
  (defun my-kill-local-tab-binding ()
    (local-set-key (kbd "TAB") nil))
  (add-hook 'c-mode-common-hook #'my-kill-local-tab-binding)

  ;; Make the default indentation style Linux instead of GNU.
  (setq c-default-style
        '((java-mode . "java")
          (awk-mode . "awk")
          (other . "linux")))

  ;; Toggle between header and implementation with M-o.
  (dolist (keymap (list c-mode-map c++-mode-map))
    (bind-key "M-o" #'ff-find-other-file keymap))

  ;; Unbind C-c . to make space for my binding for Semantic's go-to-definiton.
  (bind-key "C-c ." nil c-mode-base-map)

  ;; Flycheck: use C++11 by default.
  (setq-default flycheck-gcc-language-standard "c++11")
  (setq-default flycheck-clang-language-standard "c++11")

  ;; Default compilation, execution and cleaning commands.
  (require 'conf/utils/lists) ; Used: join-nonempty.
  (require 'conf/driving-processes/compiling) ; Used: my-additional-compile-args, compile-run-clean-command-setter-alist.
  (defun set-c-or-c++-commands ()
    "If we're in C or C++ mode, set the appropriate compilation, running and cleaning commands."
    (interactive)
    (when (and (memq major-mode '(c-mode c++-mode))
               (buffer-file-name)) ; Visiting a file.
      (let* ((input-file-name (file-name-nondirectory (buffer-file-name)))
             (output-file-name (concat (file-name-sans-extension input-file-name)
                                       (if (eq window-system 'w32) ".exe" "")))
             (input-file-name-quoted (shell-quote-argument input-file-name))
             (output-file-name-quoted (shell-quote-argument output-file-name))
             (compiler (if (eq major-mode 'c++-mode) "g++" "gcc"))
             (standard (if (eq major-mode 'c++-mode) "c++11" "c99")))
        ;; Set the variables if they're not already set for this buffer.
        (when (not (local-variable-p 'compile-command))
          (set (make-local-variable 'compile-command)
               (join-nonempty " "
                 compiler input-file-name-quoted "-o" output-file-name-quoted
                 my-additional-compile-args
                 (concat "--std=" standard)
                 "-O2" ; Optimize (may make the compiler perform more static checks, but makes the output of a debugger less clear).
                 "-Wall -Wextra" ; Essential warnings.
                 "-Wstrict-overflow=4" ; Warn on possible signed overflow.
                 "-Werror=implicit-function-declaration" ; Calling an undefined function should be an error.
                 (when (string= compiler "clang")
                   "-fsanitize=undefined-trap -fsanitize-undefined-trap-on-error")))) ; Runtime undefined behavior checks.
        (when (not (local-variable-p 'run-command))
          (set (make-local-variable 'run-command)
               (concat "./" output-file-name-quoted)))
        (when (not (local-variable-p 'clean-command))
          (set (make-local-variable 'clean-command)
               (concat "rm " output-file-name-quoted))))))
  (dolist (mode '(c-mode c++-mode))
    (add-to-list 'compile-run-clean-command-setter-alist (cons mode #'set-c-or-c++-commands))))

(provide 'conf/mode-specific/c-and-c++)
