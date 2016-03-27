;;; Python.

(use-package elpy
  :ensure t
  :init
  (with-eval-after-load 'python
    (elpy-enable))
  :config

  (setq elpy-rpc-python-command "python3")
  (setq elpy-rpc-backend "jedi") ; Better completion than Rope (as of 2015-05).

  ;; Disable some modules.
  (let ((disabled-modules '(elpy-module-flymake elpy-module-highlight-indentation)))
    (setq elpy-modules (-difference elpy-modules disabled-modules))))

(provide 'conf/mode-specific/python)
