;;; Python. -*- lexical-binding: t -*-

;; Use the built-in python-mode. See
;; <https://old.reddit.com/r/emacs/comments/sshhdi/difference_between_inbuild_python_and_pythonmode/>
;; for a comparison with other Python modes.
(use-package python
  :config

  ;; Don't add an extra empty line when filling docstrings.
  (setq python-fill-docstring-style 'pep-257-nn))

;; I've switched from Elpy to LSP-only, as Elpy is not actively maintained anymore (as of 2025).

(use-package lsp-pyright
  :ensure t
  :init

  ;; Use basedpyright (a slighly improved Pyright) when available.
  (when (executable-find "basedpyright")
    (setq lsp-pyright-langserver-command "basedpyright")))

(provide 'conf/mode-specific/python)
